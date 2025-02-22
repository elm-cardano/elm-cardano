module Cardano.Uplc exposing
    ( evalScriptsCosts, evalScriptsCostsRaw
    , applyParamsToScript
    , VmConfig, defaultVmConfig, conwayDefaultBudget, conwayDefaultCostModels, conwayNewDefaultCostModels
    , SlotConfig, slotConfigMainnet, slotConfigPreview, slotConfigPreprod
    )

{-| Handling the UPLC VM

@docs evalScriptsCosts, evalScriptsCostsRaw

@docs applyParamsToScript

@docs VmConfig, defaultVmConfig, conwayDefaultBudget, conwayDefaultCostModels, conwayNewDefaultCostModels

@docs SlotConfig, slotConfigMainnet, slotConfigPreview, slotConfigPreprod

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address exposing (CredentialHash)
import Cardano.Data as Data exposing (Data)
import Cardano.Gov as Gov exposing (CostModels)
import Cardano.Redeemer as Redeemer exposing (ExUnits, Redeemer)
import Cardano.Script as Script exposing (PlutusScript, Script(..))
import Cardano.Transaction as Transaction exposing (Transaction)
import Cardano.Utxo as Utxo exposing (Output)
import Cbor.Decode as CD
import Cbor.Encode as CE
import Dict.Any
import Json.Encode as JE
import Natural exposing (Natural)


{-| Evaluate Plutus scripts costs.

This also checks that the provided local state has all relevant UTxOs present.

This function will call Aiken UPLC VM with some JavaScript and WebAssembly code.
It requires customized Elm compilation and JS code patching,
so you need to call the `elm-cardano` binary for compilation.
More info on that in the `README` of the [elm-cardano GitHub repo](https://github.com/elm-cardano/elm-cardano).

-}
evalScriptsCosts : VmConfig -> Utxo.RefDict Output -> Transaction -> Result String (List Redeemer)
evalScriptsCosts vmConfig localStateUtxos tx =
    let
        usedUtxos : Utxo.RefDict (Maybe Output)
        usedUtxos =
            Transaction.allInputs tx
                |> Dict.Any.foldl
                    (\ref _ acc ->
                        Dict.Any.insert ref (Dict.Any.get ref localStateUtxos) acc
                    )
                    Utxo.emptyRefDict

        missingUtxos =
            Dict.Any.filter (\_ o -> o == Nothing) usedUtxos
                |> Dict.Any.keys
    in
    if not (List.isEmpty missingUtxos) then
        String.join ", " (List.map Utxo.refAsString missingUtxos)
            |> (\missing -> "Missing UTxOs in local state: " ++ missing)
            |> Err

    else
        evalScriptsCostsRaw
            vmConfig
            (Dict.Any.filterMap (\_ -> identity) usedUtxos)
            (Transaction.serialize tx)


{-| Evaluate plutus scripts costs with the Tx raw bytes.

This function will call Aiken UPLC VM with some JavaScript and WebAssembly code.
It requires customized Elm compilation and JS code patching,
so you need to call the `elm-cardano` binary for compilation.
More info on that in the `README` of the [elm-cardano GitHub repo](https://github.com/elm-cardano/elm-cardano).

-}
evalScriptsCostsRaw : VmConfig -> Utxo.RefDict Output -> Bytes any -> Result String (List Redeemer)
evalScriptsCostsRaw vmConfig usedUtxos txBytes =
    let
        ( refs, outputs ) =
            ( Dict.Any.keys usedUtxos, Dict.Any.values usedUtxos )

        jsArguments =
            JE.object
                [ ( "tx_bytes", JE.string <| Bytes.toHex txBytes )
                , ( "utxos_refs_bytes", JE.list (jsEncodeHelper Utxo.encodeOutputReference) refs )
                , ( "utxos_outputs_bytes", JE.list (jsEncodeHelper Utxo.encodeOutput) outputs )
                , ( "cost_mdls_bytes", jsEncodeHelper Gov.encodeCostModels vmConfig.costModels )
                , ( "cpu_budget", JE.int vmConfig.budget.steps )
                , ( "mem_budget", JE.int vmConfig.budget.mem )
                , ( "slot_config_zero_time", JE.int (Natural.toInt vmConfig.slotConfig.zeroTime) )
                , ( "slot_config_zero_slot", JE.int (Natural.toInt vmConfig.slotConfig.zeroSlot) )
                , ( "slot_config_slot_length", JE.int vmConfig.slotConfig.slotLengthMs )
                ]

        decodeRedeemer : String -> Maybe Redeemer
        decodeRedeemer redeemerHex =
            -- Each redeemer is provided in CBOR, in a hex-encoded string
            -- Convert the hex strings to bytes
            Bytes.fromHexUnchecked redeemerHex
                |> Bytes.toBytes
                -- Decode the bytes into redeemers
                |> CD.decode Redeemer.fromCborArray
    in
    evalScriptsCostsKernel jsArguments
        |> Result.map (List.filterMap decodeRedeemer)


{-| Apply a list of [Data] encode parameters to a given [PlutusScript].

Similar to [evalScriptsCostsRaw], This function will call Aiken UPLC VM with
some JavaScript and WebAssembly code.
It requires customized Elm compilation and JS code patching,
so you need to call the `elm-cardano` binary for compilation.
More info on that in the `README` of the [elm-cardano GitHub repo](https://github.com/elm-cardano/elm-cardano).

-}
applyParamsToScript : List Data -> PlutusScript -> Result String { plutusScript : PlutusScript, hash : Bytes CredentialHash }
applyParamsToScript params script =
    let
        jsArguments =
            JE.object
                [ ( "params", jsEncodeHelper (CE.list Data.toCbor) params )
                , ( "script", JE.string <| Bytes.toHex script.script )
                ]

        decodeAppliedScript : String -> Maybe { plutusScript : PlutusScript, hash : Bytes CredentialHash }
        decodeAppliedScript appliedScriptHex =
            let
                header =
                    String.slice 0 2 appliedScriptHex

                flat =
                    if header == "58" then
                        String.dropLeft 4 appliedScriptHex

                    else
                        String.dropLeft 6 appliedScriptHex

                mScriptHash =
                    Bytes.fromHex flat |> Maybe.map (\flatBytes -> Script.hash <| Plutus { script | script = flatBytes })
            in
            Maybe.map2
                (\scriptBytes scriptHash -> { plutusScript = { script | script = scriptBytes }, hash = scriptHash })
                (Bytes.fromHex appliedScriptHex)
                mScriptHash
    in
    applyParamsToScriptKernel jsArguments
        |> Result.andThen (decodeAppliedScript >> Result.fromMaybe "Failed to decode the applied script.")


{-| Kernel function (needs patching by elm-cardano) to run phase 2 evaluation (WASM code).
-}
evalScriptsCostsKernel : JE.Value -> Result String (List String)
evalScriptsCostsKernel _ =
    Err "To build a Tx containing scripts, you need to use the elm-cardano binary instead of directly the elm binary. Details are in the elm-cardano GitHub repo."


{-| Kernel function (needs patching by elm-cardano) to run parameter application (WASM code).
-}
applyParamsToScriptKernel : JE.Value -> Result String String
applyParamsToScriptKernel _ =
    Err "To apply parameters to a script, you need to use the elm-cardano binary instead of directly the elm binary. Details are in the elm-cardano GitHub repo."


{-| UPLC VM configuration.

This is required so that the VM knows how to price memory usage and execution steps,
as well as how to manage time.

-}
type alias VmConfig =
    { budget : ExUnits
    , slotConfig : SlotConfig
    , costModels : CostModels
    }


jsEncodeHelper : (a -> CE.Encoder) -> a -> JE.Value
jsEncodeHelper cborEncoder v =
    CE.encode (cborEncoder v)
        |> Bytes.fromBytes
        |> Bytes.toHex
        |> JE.string


{-| Default UPLC VM config.
-}
defaultVmConfig : VmConfig
defaultVmConfig =
    { budget = conwayDefaultBudget
    , slotConfig = slotConfigMainnet
    , costModels = conwayDefaultCostModels
    }


{-| The default budget currently in the Conway era.
-}
conwayDefaultBudget : ExUnits
conwayDefaultBudget =
    { mem = 14000000, steps = 10000000000 }


{-| Time managing config for the VM.
-}
type alias SlotConfig =
    { zeroTime : Natural
    , zeroSlot : Natural
    , slotLengthMs : Int
    }


{-| Default slot config for Mainnet.
-}
slotConfigMainnet : SlotConfig
slotConfigMainnet =
    -- Found in Blaze codebase
    { zeroTime = Natural.fromSafeInt 1596059091000
    , zeroSlot = Natural.fromSafeInt 4492800
    , slotLengthMs = 1000
    }


{-| Default slot config for Preview.
-}
slotConfigPreview : SlotConfig
slotConfigPreview =
    -- Found in Blaze codebase
    { zeroTime = Natural.fromSafeInt 1666656000000
    , zeroSlot = Natural.fromSafeInt 0
    , slotLengthMs = 1000
    }


{-| Default slot config for Preprod.
-}
slotConfigPreprod : SlotConfig
slotConfigPreprod =
    -- Found in Blaze codebase
    { zeroTime = Natural.fromSafeInt (1654041600000 + 1728000000)
    , zeroSlot = Natural.fromSafeInt 86400
    , slotLengthMs = 1000
    }


{-| Default cost models for the Plutus VM currently in the Conway era.
-}
conwayDefaultCostModels : CostModels
conwayDefaultCostModels =
    -- Retrieved from CardanoScan on 2024-09-06
    -- https://cardanoscan.io/protocolparams
    { plutusV1 = Just [ 100788, 420, 1, 1, 1000, 173, 0, 1, 1000, 59957, 4, 1, 11183, 32, 201305, 8356, 4, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 100, 100, 16000, 100, 94375, 32, 132994, 32, 61462, 4, 72010, 178, 0, 1, 22151, 32, 91189, 769, 4, 2, 85848, 228465, 122, 0, 1, 1, 1000, 42921, 4, 2, 24548, 29498, 38, 1, 898148, 27279, 1, 51775, 558, 1, 39184, 1000, 60594, 1, 141895, 32, 83150, 32, 15299, 32, 76049, 1, 13169, 4, 22100, 10, 28999, 74, 1, 28999, 74, 1, 43285, 552, 1, 44749, 541, 1, 33852, 32, 68246, 32, 72362, 32, 7243, 32, 7391, 32, 11546, 32, 85848, 228465, 122, 0, 1, 1, 90434, 519, 0, 1, 74433, 32, 85848, 228465, 122, 0, 1, 1, 85848, 228465, 122, 0, 1, 1, 270652, 22588, 4, 1457325, 64566, 4, 20467, 1, 4, 0, 141992, 32, 100788, 420, 1, 1, 81663, 32, 59498, 32, 20142, 32, 24588, 32, 20744, 32, 25933, 32, 24623, 32, 53384111, 14333, 10 ]
    , plutusV2 = Just [ 100788, 420, 1, 1, 1000, 173, 0, 1, 1000, 59957, 4, 1, 11183, 32, 201305, 8356, 4, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 100, 100, 16000, 100, 94375, 32, 132994, 32, 61462, 4, 72010, 178, 0, 1, 22151, 32, 91189, 769, 4, 2, 85848, 228465, 122, 0, 1, 1, 1000, 42921, 4, 2, 24548, 29498, 38, 1, 898148, 27279, 1, 51775, 558, 1, 39184, 1000, 60594, 1, 141895, 32, 83150, 32, 15299, 32, 76049, 1, 13169, 4, 22100, 10, 28999, 74, 1, 28999, 74, 1, 43285, 552, 1, 44749, 541, 1, 33852, 32, 68246, 32, 72362, 32, 7243, 32, 7391, 32, 11546, 32, 85848, 228465, 122, 0, 1, 1, 90434, 519, 0, 1, 74433, 32, 85848, 228465, 122, 0, 1, 1, 85848, 228465, 122, 0, 1, 1, 955506, 213312, 0, 2, 270652, 22588, 4, 1457325, 64566, 4, 20467, 1, 4, 0, 141992, 32, 100788, 420, 1, 1, 81663, 32, 59498, 32, 20142, 32, 24588, 32, 20744, 32, 25933, 32, 24623, 32, 43053543, 10, 53384111, 14333, 10, 43574283, 26308, 10 ]
    , plutusV3 = Just [ 100788, 420, 1, 1, 1000, 173, 0, 1, 1000, 59957, 4, 1, 11183, 32, 201305, 8356, 4, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 100, 100, 16000, 100, 94375, 32, 132994, 32, 61462, 4, 72010, 178, 0, 1, 22151, 32, 91189, 769, 4, 2, 85848, 123203, 7305, -900, 1716, 549, 57, 85848, 0, 1, 1, 1000, 42921, 4, 2, 24548, 29498, 38, 1, 898148, 27279, 1, 51775, 558, 1, 39184, 1000, 60594, 1, 141895, 32, 83150, 32, 15299, 32, 76049, 1, 13169, 4, 22100, 10, 28999, 74, 1, 28999, 74, 1, 43285, 552, 1, 44749, 541, 1, 33852, 32, 68246, 32, 72362, 32, 7243, 32, 7391, 32, 11546, 32, 85848, 123203, 7305, -900, 1716, 549, 57, 85848, 0, 1, 90434, 519, 0, 1, 74433, 32, 85848, 123203, 7305, -900, 1716, 549, 57, 85848, 0, 1, 1, 85848, 123203, 7305, -900, 1716, 549, 57, 85848, 0, 1, 955506, 213312, 0, 2, 270652, 22588, 4, 1457325, 64566, 4, 20467, 1, 4, 0, 141992, 32, 100788, 420, 1, 1, 81663, 32, 59498, 32, 20142, 32, 24588, 32, 20744, 32, 25933, 32, 24623, 32, 43053543, 10, 53384111, 14333, 10, 43574283, 26308, 10, 16000, 100, 16000, 100, 962335, 18, 2780678, 6, 442008, 1, 52538055, 3756, 18, 267929, 18, 76433006, 8868, 18, 52948122, 18, 1995836, 36, 3227919, 12, 901022, 1, 166917843, 4307, 36, 284546, 36, 158221314, 26549, 36, 74698472, 36, 333849714, 1, 254006273, 72, 2174038, 72, 2261318, 64571, 4, 207616, 8310, 4, 1293828, 28716, 63, 0, 1, 1006041, 43623, 251, 0, 1 ]
    }


{-| Default cost models for the Plutus VM currently in the Conway era.
-}
conwayNewDefaultCostModels : CostModels
conwayNewDefaultCostModels =
    -- Retrieved from Ogmios on 2024-11-01:
    --  curl -X POST "https://preview.koios.rest/api/v1/ogmios" \
    --    -H "accept: application/json"\
    --    -H "content-type: application/json" \
    --    -d '{"jsonrpc":"2.0","method":"queryLedgerState/protocolParameters"}' | jq . > params.json
    { plutusV1 = Just [ 100788, 420, 1, 1, 1000, 173, 0, 1, 1000, 59957, 4, 1, 11183, 32, 201305, 8356, 4, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 100, 100, 16000, 100, 94375, 32, 132994, 32, 61462, 4, 72010, 178, 0, 1, 22151, 32, 91189, 769, 4, 2, 85848, 228465, 122, 0, 1, 1, 1000, 42921, 4, 2, 24548, 29498, 38, 1, 898148, 27279, 1, 51775, 558, 1, 39184, 1000, 60594, 1, 141895, 32, 83150, 32, 15299, 32, 76049, 1, 13169, 4, 22100, 10, 28999, 74, 1, 28999, 74, 1, 43285, 552, 1, 44749, 541, 1, 33852, 32, 68246, 32, 72362, 32, 7243, 32, 7391, 32, 11546, 32, 85848, 228465, 122, 0, 1, 1, 90434, 519, 0, 1, 74433, 32, 85848, 228465, 122, 0, 1, 1, 85848, 228465, 122, 0, 1, 1, 270652, 22588, 4, 1457325, 64566, 4, 20467, 1, 4, 0, 141992, 32, 100788, 420, 1, 1, 81663, 32, 59498, 32, 20142, 32, 24588, 32, 20744, 32, 25933, 32, 24623, 32, 53384111, 14333, 10 ]
    , plutusV2 = Just [ 100788, 420, 1, 1, 1000, 173, 0, 1, 1000, 59957, 4, 1, 11183, 32, 201305, 8356, 4, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 100, 100, 16000, 100, 94375, 32, 132994, 32, 61462, 4, 72010, 178, 0, 1, 22151, 32, 91189, 769, 4, 2, 85848, 228465, 122, 0, 1, 1, 1000, 42921, 4, 2, 24548, 29498, 38, 1, 898148, 27279, 1, 51775, 558, 1, 39184, 1000, 60594, 1, 141895, 32, 83150, 32, 15299, 32, 76049, 1, 13169, 4, 22100, 10, 28999, 74, 1, 28999, 74, 1, 43285, 552, 1, 44749, 541, 1, 33852, 32, 68246, 32, 72362, 32, 7243, 32, 7391, 32, 11546, 32, 85848, 228465, 122, 0, 1, 1, 90434, 519, 0, 1, 74433, 32, 85848, 228465, 122, 0, 1, 1, 85848, 228465, 122, 0, 1, 1, 955506, 213312, 0, 2, 270652, 22588, 4, 1457325, 64566, 4, 20467, 1, 4, 0, 141992, 32, 100788, 420, 1, 1, 81663, 32, 59498, 32, 20142, 32, 24588, 32, 20744, 32, 25933, 32, 24623, 32, 43053543, 10, 53384111, 14333, 10, 43574283, 26308, 10 ]
    , plutusV3 = Just [ 100788, 420, 1, 1, 1000, 173, 0, 1, 1000, 59957, 4, 1, 11183, 32, 201305, 8356, 4, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 100, 100, 16000, 100, 94375, 32, 132994, 32, 61462, 4, 72010, 178, 0, 1, 22151, 32, 91189, 769, 4, 2, 85848, 123203, 7305, -900, 1716, 549, 57, 85848, 0, 1, 1, 1000, 42921, 4, 2, 24548, 29498, 38, 1, 898148, 27279, 1, 51775, 558, 1, 39184, 1000, 60594, 1, 141895, 32, 83150, 32, 15299, 32, 76049, 1, 13169, 4, 22100, 10, 28999, 74, 1, 28999, 74, 1, 43285, 552, 1, 44749, 541, 1, 33852, 32, 68246, 32, 72362, 32, 7243, 32, 7391, 32, 11546, 32, 85848, 123203, 7305, -900, 1716, 549, 57, 85848, 0, 1, 90434, 519, 0, 1, 74433, 32, 85848, 123203, 7305, -900, 1716, 549, 57, 85848, 0, 1, 1, 85848, 123203, 7305, -900, 1716, 549, 57, 85848, 0, 1, 955506, 213312, 0, 2, 270652, 22588, 4, 1457325, 64566, 4, 20467, 1, 4, 0, 141992, 32, 100788, 420, 1, 1, 81663, 32, 59498, 32, 20142, 32, 24588, 32, 20744, 32, 25933, 32, 24623, 32, 43053543, 10, 53384111, 14333, 10, 43574283, 26308, 10, 16000, 100, 16000, 100, 962335, 18, 2780678, 6, 442008, 1, 52538055, 3756, 18, 267929, 18, 76433006, 8868, 18, 52948122, 18, 1995836, 36, 3227919, 12, 901022, 1, 166917843, 4307, 36, 284546, 36, 158221314, 26549, 36, 74698472, 36, 333849714, 1, 254006273, 72, 2174038, 72, 2261318, 64571, 4, 207616, 8310, 4, 1293828, 28716, 63, 0, 1, 1006041, 43623, 251, 0, 1, 100181, 726, 719, 0, 1, 100181, 726, 719, 0, 1, 100181, 726, 719, 0, 1, 107878, 680, 0, 1, 95336, 1, 281145, 18848, 0, 1, 180194, 159, 1, 1, 158519, 8942, 0, 1, 159378, 8813, 0, 1, 107490, 3298, 1, 106057, 655, 1, 1964219, 24520, 3 ]
    }
