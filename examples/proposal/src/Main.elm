port module Main exposing (main)

{-| Example: submit governance proposals on Preview via a CIP-30 wallet.

Scenario (matches the mock data used to test conflict badges in the main app):

  - Tx A: two competing ParameterChange proposals (TEST #1 and TEST #2),
    both referencing the same `latestEnacted` protocol-param action.
  - Tx B: one ParameterChange proposal (TEST #3) whose `latestEnacted`
    points to TEST #1 — the "Follows #1" case. Must be submitted AFTER
    Tx A is on chain.
  - Tx C: NoConfidence (TEST #4) + UpdateCommittee (TEST #5) sharing the
    committee purpose — competing siblings, both "delaying".

What this example does NOT do:

  - Host / hash the CIP-108 metadata: you host the 5 JSON files from
    `frontend/cip108-dummy/` somewhere (GitHub raw, IPFS, ...), compute
    the blake2b-256 of each file, and fill the `anchor*` constants above.

Submission goes through `Cip30.submitTx` — no Ogmios / Koios needed for
submit. Koios is only used at startup to fetch:

1.  protocol parameters + cost models (for UPLC fee evaluation),
2.  the raw CBOR of the tx that created the guardrails ref UTxO (so we
    can decode it and inject the resulting `Output` into `localStateUtxos`),
3.  the most recently enacted ParameterChange action's `ActionId`,
4.  the most recently enacted committee-purpose action's `ActionId`.

(3) and (4) become the `latestEnacted*` fields of `GovernanceState` so
new proposals correctly chain off the current head of each purpose.

-}

import Browser
import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address as Address exposing (Address, Credential(..), NetworkId(..), StakeAddress, StakeCredential(..))
import Cardano.Cip30 as Cip30
import Cardano.CoinSelection as CoinSelection
import Cardano.Gov as Gov exposing (ActionId, Anchor, CostModels)
import Cardano.MultiAsset exposing (PolicyId)
import Cardano.Script as Script exposing (PlutusVersion(..), ScriptCbor)
import Cardano.Transaction as Transaction exposing (Transaction)
import Cardano.TxIntent as TxIntent exposing (ActionProposal(..), Fee(..), SpendSource(..), TxIntent(..))
import Cardano.Uplc as Uplc
import Cardano.Utxo as Utxo exposing (Output, OutputReference, TransactionId)
import Cardano.Value as Value
import Cardano.Witness as Witness
import Dict
import Dict.Any
import Html exposing (Html, button, div, h2, input, li, p, pre, text, ul)
import Html.Attributes as HA
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD exposing (Decoder, Value)
import Json.Encode as JE
import Natural exposing (Natural)



-- #########################################################
-- CONSTANTS — EDIT ME
-- #########################################################


{-| Free Tier Koios API token.
Expiration date: 2027-02-18.
-}
koiosApiToken : String
koiosApiToken =
    "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJhZGRyIjoic3Rha2UxdXljY3J5MzZwcXB0aGV4cmw5eW4zZDN6azJrbGR3N3lhdG0wM2gwcHU1eXdjMHFqMzYyNzQiLCJleHAiOjE4MDI5NDcwMTIsInRpZXIiOjEsInByb2pJRCI6Ind5Wk1Sb0ZmYnBKdmNuYncifQ.JMJNKGGXo_yDBottzKUB34D1afR6-2j3vtxw70k1Les"


koiosBaseUrl : String
koiosBaseUrl =
    "https://preview.koios.rest/api/v1"


{-| Guardrails script policyId on Preview. Extracted from tx
`c9294b4c56316f88bc5028d2f337a73f3eb73800f88188e930a2f6afad1013b4`.
-}
guardrailsPolicyId : Bytes PolicyId
guardrailsPolicyId =
    Bytes.fromHexUnchecked "fa24fb305126805cf2164c161d852a0e7330cf988f1fe558cf7d4a64"


{-| UTxO holding the guardrails script as a reference script. Used for
`ByReference` wiring once we switch to it.
-}
guardrailsScriptRefUtxo : OutputReference
guardrailsScriptRefUtxo =
    { transactionId = Bytes.fromHexUnchecked "f3f61635034140e6cec495a1c69ce85b22690e65ab9553ef408d524f58183649"
    , outputIndex = 0
    }


{-| Deposit required to submit a governance action (Preview / Conway).
Confirmed from the decoded tx: 100\_000 ADA = 100\_000\_000\_000 lovelace.
-}
govActionDeposit : Natural
govActionDeposit =
    -- 100_000 ADA
    Natural.fromSafeInt 100000
        |> Natural.mul (Natural.fromSafeInt 1000000)



-- CIP-108 metadata anchors -- TODO: fill blake2b-256 hashes


anchor1 : Anchor
anchor1 =
    -- { url = "https://raw.githubusercontent.com/elm-cardano/elm-cardano/fbf1fd54aa8063787b746f218a04ffe25b6a1825/examples/proposal/cip108-dummy/01-pparam-lower-fees.json"
    { url = "ipfs://bafkreicnmufryab62tkwz5rkteofg5imgihvjtx7v4k2t2x3p73fknfxlq"
    , dataHash = Bytes.fromHexUnchecked "e2ad9bb6fb2a7eb73616070f15bf9ccf407689181a4c4cf66c87fb1aeb3f02d6"
    }


anchor2 : Anchor
anchor2 =
    -- { url = "https://raw.githubusercontent.com/elm-cardano/elm-cardano/fbf1fd54aa8063787b746f218a04ffe25b6a1825/examples/proposal/cip108-dummy/02-pparam-raise-fees.json"
    { url = "ipfs://bafkreigyalzkphnamlebjjyciue3cxjtqrcyz2wlonaki6b7t4yrov455y"
    , dataHash = Bytes.fromHexUnchecked "6d45f572e210ad30fa76f951478097f17f0f607b3bb923e8063e4e4574353e1e"
    }


anchor3 : Anchor
anchor3 =
    -- { url = "https://raw.githubusercontent.com/elm-cardano/elm-cardano/fbf1fd54aa8063787b746f218a04ffe25b6a1825/examples/proposal/cip108-dummy/03-pparam-follows.json"
    { url = "ipfs://bafkreibo5sacbu3ecqjiwwmlvsnyljxjgunenvddev54a34bpdtxida6iq"
    , dataHash = Bytes.fromHexUnchecked "5d89005975484a2da01b490da1c8fcb50e0e7a9304eb9760ae88dccae911a7e6"
    }


anchor4 : Anchor
anchor4 =
    -- { url = "https://raw.githubusercontent.com/elm-cardano/elm-cardano/fbf1fd54aa8063787b746f218a04ffe25b6a1825/examples/proposal/cip108-dummy/04-no-confidence.json"
    { url = "ipfs://bafkreic2ol77oa2jtvdpx6poga6ptsyzgux6rvfpkuyfiioztmey6qadl4"
    , dataHash = Bytes.fromHexUnchecked "b22fcb1de25dabdb2a807962d0f84ea0052d2ca5cbdf1ef7a6a7a0033bd33e8f"
    }


anchor5 : Anchor
anchor5 =
    -- { url = "https://raw.githubusercontent.com/elm-cardano/elm-cardano/fbf1fd54aa8063787b746f218a04ffe25b6a1825/examples/proposal/cip108-dummy/05-update-committee.json"
    { url = "ipfs://bafkreidzxqunp3ruoi2rziu3isvd2ral55vwm65ksoa7ij6db6rr4762vy"
    , dataHash = Bytes.fromHexUnchecked "955fb5010f1090ee33bd75b548c61a3e6a54651bee236d6ee44c8a837422994d"
    }



-- Each ParameterChange tx must have a non-trivial update, otherwise the
-- proposal is meaningless / may be rejected. We pick three different
-- trivial field tweaks so the three txs differ from one another.


update1 : Gov.ProtocolParamUpdate
update1 =
    -- Currently 3 on Preview
    { noParamUpdate | minCommitteeSize = Just 5 }


update2 : Gov.ProtocolParamUpdate
update2 =
    { noParamUpdate | minCommitteeSize = Just 6 }


update3 : Gov.ProtocolParamUpdate
update3 =
    { noParamUpdate | minCommitteeSize = Just 7 }


noParamUpdate : Gov.ProtocolParamUpdate
noParamUpdate =
    Gov.noParamUpdate



-- #########################################################
-- PORTS
-- #########################################################


port toWallet : Value -> Cmd msg


port fromWallet : (Value -> msg) -> Sub msg



-- #########################################################
-- MODEL
-- #########################################################


type Model
    = Startup
    | WalletDiscovered (List Cip30.WalletDescriptor)
    | WalletLoading { wallet : Cip30.Wallet, utxos : List Cip30.Utxo }
    | WalletLoaded LoadedWallet
    | LoadingChainData LoadedWallet PartialChainData
    | Ready Context State
    | Submitting Context State { which : WhichTx, tx : Transaction }
    | Fatal String


type alias LoadedWallet =
    { wallet : Cip30.Wallet
    , utxos : Utxo.RefDict Output
    , changeAddress : Address
    , depositReturnAccount : StakeAddress
    }


type alias Context =
    { loadedWallet : LoadedWallet
    , costModels : CostModels
    , lastEnactedParamUpdate : Maybe ActionId
    , lastEnactedCommittee : Maybe ActionId
    , localStateUtxos : Utxo.RefDict Output
    }


{-| Mutable state that evolves as the 3 txs get submitted.
-}
type alias State =
    { txAHash : Maybe (Bytes TransactionId)
    , txBHash : Maybe (Bytes TransactionId)
    , txCHash : Maybe (Bytes TransactionId)
    , errors : String
    }


initialState : State
initialState =
    { txAHash = Nothing, txBHash = Nothing, txCHash = Nothing, errors = "" }


{-| Inner `Maybe` is "is there an enacted action?". Outer `Maybe` is "have
we received the response yet?". So `Nothing` = pending, `Just Nothing` =
loaded but no enacted action exists, `Just (Just id)` = loaded with id.
-}
type alias PartialChainData =
    { costModels : Maybe CostModels
    , guardrailsRefOutput : Maybe Output
    , lastEnactedParamUpdate : Maybe (Maybe ActionId)
    , lastEnactedCommittee : Maybe (Maybe ActionId)
    , errors : String
    }


emptyPartialChainData : PartialChainData
emptyPartialChainData =
    { costModels = Nothing
    , guardrailsRefOutput = Nothing
    , lastEnactedParamUpdate = Nothing
    , lastEnactedCommittee = Nothing
    , errors = ""
    }


type WhichTx
    = TxA
    | TxB
    | TxC


whichTxLabel : WhichTx -> String
whichTxLabel w =
    case w of
        TxA ->
            "Tx A (#1 + #2)"

        TxB ->
            "Tx B (#3 follows #1)"

        TxC ->
            "Tx C (#4 + #5)"


init : () -> ( Model, Cmd Msg )
init _ =
    ( Startup
    , toWallet (Cip30.encodeRequest Cip30.discoverWallets)
    )



-- #########################################################
-- UPDATE
-- #########################################################


type Msg
    = WalletMsg Value
    | ConnectButtonClicked { id : String }
    | LoadChainDataClicked
    | GotCostModels (Result String CostModels)
    | GotGuardrailsRefUtxo (Result String Output)
    | GotLastEnactedParamUpdate (Result String (Maybe ActionId))
    | GotLastEnactedCommittee (Result String (Maybe ActionId))
    | SubmitClicked WhichTx


walletResponseDecoder : Decoder (Cip30.Response Cip30.ApiResponse)
walletResponseDecoder =
    Cip30.responseDecoder (Dict.singleton 30 Cip30.apiDecoder)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( WalletMsg value, _ ) ->
            case ( JD.decodeValue walletResponseDecoder value, model ) of
                ( Err jsonErr, _ ) ->
                    let
                        msgStr =
                            "Wallet JSON decode error: " ++ JD.errorToString jsonErr

                        _ =
                            Debug.log "WalletMsg" msgStr
                    in
                    ( addErrorToModel msgStr model, Cmd.none )

                ( Ok (Cip30.AvailableWallets wallets), Startup ) ->
                    ( WalletDiscovered wallets, Cmd.none )

                ( Ok (Cip30.EnabledWallet wallet), WalletDiscovered _ ) ->
                    ( WalletLoading { wallet = wallet, utxos = [] }
                    , toWallet (Cip30.encodeRequest (Cip30.getUtxos wallet { amount = Nothing, paginate = Nothing }))
                    )

                ( Ok (Cip30.ApiResponse _ (Cip30.WalletUtxos utxos)), WalletLoading { wallet } ) ->
                    ( WalletLoading { wallet = wallet, utxos = utxos }
                    , toWallet (Cip30.encodeRequest (Cip30.getChangeAddress wallet))
                    )

                ( Ok (Cip30.ApiResponse _ (Cip30.ChangeAddress address)), WalletLoading { wallet, utxos } ) ->
                    case makeLoadedWallet wallet utxos address of
                        Ok lw ->
                            ( WalletLoaded lw, Cmd.none )

                        Err err ->
                            ( Fatal err, Cmd.none )

                ( Ok (Cip30.ApiResponse _ (Cip30.SignedTx vkeywitnesses)), Submitting ctx state ({ tx } as sub) ) ->
                    let
                        signedTx =
                            Transaction.updateSignatures
                                (Just << List.append vkeywitnesses << Maybe.withDefault [])
                                tx
                    in
                    ( Submitting ctx state { sub | tx = signedTx }
                    , toWallet (Cip30.encodeRequest (Cip30.submitTx ctx.loadedWallet.wallet signedTx))
                    )

                ( Ok (Cip30.ApiResponse _ (Cip30.SubmittedTx txId)), Submitting ctx state { which, tx } ) ->
                    let
                        -- Bump the local-state utxos so a subsequent tx builds
                        -- against the post-submission state.
                        { updatedState } =
                            TxIntent.updateLocalState txId tx ctx.localStateUtxos

                        newCtx =
                            { ctx | localStateUtxos = updatedState }

                        newState =
                            case which of
                                TxA ->
                                    { state | txAHash = Just txId }

                                TxB ->
                                    { state | txBHash = Just txId }

                                TxC ->
                                    { state | txCHash = Just txId }
                    in
                    ( Ready newCtx newState, Cmd.none )

                ( Ok (Cip30.ApiError err), Submitting ctx state { which } ) ->
                    let
                        msgStr =
                            "Wallet rejected " ++ whichTxLabel which ++ ": " ++ Debug.toString err

                        _ =
                            Debug.log "WalletMsg" msgStr
                    in
                    ( Ready ctx { state | errors = msgStr }, Cmd.none )

                ( Ok (Cip30.ApiError err), _ ) ->
                    let
                        msgStr =
                            "Wallet API error: " ++ Debug.toString err

                        _ =
                            Debug.log "WalletMsg" msgStr
                    in
                    ( addErrorToModel msgStr model, Cmd.none )

                ( Ok (Cip30.UnhandledResponseType t), _ ) ->
                    let
                        msgStr =
                            "Unhandled wallet response type: " ++ t

                        _ =
                            Debug.log "WalletMsg" msgStr
                    in
                    ( addErrorToModel msgStr model, Cmd.none )

                ( Ok other, _ ) ->
                    let
                        _ =
                            Debug.log "WalletMsg unexpected in state"
                                { response = other, state = stateLabel model }
                    in
                    ( model, Cmd.none )

        ( ConnectButtonClicked { id }, WalletDiscovered _ ) ->
            ( model
            , toWallet
                (Cip30.encodeRequest
                    (Cip30.enableWallet { id = id, extensions = [], watchInterval = Nothing })
                )
            )

        ( LoadChainDataClicked, WalletLoaded lw ) ->
            ( LoadingChainData lw emptyPartialChainData, fetchChainData )

        ( GotCostModels res, LoadingChainData lw partial ) ->
            case res of
                Ok cm ->
                    advanceLoading lw { partial | costModels = Just cm }

                Err e ->
                    let
                        _ =
                            Debug.log "fetchProtocolParams" e
                    in
                    advanceLoading lw { partial | errors = appendErr partial.errors ("costModels: " ++ e) }

        ( GotGuardrailsRefUtxo res, LoadingChainData lw partial ) ->
            case res of
                Ok out ->
                    advanceLoading lw { partial | guardrailsRefOutput = Just out }

                Err e ->
                    let
                        _ =
                            Debug.log "fetchGuardrailsRefUtxo" e
                    in
                    advanceLoading lw { partial | errors = appendErr partial.errors ("guardrails ref utxo: " ++ e) }

        ( GotLastEnactedParamUpdate res, LoadingChainData lw partial ) ->
            case res of
                Ok m ->
                    advanceLoading lw { partial | lastEnactedParamUpdate = Just m }

                Err e ->
                    let
                        _ =
                            Debug.log "fetchLastEnactedParamUpdate" e
                    in
                    advanceLoading lw
                        { partial
                            | lastEnactedParamUpdate = Just Nothing
                            , errors = appendErr partial.errors ("lastEnactedParamUpdate: " ++ e)
                        }

        ( GotLastEnactedCommittee res, LoadingChainData lw partial ) ->
            case res of
                Ok m ->
                    advanceLoading lw { partial | lastEnactedCommittee = Just m }

                Err e ->
                    let
                        _ =
                            Debug.log "fetchLastEnactedCommittee" e
                    in
                    advanceLoading lw
                        { partial
                            | lastEnactedCommittee = Just Nothing
                            , errors = appendErr partial.errors ("lastEnactedCommittee: " ++ e)
                        }

        ( SubmitClicked which, Ready ctx state ) ->
            case buildTx which ctx state of
                Ok { tx } ->
                    ( Submitting ctx state { which = which, tx = tx }
                    , toWallet
                        (Cip30.encodeRequest
                            (Cip30.signTx ctx.loadedWallet.wallet { partialSign = True } tx)
                        )
                    )

                Err err ->
                    let
                        msgStr =
                            "Failed to build " ++ whichTxLabel which ++ ": " ++ err

                        _ =
                            Debug.log "buildTx" msgStr
                    in
                    ( Ready ctx { state | errors = msgStr }, Cmd.none )

        _ ->
            ( model, Cmd.none )


{-| Attach an error message to whichever state currently has an `errors`
field, so the user always sees what went wrong. If the state doesn't carry
one, we fall through to `Fatal`.

For `Submitting`, the error is accumulated into the underlying `State` so
it will be visible once we transition back to `Ready` — without dropping
the in-flight sub, so the eventual `SubmittedTx` is still routed correctly.

-}
addErrorToModel : String -> Model -> Model
addErrorToModel err model =
    case model of
        Ready ctx state ->
            Ready ctx { state | errors = appendErr state.errors err }

        Submitting ctx state sub ->
            Submitting ctx { state | errors = appendErr state.errors err } sub

        LoadingChainData lw partial ->
            LoadingChainData lw { partial | errors = appendErr partial.errors err }

        _ ->
            Fatal err


appendErr : String -> String -> String
appendErr existing new =
    if existing == "" then
        new

    else
        existing ++ "\n" ++ new


stateLabel : Model -> String
stateLabel model =
    case model of
        Startup ->
            "Startup"

        WalletDiscovered _ ->
            "WalletDiscovered"

        WalletLoading _ ->
            "WalletLoading"

        WalletLoaded _ ->
            "WalletLoaded"

        LoadingChainData _ _ ->
            "LoadingChainData"

        Ready _ _ ->
            "Ready"

        Submitting _ _ _ ->
            "Submitting"

        Fatal _ ->
            "Fatal"


{-| Called when any chain-data fetch completes. Moves to `Ready` once all
four fetches have produced a result, otherwise stays in `LoadingChainData`.

The guardrails ref-utxo is injected into `localStateUtxos` so the UPLC
evaluator can read its `referenceScript` during fee calculation.

-}
advanceLoading : LoadedWallet -> PartialChainData -> ( Model, Cmd Msg )
advanceLoading lw partial =
    let
        ready =
            Maybe.map4
                (\cm refOut paramAction committeeAction ->
                    Ready
                        { loadedWallet = lw
                        , costModels = cm
                        , lastEnactedParamUpdate = paramAction
                        , lastEnactedCommittee = committeeAction
                        , localStateUtxos =
                            Dict.Any.insert guardrailsScriptRefUtxo refOut lw.utxos
                        }
                        { initialState | errors = partial.errors }
                )
                partial.costModels
                partial.guardrailsRefOutput
                partial.lastEnactedParamUpdate
                partial.lastEnactedCommittee
    in
    ( Maybe.withDefault (LoadingChainData lw partial) ready, Cmd.none )


makeLoadedWallet : Cip30.Wallet -> List Cip30.Utxo -> Address -> Result String LoadedWallet
makeLoadedWallet wallet utxos address =
    case Address.extractStakeCredential address of
        Just (InlineCredential (VKeyHash keyHash)) ->
            Ok
                { wallet = wallet
                , utxos = Utxo.refDictFromList utxos
                , changeAddress = address
                , depositReturnAccount =
                    { networkId = Testnet
                    , stakeCredential = VKeyHash keyHash
                    }
                }

        Just (InlineCredential (ScriptHash _)) ->
            Err "Wallet stake credential is a script — not supported here. Use a regular HD wallet."

        _ ->
            Err "Wallet has no stake credential. The governance deposit must be returned to a stake address."



-- #########################################################
-- TX BUILDING
-- #########################################################


buildTx : WhichTx -> Context -> State -> Result String TxIntent.TxFinalized
buildTx which ctx state =
    let
        returnAcct =
            ctx.loadedWallet.depositReturnAccount

        anchorProposal : Anchor -> ActionProposal -> TxIntent
        anchorProposal a govAction =
            Propose
                { govAction = govAction
                , offchainInfo = a
                , deposit = govActionDeposit
                , depositReturnAccount = returnAcct
                }

        proposalIntents =
            case which of
                TxA ->
                    [ anchorProposal anchor1 (ParameterChange update1)
                    , anchorProposal anchor2 (ParameterChange update2)
                    ]

                TxB ->
                    [ anchorProposal anchor3 (ParameterChange update3) ]

                TxC ->
                    [ anchorProposal anchor4 NoConfidence
                    , anchorProposal anchor5
                        (UpdateCommittee
                            { removeMembers = []
                            , addMembers = []
                            , quorumThreshold = { numerator = 2, denominator = 3 }
                            }
                        )
                    ]

        -- The library accumulates proposal deposits into the required-output
        -- side of `checkBalance`, but does NOT pull funds from the wallet
        -- automatically. We must declare an explicit `Spend FromWallet` for
        -- the total deposit so coin selection has something to balance against.
        totalDeposit =
            List.foldl (\_ acc -> Natural.add acc govActionDeposit)
                Natural.zero
                proposalIntents

        spendDepositIntent =
            Spend <|
                FromWallet
                    { address = ctx.loadedWallet.changeAddress
                    , value = Value.onlyLovelace totalDeposit
                    , guaranteedUtxos = []
                    }

        intents =
            spendDepositIntent :: proposalIntents

        govState : TxIntent.GovernanceState
        govState =
            let
                base =
                    TxIntent.emptyGovernanceState

                withGuardrails =
                    { base
                        | guardrailsScript =
                            Just
                                { policyId = guardrailsPolicyId
                                , plutusVersion = PlutusV3
                                , scriptWitness = Witness.ByReference guardrailsScriptRefUtxo
                                }
                        , lastEnactedProtocolParamUpdateAction = ctx.lastEnactedParamUpdate
                        , lastEnactedCommitteeAction = ctx.lastEnactedCommittee
                    }
            in
            case which of
                TxB ->
                    -- TEST #3 must "follow" TEST #1 — i.e. its latestEnacted
                    -- points to TEST #1's ActionId. TEST #1 is the first
                    -- proposal in Tx A, so its ActionId is (txAHash, 0).
                    -- This overrides whatever the chain currently reports.
                    case state.txAHash of
                        Just txA ->
                            { withGuardrails
                                | lastEnactedProtocolParamUpdateAction =
                                    Just { transactionId = txA, govActionIndex = 0 }
                            }

                        Nothing ->
                            withGuardrails

                _ ->
                    withGuardrails
    in
    case which of
        TxB ->
            if state.txAHash == Nothing then
                Err "Submit Tx A first — Tx B's `Follows #1` link needs Tx A's hash."

            else
                finalize ctx govState intents

        _ ->
            finalize ctx govState intents


finalize : Context -> TxIntent.GovernanceState -> List TxIntent -> Result String TxIntent.TxFinalized
finalize ctx govState intents =
    TxIntent.finalizeAdvanced
        { govState = govState
        , localStateUtxos = ctx.localStateUtxos
        , coinSelectionAlgo = CoinSelection.largestFirst
        , evalScriptsCosts = Uplc.evalScriptsCosts Uplc.defaultVmConfig
        , costModels = ctx.costModels
        }
        (AutoFee { paymentSource = ctx.loadedWallet.changeAddress })
        []
        intents
        |> Result.mapError TxIntent.errorToString



-- #########################################################
-- CHAIN DATA FETCHING (Koios)
-- #########################################################


fetchChainData : Cmd Msg
fetchChainData =
    Cmd.batch
        [ fetchProtocolParams
        , fetchGuardrailsRefUtxo
        , fetchLastEnactedParamUpdate
        , fetchLastEnactedCommittee
        ]


{-| Fetch live Conway protocol parameters from Koios's Ogmios proxy. We
only keep the Plutus cost models, which `Uplc.evalScriptsCosts` needs.

Fired in parallel with the three other chain-data fetches; results are
accumulated into `PartialChainData` and promoted to `Ready` once all four
have arrived (see `advanceLoading`).

-}
fetchProtocolParams : Cmd Msg
fetchProtocolParams =
    Http.request
        { method = "POST"
        , headers = koiosHeaders
        , url = koiosBaseUrl ++ "/ogmios"
        , body =
            Http.jsonBody
                (JE.object
                    [ ( "jsonrpc", JE.string "2.0" )
                    , ( "method", JE.string "queryLedgerState/protocolParameters" )
                    ]
                )
        , expect =
            Http.expectJson
                (GotCostModels << Result.mapError httpErrorToString)
                costModelsDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Fetch the raw CBOR of the transaction that created the guardrails ref
UTxO, deserialize it, and pick the output at the known index. This is the
most faithful way to obtain an `Output` value identical to the on-chain
representation — as opposed to reconstructing one from Koios' JSON view.
-}
fetchGuardrailsRefUtxo : Cmd Msg
fetchGuardrailsRefUtxo =
    Http.request
        { method = "POST"
        , headers = koiosHeaders
        , url = koiosBaseUrl ++ "/tx_cbor"
        , body =
            Http.jsonBody
                (JE.object
                    [ ( "_tx_hashes"
                      , JE.list JE.string
                            [ Bytes.toHex guardrailsScriptRefUtxo.transactionId ]
                      )
                    ]
                )
        , expect =
            Http.expectJson
                (\res ->
                    GotGuardrailsRefUtxo
                        (res
                            |> Result.mapError httpErrorToString
                            |> Result.andThen extractGuardrailsOutput
                        )
                )
                txCborDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Decode the `cbor` hex string from the first entry of a `/tx_cbor`
response into the raw tx bytes.
-}
txCborDecoder : Decoder (Bytes Transaction)
txCborDecoder =
    JD.index 0 (JD.field "cbor" JD.string)
        |> JD.map Bytes.fromHexUnchecked


{-| Deserialize the tx and pick the output at `guardrailsScriptRefUtxo.outputIndex`.
-}
extractGuardrailsOutput : Bytes Transaction -> Result String Output
extractGuardrailsOutput rawTx =
    case Transaction.deserialize rawTx of
        Nothing ->
            Err "Transaction.deserialize returned Nothing for the guardrails ref tx."

        Just tx ->
            case List.drop guardrailsScriptRefUtxo.outputIndex tx.body.outputs |> List.head of
                Just out ->
                    Ok out

                Nothing ->
                    Err
                        ("Output index "
                            ++ String.fromInt guardrailsScriptRefUtxo.outputIndex
                            ++ " not found in tx (has "
                            ++ String.fromInt (List.length tx.body.outputs)
                            ++ " outputs)."
                        )


koiosHeaders : List Http.Header
koiosHeaders =
    if koiosApiToken == "" then
        []

    else
        [ Http.header "Authorization" ("Bearer " ++ koiosApiToken) ]


costModelsDecoder : Decoder CostModels
costModelsDecoder =
    JD.map3
        (\v1 v2 v3 -> CostModels (Just v1) (Just v2) (Just v3))
        (JD.at [ "result", "plutusCostModels", "plutus:v1" ] (JD.list JD.int))
        (JD.at [ "result", "plutusCostModels", "plutus:v2" ] (JD.list JD.int))
        (JD.at [ "result", "plutusCostModels", "plutus:v3" ] (JD.list JD.int))


{-| Query Koios for the most recently enacted ParameterChange action on
Preview. We must reference its ActionId as `latestEnacted` for any new
ParameterChange proposal.
-}
fetchLastEnactedParamUpdate : Cmd Msg
fetchLastEnactedParamUpdate =
    fetchLastEnactedActionId
        "/proposal_list?proposal_type=eq.ParameterChange&enacted_epoch=not.is.null&order=enacted_epoch.desc&limit=1&select=proposal_tx_hash,proposal_index,proposal_type,enacted_epoch"
        GotLastEnactedParamUpdate


{-| Query Koios for the most recently enacted committee-purpose action on
Preview. NoConfidence and NewCommittee share the same `latestEnacted`
chain, so we look at both.

NOTE: Koios appears to require any column used in a filter to also appear
in the `select` list — otherwise the response is empty.

-}
fetchLastEnactedCommittee : Cmd Msg
fetchLastEnactedCommittee =
    fetchLastEnactedActionId
        "/proposal_list?proposal_type=in.(NoConfidence,NewCommittee)&enacted_epoch=not.is.null&order=enacted_epoch.desc&limit=1&select=proposal_tx_hash,proposal_index,proposal_type,enacted_epoch"
        GotLastEnactedCommittee


fetchLastEnactedActionId :
    String
    -> (Result String (Maybe ActionId) -> Msg)
    -> Cmd Msg
fetchLastEnactedActionId path toMsg =
    Http.request
        { method = "GET"
        , headers = koiosHeaders
        , url = koiosBaseUrl ++ path
        , body = Http.emptyBody
        , expect =
            Http.expectJson
                (toMsg << Result.mapError httpErrorToString)
                actionIdListDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Decode a `/proposal_list` response with at most one element. Returns
`Nothing` if the list is empty (no enacted action exists yet on the chain).
-}
actionIdListDecoder : Decoder (Maybe ActionId)
actionIdListDecoder =
    JD.list
        (JD.map2 (\h i -> { transactionId = Bytes.fromHexUnchecked h, govActionIndex = i })
            (JD.field "proposal_tx_hash" JD.string)
            (JD.field "proposal_index" JD.int)
        )
        |> JD.map List.head


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl s ->
            "Bad URL: " ++ s

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus code ->
            "HTTP " ++ String.fromInt code

        Http.BadBody s ->
            "Decode error: " ++ s



-- #########################################################
-- VIEW
-- #########################################################


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> fromWallet WalletMsg
        , view = view
        }


view : Model -> Html Msg
view model =
    div [ HA.style "font-family" "system-ui, sans-serif", HA.style "padding" "1.5rem" ]
        [ h2 [] [ text "elm-cardano · governance proposals on Preview" ]
        , viewBody model
        ]


viewBody : Model -> Html Msg
viewBody model =
    case model of
        Startup ->
            text "Discovering wallets..."

        WalletDiscovered wallets ->
            viewWalletPicker wallets

        WalletLoading _ ->
            text "Loading wallet utxos..."

        WalletLoaded lw ->
            div []
                [ viewLoadedWallet lw
                , button [ onClick LoadChainDataClicked ] [ text "Load chain data (pparams + guardrails script)" ]
                ]

        LoadingChainData lw partial ->
            div []
                [ viewLoadedWallet lw
                , p []
                    [ text "Fetching:"
                    , ul []
                        [ li [] [ text ("protocol params — " ++ doneOrPending (partial.costModels /= Nothing)) ]
                        , li [] [ text ("guardrails ref utxo — " ++ doneOrPending (partial.guardrailsRefOutput /= Nothing)) ]
                        , li [] [ text ("last enacted ParameterChange — " ++ doneOrPending (partial.lastEnactedParamUpdate /= Nothing)) ]
                        , li [] [ text ("last enacted committee action — " ++ doneOrPending (partial.lastEnactedCommittee /= Nothing)) ]
                        ]
                    ]
                , viewErrors partial.errors
                ]

        Ready ctx state ->
            div []
                [ viewLoadedWallet ctx.loadedWallet
                , viewReady state
                , viewErrors state.errors
                ]

        Submitting ctx state { which } ->
            div []
                [ viewLoadedWallet ctx.loadedWallet
                , p [] [ text ("Signing & submitting " ++ whichTxLabel which ++ "...") ]
                , viewSubmissionStatus state
                , viewErrors state.errors
                ]

        Fatal err ->
            pre [] [ text ("FATAL: " ++ err) ]


viewReady : State -> Html Msg
viewReady state =
    let
        statusLine label hash =
            li []
                [ text label
                , text " — "
                , text (Maybe.map Bytes.toHex hash |> Maybe.withDefault "not submitted")
                ]

        txBDisabled =
            state.txAHash == Nothing
    in
    div []
        [ p []
            [ text "Each tx below anchors 100_000 ADA as the governance action deposit. Make sure your wallet has enough tADA." ]
        , div [ HA.style "display" "flex", HA.style "gap" "0.5rem" ]
            [ button [ onClick (SubmitClicked TxA) ] [ text "Submit Tx A (#1 + #2)" ]
            , button
                [ onClick (SubmitClicked TxB)
                , HA.disabled txBDisabled
                , HA.title
                    (if txBDisabled then
                        "Submit Tx A first"

                     else
                        ""
                    )
                ]
                [ text "Submit Tx B (#3 follows #1)" ]
            , button [ onClick (SubmitClicked TxC) ] [ text "Submit Tx C (#4 + #5)" ]
            ]
        , ul []
            [ statusLine "Tx A" state.txAHash
            , statusLine "Tx B" state.txBHash
            , statusLine "Tx C" state.txCHash
            ]
        ]


{-| Same status list as `viewReady` but without the action buttons —
used while a signing/submission is in flight to prevent the user from
clicking another tx and silently dropping the click in the catch-all.
-}
viewSubmissionStatus : State -> Html msg
viewSubmissionStatus state =
    let
        statusLine label hash =
            li []
                [ text label
                , text " — "
                , text (Maybe.map Bytes.toHex hash |> Maybe.withDefault "not submitted")
                ]
    in
    ul []
        [ statusLine "Tx A" state.txAHash
        , statusLine "Tx B" state.txBHash
        , statusLine "Tx C" state.txCHash
        ]


doneOrPending : Bool -> String
doneOrPending done =
    if done then
        "done"

    else
        "pending"


viewErrors : String -> Html msg
viewErrors err =
    if err == "" then
        text ""

    else
        pre [ HA.style "color" "#b00020" ] [ text err ]


viewLoadedWallet : LoadedWallet -> Html msg
viewLoadedWallet { wallet, utxos, changeAddress } =
    let
        totalLovelace =
            Dict.Any.foldl
                (\_ output sum -> Natural.add sum output.amount.lovelace)
                Natural.zero
                utxos

        formattedAda =
            -- Integer ADA, ignoring fractional lovelace for display.
            Natural.divBy (Natural.fromSafeInt 1000000) totalLovelace
                |> Maybe.map Natural.toString
                |> Maybe.withDefault "?"
    in
    div []
        [ p [] [ text ("Wallet: " ++ (Cip30.walletDescriptor wallet).name) ]
        , p [] [ text ("Address: " ++ Bytes.toHex (Address.toBytes changeAddress)) ]
        , p []
            [ text
                ("UTxO count: "
                    ++ String.fromInt (Dict.Any.size utxos)
                    ++ " — total: ₳ "
                    ++ formattedAda
                )
            ]
        ]


viewWalletPicker : List Cip30.WalletDescriptor -> Html Msg
viewWalletPicker wallets =
    div []
        [ p [] [ text "Pick a CIP-30 wallet:" ]
        , ul []
            (List.map
                (\w ->
                    li []
                        [ text (w.name ++ " ")
                        , button [ onClick (ConnectButtonClicked { id = w.id }) ] [ text "connect" ]
                        ]
                )
                wallets
            )
        ]
