module Cardano.ScriptTests exposing (suite)

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address exposing (CredentialHash)
import Cardano.Script as Script exposing (NativeScript(..), PlutusScript, Script)
import Cbor.Test exposing (roundtrip)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Fuzz.Extra as Fuzz
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Script"
        [ describe "toCbor >> fromCbor"
            [ roundtrip Script.toCbor Script.fromCbor fuzzer
            ]
        , describe "hash"
            [ test "Plutus script" plutusScriptHashTest
            ]
        , describe "Bech32 encoding and decoding"
            [ test "encoding" bech32EncodingTest
            , test "decoding" bech32DecodingTest
            ]
        ]


fuzzer : Fuzzer Script
fuzzer =
    Fuzz.oneOf
        [ Fuzz.map Script.Native nativeScriptFuzzer
        , Fuzz.map Script.Plutus plutusScriptFuzzer
        ]


nativeScriptFuzzer : Fuzzer NativeScript
nativeScriptFuzzer =
    Fuzz.lazy
        (\_ ->
            Fuzz.frequency
                [ ( 50, Fuzz.map Script.ScriptPubkey (Fuzz.bytesOfSize 28) )
                , ( 10, Fuzz.map Script.ScriptAll (Fuzz.listOfLengthBetween 0 5 nativeScriptFuzzer) )
                , ( 10, Fuzz.map Script.ScriptAny (Fuzz.listOfLengthBetween 0 5 nativeScriptFuzzer) )
                , ( 10, Fuzz.map2 Script.ScriptNofK (Fuzz.intAtMost 5) (Fuzz.listOfLengthBetween 0 5 nativeScriptFuzzer) )
                , ( 10, Fuzz.map Script.InvalidBefore Fuzz.natural )
                , ( 10, Fuzz.map Script.InvalidHereafter Fuzz.natural )
                ]
        )


plutusScriptFuzzer : Fuzzer PlutusScript
plutusScriptFuzzer =
    Fuzz.map2 PlutusScript
        plutusVersionFuzzer
        Fuzz.bytes


plutusVersionFuzzer : Fuzzer Script.PlutusVersion
plutusVersionFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant Script.PlutusV1
        , Fuzz.constant Script.PlutusV2
        , Fuzz.constant Script.PlutusV3
        ]



-- Hashes


plutusScriptHashTest : () -> Expectation
plutusScriptHashTest _ =
    Script.hash (Script.Plutus <| PlutusScript Script.PlutusV3 (Bytes.fromHexUnchecked "58b501010032323232323225333002323232323253330073370e900118041baa0011323232533300a3370e900018059baa00113322323300100100322533301100114a0264a66601e66e3cdd718098010020a5113300300300130130013758601c601e601e601e601e601e601e601e601e60186ea801cdd7180718061baa00116300d300e002300c001300937540022c6014601600460120026012004600e00260086ea8004526136565734aae7555cf2ab9f5742ae881"))
        |> Expect.equal (Bytes.fromHexUnchecked "3ff0b1bb5815347c6f0c05328556d80c1f83ca47ac410d25ffb4a330")



-- Bech32 decoding


bech32DecodingTest : () -> Expectation
bech32DecodingTest _ =
    Script.fromBech32 "script163qjya2n5rc6je07ultq5rmjfvmgm5dam0pqsuc0en4u7967saj"
        |> Expect.equal (Just emptyMultisigScript)



-- Bech32 encoding


bech32EncodingTest : () -> Expectation
bech32EncodingTest _ =
    Script.toBech32 emptyMultisigScript
        |> Expect.equal "script163qjya2n5rc6je07ultq5rmjfvmgm5dam0pqsuc0en4u7967saj"



-- Samples


emptyMultisigScript : Bytes CredentialHash
emptyMultisigScript =
    Script.Native (ScriptAll [])
        |> Script.hash
