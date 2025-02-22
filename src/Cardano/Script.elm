module Cardano.Script exposing
    ( Script(..), NativeScript(..), PlutusScript, PlutusVersion(..), ScriptCbor, hash
    , toCbor, encodeNativeScript, encodePlutusScript
    , fromCbor, decodeNativeScript
    )

{-| Script

@docs Script, NativeScript, PlutusScript, PlutusVersion, ScriptCbor, hash


## Encoders

@docs toCbor, encodeNativeScript, encodePlutusScript


## Decoders

@docs fromCbor, decodeNativeScript

-}

import Blake2b exposing (blake2b224)
import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address exposing (CredentialHash)
import Cbor.Decode as D
import Cbor.Decode.Extra as D
import Cbor.Encode as E
import Cbor.Encode.Extra as EE
import Natural exposing (Natural)


{-| Cardano script, either a native script or a plutus script.

`script = [ 0, native_script // 1, plutus_v1_script // 2, plutus_v2_script ]`

[Babbage implementation in Pallas][pallas].

[pallas]: https://github.com/txpipe/pallas/blob/d1ac0561427a1d6d1da05f7b4ea21414f139201e/pallas-primitives/src/babbage/model.rs#L58

-}
type Script
    = Native NativeScript
    | Plutus PlutusScript


{-| A native script
<https://github.com/txpipe/pallas/blob/d1ac0561427a1d6d1da05f7b4ea21414f139201e/pallas-primitives/src/alonzo/model.rs#L772>
-}
type NativeScript
    = ScriptPubkey (Bytes CredentialHash)
    | ScriptAll (List NativeScript)
    | ScriptAny (List NativeScript)
    | ScriptNofK Int (List NativeScript)
    | InvalidBefore Natural
    | InvalidHereafter Natural


{-| A plutus script.
-}
type alias PlutusScript =
    { version : PlutusVersion
    , script : Bytes ScriptCbor
    }


{-| The plutus version.
-}
type PlutusVersion
    = PlutusV1
    | PlutusV2
    | PlutusV3


{-| Phantom type describing the kind of bytes within a [PlutusScript] object.
-}
type ScriptCbor
    = ScriptCbor Never


{-| Compute the script hash.

The script type tag must be prepended before hashing,
but not encapsulated as a list to make a valid CBOR struct.
This is not valid CBOR, just concatenation of tag|scriptBytes.

-}
hash : Script -> Bytes CredentialHash
hash script =
    let
        taggedScriptBytes =
            taggedEncoder script
                |> E.encode
                |> Bytes.fromBytes
    in
    blake2b224 Nothing (Bytes.toU8 <| Debug.log "SCRIPT ABOUT TO BE HASHED" taggedScriptBytes)
        |> Bytes.fromU8


{-| Cbor Encoder for [Script]
-}
toCbor : Script -> E.Encoder
toCbor script =
    E.sequence [ E.length 2, taggedEncoder script ]


{-| Helper encoder that prepends a tag (corresponding to language) to the script bytes.
-}
taggedEncoder : Script -> E.Encoder
taggedEncoder script =
    case script of
        Native nativeScript ->
            E.sequence
                [ E.int 0
                , encodeNativeScript nativeScript
                ]

        Plutus plutusScript ->
            E.sequence
                [ encodePlutusVersion plutusScript.version
                , encodePlutusScript plutusScript
                ]


{-| Cbor Encoder for [NativeScript]
-}
encodeNativeScript : NativeScript -> E.Encoder
encodeNativeScript nativeScript =
    E.list identity <|
        case nativeScript of
            ScriptPubkey addrKeyHash ->
                [ E.int 0
                , Bytes.toCbor addrKeyHash
                ]

            ScriptAll nativeScripts ->
                [ E.int 1
                , E.list encodeNativeScript nativeScripts
                ]

            ScriptAny nativeScripts ->
                [ E.int 2
                , E.list encodeNativeScript nativeScripts
                ]

            ScriptNofK atLeast nativeScripts ->
                [ E.int 3
                , E.int atLeast
                , E.list encodeNativeScript nativeScripts
                ]

            InvalidBefore start ->
                [ E.int 4
                , EE.natural start
                ]

            InvalidHereafter end ->
                [ E.int 5
                , EE.natural end
                ]


{-| Cbor Encoder for PlutusScript
-}
encodePlutusScript : PlutusScript -> E.Encoder
encodePlutusScript { script } =
    Bytes.toCbor script


encodePlutusVersion : PlutusVersion -> E.Encoder
encodePlutusVersion version =
    E.int <|
        case version of
            PlutusV1 ->
                1

            PlutusV2 ->
                2

            PlutusV3 ->
                3



-- Decoders


{-| CBOR decoder for [Script].

This does not contain the double CBOR decoding of the `script_ref` UTxO field.
That part has to be handled in the UTxO decoder.

-}
fromCbor : D.Decoder Script
fromCbor =
    D.length
        |> D.ignoreThen D.int
        |> D.andThen
            (\v ->
                case v of
                    0 ->
                        D.map Native decodeNativeScript

                    1 ->
                        D.map (\s -> Plutus { version = PlutusV1, script = Bytes.fromBytes s }) D.bytes

                    2 ->
                        D.map (\s -> Plutus { version = PlutusV2, script = Bytes.fromBytes s }) D.bytes

                    3 ->
                        D.map (\s -> Plutus { version = PlutusV3, script = Bytes.fromBytes s }) D.bytes

                    _ ->
                        D.failWith ("Unknown script version: " ++ String.fromInt v)
            )


{-| Decode NativeScript from CBOR.
-}
decodeNativeScript : D.Decoder NativeScript
decodeNativeScript =
    D.length
        |> D.ignoreThen D.int
        |> D.andThen
            (\tag ->
                case tag of
                    0 ->
                        D.map (ScriptPubkey << Bytes.fromBytes) D.bytes

                    1 ->
                        D.map ScriptAll (D.list decodeNativeScript)

                    2 ->
                        D.map ScriptAny (D.list decodeNativeScript)

                    3 ->
                        D.map2 ScriptNofK D.int (D.list decodeNativeScript)

                    4 ->
                        D.map InvalidBefore D.natural

                    5 ->
                        D.map InvalidHereafter D.natural

                    _ ->
                        D.fail
            )
