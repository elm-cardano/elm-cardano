module Cardano.Script exposing
    ( Script(..), NativeScript(..), PlutusScript, PlutusVersion(..), ScriptCbor, extractSigners, hash, fromBech32, toBech32
    , Reference, refFromBytes, refFromScript, refBytes, refScript, refHash
    , toCbor, encodeNativeScript, encodePlutusScript
    , fromCbor, decodeNativeScript, jsonDecodeNativeScript
    )

{-| Script

@docs Script, NativeScript, PlutusScript, PlutusVersion, ScriptCbor, extractSigners, hash, fromBech32, toBech32

@docs Reference, refFromBytes, refFromScript, refBytes, refScript, refHash


## Encoders

@docs toCbor, encodeNativeScript, encodePlutusScript


## Decoders

@docs fromCbor, decodeNativeScript, jsonDecodeNativeScript

-}

import Bech32.Decode as Bech32
import Bech32.Encode as Bech32
import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address exposing (CredentialHash)
import Cbor.Decode as D
import Cbor.Decode.Extra as D
import Cbor.Encode as E
import Cbor.Encode.Extra as EE
import Dict exposing (Dict)
import Json.Decode as JD
import Natural exposing (Natural)


{-| Script Reference type, to be used to store a script into UTxOs.
Internally, it also stores the Bytes version of the script to later have correct fees computations.
-}
type Reference
    = Reference
        { scriptHash : Bytes CredentialHash
        , bytes : Bytes Script
        , script : Script
        }


{-| Create a Script Reference from the script bytes.
Returns Nothing if the bytes are not a valid script.
-}
refFromBytes : Bytes Script -> Maybe Reference
refFromBytes bytes =
    let
        taggedRawScriptDecoder =
            D.length
                |> D.ignoreThen (D.map2 rawConcat D.raw D.raw)

        rawConcat raw1 raw2 =
            Bytes.concat (Bytes.fromBytes raw1) (Bytes.fromBytes raw2)

        elmBytes =
            Bytes.toBytes bytes
    in
    case ( D.decode fromCbor elmBytes, D.decode taggedRawScriptDecoder elmBytes ) of
        ( Just script, Just taggedScriptBytes ) ->
            Just <|
                Reference
                    { scriptHash = Bytes.blake2b224 taggedScriptBytes
                    , bytes = bytes
                    , script = script
                    }

        _ ->
            Nothing


{-| Create a Script Reference from a Script (using elm-cardano encoding approach).
-}
refFromScript : Script -> Reference
refFromScript script =
    Reference
        { scriptHash = hash script
        , bytes = Bytes.fromBytes <| E.encode (toCbor script)
        , script = script
        }


{-| Extract the Script from a script Reference.
-}
refScript : Reference -> Script
refScript (Reference { script }) =
    script


{-| Extract the Bytes from a script Reference.

If the script was encoded as would elm-cardano,
this would be equivalent to calling `Bytes.fromBytes <| encode (toCbor script)`

-}
refBytes : Reference -> Bytes Script
refBytes (Reference { bytes }) =
    bytes


{-| Extract the Script hash from the script Reference.
-}
refHash : Reference -> Bytes CredentialHash
refHash (Reference { scriptHash }) =
    scriptHash


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


{-| Extract all mentionned pubkeys in the Native script.
Keys of the dict are the hex version of the keys.
-}
extractSigners : NativeScript -> Dict String (Bytes CredentialHash)
extractSigners nativeScript =
    extractSignersHelper nativeScript Dict.empty


extractSignersHelper : NativeScript -> Dict String (Bytes CredentialHash) -> Dict String (Bytes CredentialHash)
extractSignersHelper nativeScript accum =
    case nativeScript of
        ScriptPubkey key ->
            Dict.insert (Bytes.toHex key) key accum

        ScriptAll list ->
            List.foldl extractSignersHelper accum list

        ScriptAny list ->
            List.foldl extractSignersHelper accum list

        ScriptNofK _ list ->
            List.foldl extractSignersHelper accum list

        InvalidBefore _ ->
            accum

        InvalidHereafter _ ->
            accum


{-| Compute the script hash.

The script type tag must be prepended before hashing,
but not encapsulated as a list to make a valid CBOR struct.
This is not valid CBOR, just concatenation of tag|scriptBytes.

-}
hash : Script -> Bytes CredentialHash
hash script =
    taggedEncoder script
        |> E.encode
        |> Bytes.fromBytes
        |> Bytes.blake2b224


{-| Convert a script hash to its Bech32 representation.
-}
toBech32 : Bytes CredentialHash -> String
toBech32 id =
    Bech32.encode { prefix = "script", data = Bytes.toBytes id }
        |> Result.withDefault "script"


{-| Convert a script hash from its Bech32 representation.
-}
fromBech32 : String -> Maybe (Bytes CredentialHash)
fromBech32 str =
    case Bech32.decode str of
        Err _ ->
            Nothing

        Ok { prefix, data } ->
            if prefix == "script" then
                Just <| Bytes.fromBytes data

            else
                Nothing


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


{-| Decode NativeScript from its JSON node specification.

<https://github.com/IntersectMBO/cardano-node/blob/40ebadd4b70530f89fe76513c108a1a356ad16ea/doc/reference/simple-scripts.md#type-after>

-}
jsonDecodeNativeScript : JD.Decoder NativeScript
jsonDecodeNativeScript =
    let
        sig =
            JD.field "keyHash" JD.string
                |> JD.andThen
                    (\hashHex ->
                        case Bytes.fromHex hashHex of
                            Nothing ->
                                JD.fail <| "Invalid key hash: " ++ hashHex

                            Just scriptHash ->
                                JD.succeed <| ScriptPubkey scriptHash
                    )
    in
    JD.field "type" JD.string
        |> JD.andThen
            (\nodeType ->
                case nodeType of
                    "sig" ->
                        sig

                    "all" ->
                        JD.field "scripts" <|
                            JD.map ScriptAll <|
                                JD.lazy (\_ -> JD.list jsonDecodeNativeScript)

                    "any" ->
                        JD.field "scripts" <|
                            JD.map ScriptAny <|
                                JD.list (JD.lazy (\_ -> jsonDecodeNativeScript))

                    "atLeast" ->
                        JD.map2 ScriptNofK
                            (JD.field "required" JD.int)
                            (JD.field "scripts" <| JD.list (JD.lazy (\_ -> jsonDecodeNativeScript)))

                    -- TODO: is this actually the reverse of the CBOR???
                    "after" ->
                        JD.field "slot" JD.int
                            -- TODO: can we fix this to also be correct with numbers bigger than 2^53?
                            -- Unlikely error considering slots are in seconds (not milliseconds)?
                            |> JD.map (InvalidBefore << Natural.fromSafeInt)

                    "before" ->
                        JD.field "slot" JD.int
                            -- TODO: can we fix this to also be correct with numbers bigger than 2^53?
                            -- Unlikely error considering slots are in seconds (not milliseconds)?
                            |> JD.map (InvalidHereafter << Natural.fromSafeInt)

                    _ ->
                        JD.fail <| "Unknown type: " ++ nodeType
            )
