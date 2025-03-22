module Cardano.Script exposing
    ( Script(..), NativeScript(..), PlutusScript, PlutusVersion(..), ScriptCbor, extractSigners, hash, fromBech32, toBech32
    , Reference, refFromBytes, refFromScript, refBytes, refScript, refHash
    , nativeScriptFromBytes, nativeScriptBytes
    , plutusScriptFromBytes, plutusVersion, cborWrappedBytes
    , toCbor, encodeNativeScript
    , fromCbor, decodeNativeScript, jsonDecodeNativeScript
    )

{-| Script

@docs Script, NativeScript, PlutusScript, PlutusVersion, ScriptCbor, extractSigners, hash, fromBech32, toBech32

@docs Reference, refFromBytes, refFromScript, refBytes, refScript, refHash


## Scripts and Bytes

@docs nativeScriptFromBytes, nativeScriptBytes

@docs plutusScriptFromBytes, plutusVersion, cborWrappedBytes


## CBOR Encoders

@docs toCbor, encodeNativeScript


## CBOR and JSON Decoders

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
type PlutusScript
    = PlutusScript
        { version : PlutusVersion
        , flatBytes : Bytes Flat
        }


{-| The plutus version.
-}
type PlutusVersion
    = PlutusV1
    | PlutusV2
    | PlutusV3


{-| Phantom type describing the kind of bytes within a [PlutusScript] object.
They are supposed to be the Flat bytes wrapped into a single CBOR byte string.
-}
type ScriptCbor
    = ScriptCbor Never


{-| Phantom type describing the Flat encoding of Plutus scripts.
-}
type Flat
    = Flat Never


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


{-| Get the bytes representation of a native script.

This is just a helper function doing the following:

    nativeScriptBytes nativeScript =
        encodeNativeScript nativeScript
            |> Cbor.Encode.encode
            |> Bytes.fromBytes

-}
nativeScriptBytes : NativeScript -> Bytes a
nativeScriptBytes nativeScript =
    encodeNativeScript nativeScript
        |> E.encode
        |> Bytes.fromBytes


{-| Decode a native script from its bytes representation.

This is just a helper function doing the following:

    Cbor.Decode.decode decodeNativeScript (Bytes.toBytes bytes)

-}
nativeScriptFromBytes : Bytes a -> Maybe NativeScript
nativeScriptFromBytes bytes =
    D.decode decodeNativeScript (Bytes.toBytes bytes)


{-| Create a PlutusScript from its bytes representation.

Depending on where the bytes come from, they can have many different shapes.
At the lowest level, Plutus scripts are encoded with a format called [Flat][flat].
But since Cardano uses CBOR everywhere else, the Flat bytes are usually wrapped in a CBOR byte string.
But that’s not the only shape they can have.
Indeed, when transmitted in transactions like in a UTxO script reference,
they are usually packed into a pair containing an integer tag for the Plutus version,
and the CBOR bytes themselves, re-encoded as Bytes in the pair!
Meaning the script Flat bytes might get a double CBOR byte wrap.

And if that wasn’t confusing enough, when computing the hash of a Plutus script,
it’s not even the above CBOR tagged pair that is used.
Instead it is the raw concatenation of the tag and the single-wrapped Flat bytes.

Summary, with pseudo-code:

1.  `flat_bytes`: the actual Plutus script bytes.
2.  `cbor_bytes(flat_bytes)`: the thing that is provided in the `plutus.json` blueprint of an Aiken compilation.
3.  `cbor_bytes(cbor_bytes(flat_bytes))`: how encoded in the witness set and by some CLI tools.
4.  `tag + cbor_bytes(flat_bytes)`: the input of the Blake2b-224 hash to compute the script hash.
5.  `cbor_array[cbor_int(tag), cbor_bytes(cbor_bytes(flat_bytes))]`: the thing transmitted in UTxO script references.

It’s confusing and error prone, so we made the `PlutusScript` type opaque,
and you basically need to call this function to convert from bytes,
which will smartly figure it out.

WARNING: This function is only for shapes (1, 2, 3).
Shape (4) is never used except temporarily to compute hashes.
Shape (5) is supposed to be decoded with the `Script.fromCbor` decoder:
`D.decode Script.fromCbor scriptCborBytes`.

If no particular shape is recognized, this function will assume the whole bytes are the Flat bytes,
because we don’t have a Flat decoder in Elm.

[flat]: https://quid2.org/docs/Flat.pdf

-}
plutusScriptFromBytes : PlutusVersion -> Bytes a -> PlutusScript
plutusScriptFromBytes version bytes =
    case D.decode D.bytes (Bytes.toBytes bytes) of
        Nothing ->
            PlutusScript
                { version = version
                , flatBytes = Bytes.fromHexUnchecked <| Bytes.toHex bytes
                }

        Just wrappedBytes ->
            case D.decode D.bytes wrappedBytes of
                Nothing ->
                    PlutusScript
                        { version = version
                        , flatBytes = Bytes.fromBytes wrappedBytes
                        }

                Just flatBytes ->
                    PlutusScript
                        { version = version
                        , flatBytes = Bytes.fromBytes flatBytes
                        }


{-| Extract the PlutusVersion from a PlutusScript.
-}
plutusVersion : PlutusScript -> PlutusVersion
plutusVersion (PlutusScript plutusScript) =
    plutusScript.version


{-| Extract the script bytes, wrapped in a CBOR byte string,
So basically the same thing that you would get in the `plutus.json` blueprint file.

This is returning: `cbor_bytes(flat_bytes)`.

-}
cborWrappedBytes : PlutusScript -> Bytes ScriptCbor
cborWrappedBytes (PlutusScript plutusScript) =
    plutusScript.flatBytes
        |> Bytes.toBytes
        |> E.bytes
        |> E.encode
        |> Bytes.fromBytes


{-| Compute the script hash.

The script type tag must be prepended before hashing,
but not encapsulated as a list to make a valid CBOR struct.
This is not valid CBOR, just concatenation of tag|scriptBytes.

-}
hash : Script -> Bytes CredentialHash
hash script =
    let
        -- Almost like taggedEncoder function, but not exactly
        hashEncoder : E.Encoder
        hashEncoder =
            case script of
                Native nativeScript ->
                    E.sequence
                        [ E.int 0
                        , encodeNativeScript nativeScript
                        ]

                Plutus (PlutusScript plutusScript) ->
                    E.sequence
                        [ encodePlutusVersion plutusScript.version

                        -- Single CBOR wrap of the Flat bytes
                        , E.bytes (Bytes.toBytes plutusScript.flatBytes)
                        ]
    in
    E.encode hashEncoder
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

        Plutus (PlutusScript plutusScript) ->
            E.sequence
                [ encodePlutusVersion plutusScript.version

                -- Double CBOR wrapping of the script flat bytes
                , E.bytes (Bytes.toBytes plutusScript.flatBytes)
                    |> E.encode
                    |> E.bytes
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
                        D.map Plutus (plutusFromCbor PlutusV1)

                    2 ->
                        D.map Plutus (plutusFromCbor PlutusV2)

                    3 ->
                        D.map Plutus (plutusFromCbor PlutusV3)

                    _ ->
                        D.failWith ("Unknown script version: " ++ String.fromInt v)
            )


plutusFromCbor : PlutusVersion -> D.Decoder PlutusScript
plutusFromCbor version =
    D.bytes
        |> D.andThen
            -- Double CBOR unwrapping of the script flat bytes
            (\wrappedBytes ->
                case D.decode D.bytes wrappedBytes of
                    Just flatBytes ->
                        D.succeed (PlutusScript { version = version, flatBytes = Bytes.fromBytes flatBytes })

                    Nothing ->
                        D.failWith ("Failed to decode Plutus script: " ++ Bytes.toHex (Bytes.fromBytes wrappedBytes))
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
