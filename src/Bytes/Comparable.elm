module Bytes.Comparable exposing
    ( Bytes
    , Any, toAny
    , concat, chunksOf, width, isEmpty
    , empty, fromBytes, fromHex, fromHexUnchecked, fromText, fromU8
    , toBytes, toHex, toText, toCbor, toU8
    , jsonEncode, jsonDecoder
    , blake2b224, blake2b256, blake2b512
    , dummy, dummyWithPrefix, pretty
    )

{-| Comparable Bytes

-- TODO: Because we rely on the sorting of the hex string,
-- it is super important to check that we only use
-- lower case letters, but no uppercase.

@docs Bytes
@docs Any, toAny
@docs concat, chunksOf, width, isEmpty
@docs empty, fromBytes, fromHex, fromHexUnchecked, fromText, fromU8
@docs toBytes, toHex, toText, toCbor, toU8
@docs jsonEncode, jsonDecoder
@docs blake2b224, blake2b256, blake2b512
@docs dummy, dummyWithPrefix, pretty

-}

import Blake2b
import Bytes
import Bytes.Decode as D
import Bytes.Encode as E
import Cbor.Encode as Cbor
import Hex.Convert as Hex
import Json.Decode as JD
import Json.Encode as JE


{-| A custom `Bytes` type that is comparable with `==`.

Useful as otherwise, the original `Bytes` type from `elm/bytes` package cannot be used to compare for equality with `==`.
The phantom type parameter `a` indicates what type of Bytes are stored.

-}
type Bytes a
    = Bytes String


{-| A catch-all phantom type for bytes.
-}
type Any
    = Any Never


{-| Convert any type of bytes to `Bytes Any`.
-}
toAny : Bytes a -> Bytes Any
toAny (Bytes str) =
    Bytes str


{-| Create a [Bytes] object from individual U8 integers.
-}
fromU8 : List Int -> Bytes a
fromU8 =
    List.map E.unsignedInt8 >> E.sequence >> E.encode >> fromBytes


{-| Check if this is empy.
-}
isEmpty : Bytes a -> Bool
isEmpty (Bytes str) =
    String.isEmpty str


{-| Length in bytes.
-}
width : Bytes a -> Int
width (Bytes str) =
    String.length str // 2


{-| Create an empty Bytes object.
-}
empty : Bytes a
empty =
    fromHexUnchecked ""


{-| Create a [Bytes] object from a hex-encoded string.
-}
fromHex : String -> Maybe (Bytes a)
fromHex str =
    str |> Hex.toBytes |> Maybe.map (always <| Bytes (String.toLower str))


{-| Same as [fromHex] except it does not check that the hex-encoded string is well formed.
It is your responsability.
-}
fromHexUnchecked : String -> Bytes a
fromHexUnchecked =
    Bytes


{-| Create a [Bytes] with some text encoded as UTF8.
-}
fromText : String -> Bytes a
fromText str =
    E.encode (E.string str) |> fromBytes


{-| Create a [Bytes] object from an elm/bytes [Bytes.Bytes].
-}
fromBytes : Bytes.Bytes -> Bytes a
fromBytes bs =
    Bytes (String.toLower <| Hex.toString bs)


{-| Convert [Bytes] into a hex-encoded String.
-}
toHex : Bytes a -> String
toHex (Bytes str) =
    str


{-| Convert [Bytes] into a UTF8 String.
-}
toText : Bytes a -> Maybe String
toText bs =
    D.decode (D.string <| width bs) (toBytes bs)


{-| Convert [Bytes] into elm/bytes [Bytes.Bytes].
-}
toBytes : Bytes a -> Bytes.Bytes
toBytes (Bytes str) =
    str |> Hex.toBytes |> Maybe.withDefault absurd


{-| Cbor encoder.
-}
toCbor : Bytes a -> Cbor.Encoder
toCbor =
    toBytes >> Cbor.bytes


{-| JSON decoder for Bytes.
-}
jsonDecoder : JD.Decoder (Bytes a)
jsonDecoder =
    JD.string
        |> JD.andThen
            (\hex ->
                fromHex hex
                    |> Maybe.map JD.succeed
                    |> Maybe.withDefault (JD.fail <| "Failed to decode Bytes: " ++ hex)
            )


{-| JSON encoder for Bytes.
-}
jsonEncode : Bytes a -> JE.Value
jsonEncode (Bytes hex) =
    JE.string hex


absurd : Bytes.Bytes
absurd =
    E.encode (E.sequence [])


{-| Concatenate two bytes sequences.
-}
concat : Bytes a -> Bytes b -> Bytes c
concat (Bytes b1) (Bytes b2) =
    Bytes (b1 ++ b2)


{-| Break a Bytestring into a list of chunks. Chunks are of the given width,
except the last chunk which is only _at most_ the given width.
-}
chunksOf : Int -> Bytes a -> List (Bytes a)
chunksOf n =
    toBytes
        >> (\bs ->
                D.decode
                    (D.loop ( Bytes.width bs, [] ) <|
                        \( w, chunks ) ->
                            if w == 0 then
                                D.succeed (D.Done <| List.reverse chunks)

                            else
                                let
                                    len =
                                        min w n
                                in
                                D.bytes len
                                    |> D.map (\chunk -> D.Loop ( w - len, chunk :: chunks ))
                    )
                    bs
                    |> Maybe.withDefault []
           )
        >> List.map fromBytes


{-| Convert a given [Bytes] into a list of U8 integers.
-}
toU8 : Bytes a -> List Int
toU8 bs =
    bytesToU8 (width bs) (toBytes bs)


bytesToU8 : Int -> Bytes.Bytes -> List Int
bytesToU8 size bs =
    D.decode (D.loop ( size, [] ) splitStep) bs
        |> Maybe.withDefault []


splitStep : ( Int, List Int ) -> D.Decoder (D.Step ( Int, List Int ) (List Int))
splitStep ( size, u8s ) =
    if size <= 0 then
        D.succeed (D.Done <| List.reverse u8s)

    else
        D.map (\u8 -> D.Loop ( size - 1, u8 :: u8s )) D.unsignedInt8


{-| Compute the Blake2b-224 hash (28 bytes) of the given bytes.
-}
blake2b224 : Bytes a -> Bytes b
blake2b224 bs =
    hash (Blake2b.blake2b224 Nothing) bs


{-| Compute the Blake2b-256 hash (32 bytes) of the given bytes.
-}
blake2b256 : Bytes a -> Bytes b
blake2b256 bs =
    hash (Blake2b.blake2b256 Nothing) bs


{-| Compute the Blake2b-512 hash (64 bytes) of the given bytes.
-}
blake2b512 : Bytes a -> Bytes b
blake2b512 bs =
    hash (Blake2b.blake2b512 Nothing) bs


{-| Helper parameterized hash function.
-}
hash : (List Int -> List Int) -> Bytes a -> Bytes b
hash hashFunction bs =
    fromU8 <| hashFunction <| toU8 bs


{-| Helper function to make up some bytes of a given length,
starting by the given text when decoded as text.
-}
dummy : Int -> String -> Bytes a
dummy length prefix =
    let
        zeroSuffix =
            String.repeat (2 * length) "0"
    in
    fromText (prefix ++ zeroSuffix)
        |> toHex
        |> String.slice 0 (2 * length)
        |> fromHexUnchecked


{-| Helper function to make up some bytes of a given length,
starting with the provided bytes.
-}
dummyWithPrefix : Int -> Bytes a -> Bytes b
dummyWithPrefix length bytesPrefix =
    let
        zeroSuffix =
            String.repeat (2 * length) "0"
    in
    (toHex bytesPrefix ++ zeroSuffix)
        |> String.slice 0 (2 * length)
        |> fromHexUnchecked


{-| Helper function that convert bytes to either Text if it looks like text,
or its Hex representation otherwise.
-}
pretty : Bytes a -> String
pretty b =
    case toText b of
        Nothing ->
            toHex b

        Just text ->
            let
                isLikelyAscii char =
                    Char.toCode char < 128
            in
            if String.all isLikelyAscii text then
                text

            else
                toHex b
