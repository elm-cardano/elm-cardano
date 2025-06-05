module Cardano.Data exposing (Data(..), hash, rawDatumHash, fromBytes, fromCbor, toCbor, toCborUplc)

{-| Handling Cardano Data objects.

@docs Data, hash, rawDatumHash, fromBytes, fromCbor, toCbor, toCborUplc

-}

import Bytes.Comparable as Bytes exposing (Any, Bytes)
import Cbor exposing (CborItem(..))
import Cbor.Decode as D
import Cbor.Decode.Extra as DE
import Cbor.Encode as E
import Cbor.Encode.Extra as EE
import Cbor.Tag as Tag
import Integer exposing (Integer)
import Natural exposing (Natural)


{-| A Data is a compound type that can represent any possible user-defined type in Aiken.
-}
type Data
    = Constr Natural (List Data)
    | Map (List ( Data, Data ))
    | List (List Data)
    | Int Integer
    | Bytes (Bytes Any)


{-| Compute the Blake2b-256 (32 bytes) hash of a Data object.
-}
hash : Data -> Bytes a
hash data =
    E.encode (toCbor data)
        |> Bytes.fromBytes
        |> Bytes.blake2b256


{-| Helper function to compute the Blake2b-256 hash of the raw Data bytes in a UTxO datum.
-}
rawDatumHash : Bytes Data -> Bytes a
rawDatumHash rawData =
    Bytes.blake2b256 rawData


{-| Helper function to decode a Data object from its raw CBOR bytes.
-}
fromBytes : Bytes a -> Maybe Data
fromBytes bytes =
    Bytes.toBytes bytes
        |> D.decode fromCbor


{-| CBOR encoder for [Data].
-}
toCbor : Data -> E.Encoder
toCbor data =
    case data of
        Constr ixNat fields ->
            if ixNat |> Natural.isLessThan (Natural.fromSafeInt 128) then
                let
                    ix =
                        Natural.toInt ixNat
                in
                if ix < 7 then
                    E.tagged (Tag.Unknown <| 121 + ix) (E.list toCbor) fields

                else
                    E.tagged (Tag.Unknown <| 1280 + ix - 7) (E.list toCbor) fields

            else
                E.tagged (Tag.Unknown 102)
                    (E.tuple <|
                        E.elems
                            >> E.elem EE.natural .ixNat
                            >> E.elem (E.list toCbor) .fields
                    )
                    { ixNat = ixNat, fields = fields }

        Map xs ->
            EE.associativeList toCbor toCbor xs

        List xs ->
            E.list toCbor xs

        Int i ->
            EE.integer i

        Bytes bytes ->
            if Bytes.width bytes <= 64 then
                E.bytes (Bytes.toBytes bytes)

            else
                E.sequence <|
                    EE.beginBytes
                        :: List.foldr
                            (\chunk rest -> E.bytes (Bytes.toBytes chunk) :: rest)
                            [ E.break ]
                            (Bytes.chunksOf 64 bytes)


{-| CBOR encoder for [Data].
Only to be used for things sent to the UPLC VM,
such as datums, redeemers, and script parameter application.
-}
toCborUplc : Data -> E.Encoder
toCborUplc data =
    let
        -- NOTE: 'Data' lists are weirdly encoded:
        --
        -- 1. They are encoded as definite empty lists (0x80)
        -- 2. But, are encoded as indefinite list otherwise.
        encodeList : List Data -> E.Encoder
        encodeList xs =
            case xs of
                [] ->
                    E.length 0

                _ ->
                    EE.indefiniteList toCborUplc xs
    in
    case data of
        Constr ixNat fields ->
            if ixNat |> Natural.isLessThan (Natural.fromSafeInt 128) then
                let
                    ix =
                        Natural.toInt ixNat
                in
                if ix < 7 then
                    E.tagged (Tag.Unknown <| 121 + ix) encodeList fields

                else
                    E.tagged (Tag.Unknown <| 1280 + ix - 7) encodeList fields

            else
                E.tagged (Tag.Unknown 102)
                    (E.tuple <|
                        E.elems
                            >> E.elem EE.natural .ixNat
                            >> E.elem encodeList .fields
                    )
                    { ixNat = ixNat, fields = fields }

        Map xs ->
            EE.associativeList toCborUplc toCborUplc xs

        List xs ->
            encodeList xs

        Int i ->
            EE.integer i

        Bytes bytes ->
            if Bytes.width bytes <= 64 then
                E.bytes (Bytes.toBytes bytes)

            else
                E.sequence <|
                    EE.beginBytes
                        :: List.foldr
                            (\chunk rest -> E.bytes (Bytes.toBytes chunk) :: rest)
                            [ E.break ]
                            (Bytes.chunksOf 64 bytes)


{-| CBOR decoder for [Data].
-}
fromCbor : D.Decoder Data
fromCbor =
    D.any
        |> D.andThen
            (\any ->
                case fromCborItem any of
                    Nothing ->
                        D.fail

                    Just data ->
                        D.succeed data
            )


fromCborItem : CborItem -> Maybe Data
fromCborItem item =
    -- TODO: make more tail-rec,
    -- but would require difficult inlining of collectCborPairs and collectCborItems
    case item of
        CborMap xs ->
            collectCborPairs [] xs |> Maybe.map Map

        CborList xs ->
            collectCborItems [] xs |> Maybe.map List

        CborInt32 i ->
            Just (Int (Integer.fromSafeInt i))

        CborInt64 ( msb, lsb ) ->
            let
                bigMsb =
                    Integer.fromSafeInt msb
                        |> Integer.mul (Integer.fromSafeInt 4294967296)

                bigLsb =
                    Integer.fromSafeInt lsb
            in
            if msb >= 0 then
                Just (Int (Integer.add bigMsb bigLsb))

            else
                Just (Int (Integer.sub bigMsb bigLsb))

        CborBytes bs ->
            Just (Bytes <| Bytes.fromBytes bs)

        CborTag Tag.PositiveBigNum _ ->
            E.encode (E.any item)
                |> D.decode DE.integer
                |> Maybe.map Int

        CborTag Tag.NegativeBigNum _ ->
            E.encode (E.any item)
                |> D.decode DE.integer
                |> Maybe.map Int

        CborTag (Tag.Unknown n) tagged ->
            if n == 102 then
                case tagged of
                    CborList [ ixItem, CborList fields ] ->
                        case ( unwrapCborUint ixItem, collectCborItems [] fields ) of
                            ( Just ix, Just items ) ->
                                Just (Constr ix items)

                            _ ->
                                Nothing

                    _ ->
                        Nothing

            else
                case tagged of
                    CborList fields ->
                        let
                            ix =
                                if n >= 1280 then
                                    n - 1280 + 7

                                else
                                    n - 121
                        in
                        collectCborItems [] fields
                            |> Maybe.map (Constr <| Natural.fromSafeInt ix)

                    _ ->
                        Nothing

        _ ->
            Nothing


unwrapCborUint : CborItem -> Maybe Natural
unwrapCborUint item =
    case item of
        CborInt32 i ->
            if i >= 0 then
                Just (Natural.fromSafeInt i)

            else
                Nothing

        CborInt64 ( msb, lsb ) ->
            if msb >= 0 then
                let
                    bigMsb =
                        Natural.fromSafeInt msb
                            |> Natural.mul (Natural.fromSafeInt 4294967296)

                    bigLsb =
                        Natural.fromSafeInt lsb
                in
                Just (Natural.add bigMsb bigLsb)

            else
                Nothing

        _ ->
            Nothing


collectCborPairs : List ( Data, Data ) -> List ( CborItem, CborItem ) -> Maybe (List ( Data, Data ))
collectCborPairs st pairs =
    case pairs of
        [] ->
            Just (List.reverse st)

        ( left, right ) :: tail ->
            case ( fromCborItem left, fromCborItem right ) of
                ( Just l, Just r ) ->
                    collectCborPairs (( l, r ) :: st) tail

                _ ->
                    Nothing


collectCborItems : List Data -> List CborItem -> Maybe (List Data)
collectCborItems st items =
    case items of
        [] ->
            Just (List.reverse st)

        head :: tail ->
            case fromCborItem head of
                Just s ->
                    collectCborItems (s :: st) tail

                Nothing ->
                    Nothing
