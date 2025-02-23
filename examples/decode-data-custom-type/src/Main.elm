module Main exposing (main)

import Browser
import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Data as Data exposing (Data)
import Cbor.Decode
import DataDecoder exposing (Decoder)
import Html exposing (Html, div, text)
import Integer exposing (Integer)
import Natural


main : Program () () ()
main =
    Browser.element
        { init = \_ -> ( (), Cmd.none )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        , view = view
        }



-- DATA
-- Example from the Orcfax docs: https://docs.orcfax.io/consume#example-datum


orcfaxDatumBytes : Bytes a
orcfaxDatumBytes =
    Bytes.fromHexUnchecked "D87982D879834E4345522F464143542D4144412F331B0000019012C8CCD3D879821981951A000F4240D879821B0000019012C90F89581C694F647F6C6FEE725EAF731938613059AC560E573D9FA6560850EAB0"


orcfaxDatum : Data
orcfaxDatum =
    -- Constr (Natural [])
    --     [ Constr (Natural [])
    --         [ Bytes (Bytes "4345522f464143542d4144412f33")
    --         , Int (Positive (Natural [ 46714067, 25604 ]))
    --         , Constr (Natural [])
    --             [ Int (Positive (Natural [ 33173 ]))
    --             , Int (Positive (Natural [ 1000000 ]))
    --             ]
    --         ]
    --     , Constr (Natural [])
    --         [ Int (Positive (Natural [ 46731145, 25604 ]))
    --         , Bytes (Bytes "694f647f6c6fee725eaf731938613059ac560e573d9fa6560850eab0")
    --         ]
    --     ]
    Bytes.toBytes orcfaxDatumBytes
        |> Cbor.Decode.decode Data.fromCbor
        |> Maybe.withDefault (Data.List [])


type alias RationalDatum =
    { statement : RationalStatement
    , context : Data
    }


type alias RationalStatement =
    { feedId : String
    , createdAt : Integer
    , body : Rational
    }


type alias Rational =
    { num : Integer
    , denom : Integer
    }



-- DECODERS


rationalDatumDecoder : Decoder RationalDatum
rationalDatumDecoder =
    DataDecoder.constr Natural.zero <|
        \data ->
            Result.map2 RationalDatum
                (DataDecoder.index 0 rationalStatementDecoder data)
                (DataDecoder.index 1 (\d -> Ok d) data)


rationalStatementDecoder : Decoder RationalStatement
rationalStatementDecoder =
    DataDecoder.constr Natural.zero <|
        \data ->
            Result.map3 RationalStatement
                (DataDecoder.index 0 DataDecoder.text data)
                (DataDecoder.index 1 DataDecoder.int data)
                (DataDecoder.index 2 rationalDecoder data)


rationalDecoder : Decoder Rational
rationalDecoder =
    DataDecoder.constr Natural.zero <|
        \data ->
            Result.map2 Rational
                (DataDecoder.index 0 DataDecoder.int data)
                (DataDecoder.index 1 DataDecoder.int data)



-- VIEW


view : () -> Html ()
view _ =
    div []
        [ Html.p [] [ text <| "Example Orcfax datum (hex): " ++ Bytes.toHex orcfaxDatumBytes ]
        , Html.p [] [ text <| "Same thing but in the Datum custom type: " ++ Debug.toString orcfaxDatum ]
        , Html.p [] [ text <| "Decoded rational datum: " ++ Debug.toString (rationalDatumDecoder orcfaxDatum) ]
        ]
