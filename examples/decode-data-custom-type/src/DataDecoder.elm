module DataDecoder exposing (..)

import Bytes.Comparable as Bytes exposing (Any, Bytes)
import Cardano.Data exposing (Data(..))
import Integer exposing (Integer)
import Natural exposing (Natural)


type alias Decoder a =
    Data -> Result String a



-- Basic decoders


int : Decoder Integer
int data =
    case data of
        Int n ->
            Ok n

        _ ->
            Err "Expected Int"


bytes : Decoder (Bytes Any)
bytes data =
    case data of
        Bytes b ->
            Ok b

        _ ->
            Err "Expected Bytes"


text : Decoder String
text =
    bytes
        |> andThen
            (\b ->
                case Bytes.toText b of
                    Nothing ->
                        \_ -> Err "Expected text"

                    Just str ->
                        \_ -> Ok str
            )



-- List decoder


list : Decoder a -> Decoder (List a)
list decoder data =
    case data of
        List items ->
            List.foldr
                (\item -> Result.andThen (\acc -> Result.map (\x -> x :: acc) (decoder item)))
                (Ok [])
                items

        _ ->
            Err "Expected List"



-- Constructor decoder


constr : Natural -> Decoder a -> Decoder a
constr expected decoder data =
    case data of
        Constr tag items ->
            if tag == expected then
                decoder data

            else
                Err "Wrong constructor tag"

        _ ->
            Err "Expected Constructor"



-- Get nth item from constructor


index : Int -> Decoder a -> Decoder a
index n decoder data =
    case data of
        Constr _ items ->
            case List.drop n items of
                item :: _ ->
                    decoder item

                [] ->
                    Err "Index out of bounds"

        _ ->
            Err "Expected Constructor"



-- Combine decoders


map : (a -> b) -> Decoder a -> Decoder b
map f decoderA data =
    Result.map f (decoderA data)


map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 f decoderA decoderB data =
    Result.map2 f (decoderA data) (decoderB data)



-- Chain decoders


andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen f decoder data =
    Result.andThen (\a -> f a data) (decoder data)
