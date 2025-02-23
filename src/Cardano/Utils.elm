module Cardano.Utils exposing (RationalNumber, UnitInterval, PositiveInterval, decodeRational, encodeRationalNumber)

{-| Just a utility module to avoid cyclic import dependencies.

@docs RationalNumber, UnitInterval, PositiveInterval, decodeRational, encodeRationalNumber

-}

import Cbor.Decode as D
import Cbor.Encode as E
import Cbor.Tag as Tag


{-| Represents a rational number.
-}
type alias RationalNumber =
    { numerator : Int
    , denominator : Int
    }


{-| Represents a unit interval (0 to 1).
-}
type alias UnitInterval =
    RationalNumber


{-| Represents a positive interval (> 0).
-}
type alias PositiveInterval =
    RationalNumber


{-| Encoder for RationalNumber type.
-}
encodeRationalNumber : RationalNumber -> E.Encoder
encodeRationalNumber =
    E.tagged (Tag.Unknown 30) <|
        E.tuple <|
            E.elems
                >> E.elem E.int .numerator
                >> E.elem E.int .denominator


{-| Decoder for RationalNumber type.
-}
decodeRational : D.Decoder RationalNumber
decodeRational =
    D.tag
        |> D.andThen
            (\tag ->
                case tag of
                    Tag.Unknown 30 ->
                        D.tuple RationalNumber <|
                            D.elems
                                >> D.elem D.int
                                >> D.elem D.int

                    _ ->
                        D.fail
            )
