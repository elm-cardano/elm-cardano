module Cardano.DataTests exposing (..)

import Bytes.Comparable as Bytes exposing (fromU8)
import Cardano.Data as Data exposing (Data(..))
import Cbor.Decode as D
import Cbor.Encode as E
import Cbor.Test as Cbor
import Dict.Any
import Expect
import Fuzz exposing (Fuzzer)
import Hex
import Integer
import Natural
import Test exposing (Test, describe, test)
import Tests exposing (expectBytes)


fuzzer : Fuzzer Data
fuzzer =
    let
        byte =
            Fuzz.intRange 0 255

        leaf =
            Fuzz.oneOf
                [ Fuzz.map Int <|
                    Fuzz.oneOf
                        [ Fuzz.map Integer.fromSafeInt Fuzz.int
                        , Fuzz.map Integer.fromSafeInt <| Fuzz.intRange -(2 ^ 51) -(2 ^ 50)
                        , Fuzz.map Integer.fromSafeInt <| Fuzz.intRange (2 ^ 50) (2 ^ 51)
                        ]
                , Fuzz.map (Bytes << fromU8) <|
                    Fuzz.oneOf
                        [ Fuzz.list byte
                        , Fuzz.listOfLengthBetween 65 130 byte
                        ]
                ]

        -- Map with no duplicate key and keys ordered
        -- by the spec for canonical CBOR (byte size then value)
        canonicalMap pairs =
            Dict.Any.fromList toCanonicalKey pairs
                |> Dict.Any.toList
                |> Map

        toCanonicalKey k =
            let
                encodedKey =
                    E.encode (Data.toCbor k)
                        |> Bytes.fromBytes
                        |> Bytes.toHex
            in
            ( String.length encodedKey, encodedKey )

        whole depth =
            if depth <= 0 then
                leaf

            else
                let
                    nested =
                        whole (depth - 1)
                in
                Fuzz.oneOf
                    [ Fuzz.map List <|
                        Fuzz.listOfLengthBetween 0 depth nested
                    , Fuzz.map canonicalMap <|
                        Fuzz.listOfLengthBetween 0 depth <|
                            Fuzz.map2 Tuple.pair nested nested
                    , Fuzz.map2 Constr
                        (Fuzz.map Natural.fromSafeInt <| Fuzz.intRange 0 199)
                        (Fuzz.listOfLengthBetween 0 depth nested)
                    , leaf
                    ]
    in
    whole 3


suite : Test
suite =
    describe "Data"
        [ describe "toCbor"
            [ testEncode "D87980" <|
                Constr Natural.zero []
            , testEncode "80" <|
                List []

            -- , testEncode "9F010203FF" <|
            -- Now using definite length instead of indefinite length
            , testEncode "83010203" <|
                List [ Int Integer.one, Int Integer.two, Int Integer.three ]
            , testEncode "A0" <|
                Map []
            , testEncode "A201020304" <|
                Map [ ( Int Integer.one, Int Integer.two ), ( Int Integer.three, Int Integer.four ) ]
            , testEncode "00" <|
                Int Integer.zero
            , testEncode "20" <|
                Int Integer.negativeOne
            , testEncode "C249010000000000000000" <|
                Int (Integer.fromSafeString "0x010000000000000000")
            , testEncode "C349010000000000000000" <|
                Int (Integer.fromSafeString "-0x010000000000000001")
            , testEncode "40" <|
                (Bytes <| fromU8 [])

            -- , testEncode "D87A9F21D87E9FD87C9F2143C2599BFF01FFD87C9F41B19F0044A06D8DCBFF40FFFF" <|
            -- Now using definite length instead of indefinite length
            , testEncode "d87a8321d87e82d87c822143c2599b01d87c8341b1820044a06d8dcb40" <|
                Constr Natural.one
                    [ Int Integer.negativeTwo
                    , Constr Natural.five [ Constr Natural.three [ Int Integer.negativeTwo, Bytes (fromU8 [ 0xC2, 0x59, 0x9B ]) ], Int Integer.one ]
                    , Constr Natural.three [ Bytes (fromU8 [ 0xB1 ]), List [ Int Integer.zero, Bytes (fromU8 [ 0xA0, 0x6D, 0x8D, 0xCB ]) ], Bytes (fromU8 []) ]
                    ]
            ]
        , describe "fromCbor"
            [ testDecode "D87980" <|
                Constr Natural.zero []
            , testDecode "80" <|
                List []
            , testDecode "9F010203FF" <|
                List [ Int Integer.one, Int Integer.two, Int Integer.three ]
            , testDecode "A0" <|
                Map []
            , testDecode "A201020304" <|
                Map [ ( Int Integer.one, Int Integer.two ), ( Int Integer.three, Int Integer.four ) ]
            , testDecode "00" <|
                Int Integer.zero
            , testDecode "20" <|
                Int Integer.negativeOne
            , testDecode "C249010000000000000000" <|
                Int (Integer.fromSafeString "0x010000000000000000")
            , testDecode "C349010000000000000000" <|
                Int (Integer.fromSafeString "-0x010000000000000001")
            , testDecode "40" <|
                (Bytes <| fromU8 [])
            , testDecode "D87A9F21D87E9FD87C9F2143C2599BFF01FFD87C9F41B19F0044A06D8DCBFF40FFFF" <|
                Constr Natural.one
                    [ Int Integer.negativeTwo
                    , Constr Natural.five [ Constr Natural.three [ Int Integer.negativeTwo, Bytes (fromU8 [ 0xC2, 0x59, 0x9B ]) ], Int Integer.one ]
                    , Constr Natural.three [ Bytes (fromU8 [ 0xB1 ]), List [ Int Integer.zero, Bytes (fromU8 [ 0xA0, 0x6D, 0x8D, 0xCB ]) ], Bytes (fromU8 []) ]
                    ]
            ]
        , describe "toCbor >> fromCbor"
            [ Cbor.roundtrip Data.toCbor Data.fromCbor fuzzer
            ]
        ]


testEncode : String -> Data -> Test
testEncode bytes data =
    test (Debug.toString data) <|
        \_ -> data |> Data.toCbor |> E.encode |> Bytes.fromBytes |> expectBytes bytes


testDecode : String -> Data -> Test
testDecode bytes data =
    test (Debug.toString data) <|
        \_ -> String.toLower bytes |> Hex.toBytesUnchecked |> D.decode Data.fromCbor |> Expect.equal (Just data)
