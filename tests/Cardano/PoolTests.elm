module Cardano.PoolTests exposing (suite)

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Pool as Pool
import Expect exposing (Expectation)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Pool"
        -- Bech32 decoding tests
        [ describe "Bech32 decoding"
            [ test "smaug" testSmaugBech32DecodingTest
            ]

        -- Bech32 encoding tests
        , describe "Bech32 encoding"
            [ test "smaug" testSmaugBech32EncodingTest
            ]
        ]



-- Bech32 decoding


testSmaugBech32DecodingTest : () -> Expectation
testSmaugBech32DecodingTest _ =
    Pool.fromBech32 "pool14wk2m2af7y4gk5uzlsmsunn7d9ppldvcxxa5an9r5ywek8330fg"
        |> Expect.equal (Just smaugPoolId)



-- Bech32 encoding


testSmaugBech32EncodingTest : () -> Expectation
testSmaugBech32EncodingTest _ =
    Pool.toBech32 smaugPoolId
        |> Expect.equal "pool14wk2m2af7y4gk5uzlsmsunn7d9ppldvcxxa5an9r5ywek8330fg"



-- Samples


smaugPoolId : Bytes Pool.Id
smaugPoolId =
    Bytes.fromHexUnchecked "abacadaba9f12a8b5382fc370e4e7e69421fb59831bb4ecca3a11d9b"
