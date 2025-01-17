module Cardano.AddressTests exposing (suite)

import Bytes.Comparable as Bytes
import Cardano.Address as Address exposing (Address, Credential(..), NetworkId(..))
import Expect exposing (Expectation)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Address"
        -- Bech32 decoding tests
        [ describe "Bech32 decoding"
            [ test "empty string" emptyStringBech32DecodingTest
            , test "addr_test1qpwax...seyy6gp" testAddress1Bech32DecodingTest
            ]

        -- Bech32 encoding tests
        , describe "Bech32 encoding"
            [ test "addr_test1qpwax...seyy6gp" testAddress1Bech32EncodingTest
            ]
        ]



-- Bech32 decoding


emptyStringBech32DecodingTest : () -> Expectation
emptyStringBech32DecodingTest _ =
    Address.fromBech32 ""
        |> Expect.equal Nothing


testAddress1Bech32DecodingTest : () -> Expectation
testAddress1Bech32DecodingTest _ =
    Address.fromBech32 "addr_test1qpwaxl30e4u0a7pjqrtytjjvum3s2tpee8l7q3td6fl8wflw8uenkdusnell2z22cck2daugr7af86pa9y3xltwdksuseyy6gp"
        |> Expect.equal (Just testAddr1)



-- Bech32 encoding


testAddress1Bech32EncodingTest : () -> Expectation
testAddress1Bech32EncodingTest _ =
    Address.toBech32 testAddr1
        |> Expect.equal "addr_test1qpwaxl30e4u0a7pjqrtytjjvum3s2tpee8l7q3td6fl8wflw8uenkdusnell2z22cck2daugr7af86pa9y3xltwdksuseyy6gp"



-- Sample addresses


testAddr1 : Address
testAddr1 =
    Address.base Testnet
        (VKeyHash <| Bytes.fromHexUnchecked "5dd37e2fcd78fef83200d645ca4ce6e3052c39c9ffe0456dd27e7727")
        (VKeyHash <| Bytes.fromHexUnchecked "ee3f333b37909e7ff5094ac62ca6f7881fba93e83d29226fadcdb439")
