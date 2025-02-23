module Cardano.GovTests exposing (suite)

import Bytes.Comparable as Bytes
import Cardano.Address exposing (Credential(..))
import Cardano.Gov as Gov
import Expect exposing (Expectation)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Gov"
        -- Bech32 decoding tests
        [ describe "Gov Id Bech32 decoding"
            [ test "Smaug Pool" smaugBech32DecodingTest
            , test "Gov action id" govActionIdBech32DecodingTest
            , test "Cc hot" ccHotBech32DecodingTest
            , test "Cc cold" ccColdBech32DecodingTest
            , test "DRep" drepBech32DecodingTest
            ]

        -- Bech32 encoding tests
        , describe "Gov Id Bech32 encoding"
            [ test "Smaug Pool" smaugBech32EncodingTest
            , test "Gov action id" govActionIdBech32EncodingTest
            , test "Cc hot" ccHotBech32EncodingTest
            , test "Cc cold" ccColdBech32EncodingTest
            , test "DRep" drepBech32EncodingTest
            ]
        ]



-- Bech32 decoding


smaugBech32DecodingTest : () -> Expectation
smaugBech32DecodingTest _ =
    Gov.idFromBech32 "pool14wk2m2af7y4gk5uzlsmsunn7d9ppldvcxxa5an9r5ywek8330fg"
        |> Expect.equal (Just smaugPoolGovId)


govActionIdBech32DecodingTest : () -> Expectation
govActionIdBech32DecodingTest _ =
    Gov.idFromBech32 "gov_action1qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqpzklpgpf"
        |> Expect.equal (Just sampleGovActionId)


ccHotBech32DecodingTest : () -> Expectation
ccHotBech32DecodingTest _ =
    Gov.idFromBech32 "cc_hot1qgqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqvcdjk7"
        |> Expect.equal (Just sampleCcHot)


ccColdBech32DecodingTest : () -> Expectation
ccColdBech32DecodingTest _ =
    Gov.idFromBech32 "cc_cold1zvqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq6kflvs"
        |> Expect.equal (Just sampleCcCold)


drepBech32DecodingTest : () -> Expectation
drepBech32DecodingTest _ =
    Gov.idFromBech32 "drep1ygqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq7vlc9n"
        |> Expect.equal (Just sampleDrep)



-- Bech32 encoding


smaugBech32EncodingTest : () -> Expectation
smaugBech32EncodingTest _ =
    Gov.idToBech32 smaugPoolGovId
        |> Expect.equal "pool14wk2m2af7y4gk5uzlsmsunn7d9ppldvcxxa5an9r5ywek8330fg"


govActionIdBech32EncodingTest : () -> Expectation
govActionIdBech32EncodingTest _ =
    Gov.idToBech32 sampleGovActionId
        |> Expect.equal "gov_action1qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqpzklpgpf"


ccHotBech32EncodingTest : () -> Expectation
ccHotBech32EncodingTest _ =
    Gov.idToBech32 sampleCcHot
        |> Expect.equal "cc_hot1qgqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqvcdjk7"


ccColdBech32EncodingTest : () -> Expectation
ccColdBech32EncodingTest _ =
    Gov.idToBech32 sampleCcCold
        |> Expect.equal "cc_cold1zvqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq6kflvs"


drepBech32EncodingTest : () -> Expectation
drepBech32EncodingTest _ =
    Gov.idToBech32 sampleDrep
        |> Expect.equal "drep1ygqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq7vlc9n"



-- Samples


smaugPoolGovId : Gov.Id
smaugPoolGovId =
    Gov.PoolId <| Bytes.fromHexUnchecked "abacadaba9f12a8b5382fc370e4e7e69421fb59831bb4ecca3a11d9b"


sampleGovActionId : Gov.Id
sampleGovActionId =
    Gov.GovActionId
        { transactionId = Bytes.fromHexUnchecked "0000000000000000000000000000000000000000000000000000000000000000"
        , govActionIndex = 17
        }


sampleCcHot : Gov.Id
sampleCcHot =
    Gov.CcHotCredId <| VKeyHash <| Bytes.fromHexUnchecked "00000000000000000000000000000000000000000000000000000000"


sampleCcCold : Gov.Id
sampleCcCold =
    Gov.CcColdCredId <| ScriptHash <| Bytes.fromHexUnchecked "00000000000000000000000000000000000000000000000000000000"


sampleDrep : Gov.Id
sampleDrep =
    Gov.DrepId <| VKeyHash <| Bytes.fromHexUnchecked "00000000000000000000000000000000000000000000000000000000"
