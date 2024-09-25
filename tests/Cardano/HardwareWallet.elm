module Cardano.HardwareWallet exposing (suite)

import Bytes.Comparable as Bytes
import Bytes.Map exposing (BytesMap)
import Cardano.Address as Address exposing (Credential(..), NetworkId(..))
import Cardano.Transaction as Transaction exposing (Certificate(..), newBody, newWitnessSet)
import Cardano.Value as Value
import Dict exposing (Dict)
import Expect
import Integer
import Natural as N exposing (Natural)
import Test exposing (Test, describe, test)


suite : Test
suite =
    -- Verification of validity can be done with cardano-hw-cli
    -- https://github.com/vacuumlabs/cardano-hw-cli
    describe "Transaction encoding must be Hardware Wallet (HW) compliant (CIP-21)"
        [ test "Simple transaction" <|
            \_ ->
                let
                    expectedEncoding =
                        "84a40081825820bc8bf52ea894fb8e442fe3eea628be87d0c9a37baef185b70eb00a5c8a849d3b000181a20058390180f9e2c88e6c817008f3a812ed889b4a4da8e0bd103f86e7335422aa122a946b9ad3d2ddf029d3a828f0468aece76895f15c9efbd69b4277011a0023583c021a00029b75031a01a3bd8fa0f5f6"

                    tx =
                        { auxiliaryData = Nothing
                        , body =
                            { newBody
                                | auxiliaryDataHash = Nothing
                                , fee = Just (N.fromSafeInt 170869)
                                , inputs = [ { outputIndex = 0, transactionId = Bytes.fromStringUnchecked "bc8bf52ea894fb8e442fe3eea628be87d0c9a37baef185b70eb00a5c8a849d3b" } ]
                                , outputs =
                                    [ { address =
                                            Address.base Mainnet
                                                (VKeyHash (Bytes.fromStringUnchecked "80f9e2c88e6c817008f3a812ed889b4a4da8e0bd103f86e7335422aa"))
                                                (VKeyHash (Bytes.fromStringUnchecked "122a946b9ad3d2ddf029d3a828f0468aece76895f15c9efbd69b4277"))
                                      , amount = Value.onlyLovelace (N.fromSafeInt 2316348)
                                      , datumOption = Nothing
                                      , referenceScript = Nothing
                                      }
                                    ]
                                , ttl = Just (N.fromSafeInt 27508111)
                            }
                        , isValid = True
                        , witnessSet = newWitnessSet
                        }
                in
                Transaction.serialize tx
                    |> Expect.equal (Bytes.fromStringUnchecked expectedEncoding)
        , test "Complex transaction" <|
            \_ ->
                let
                    expectedEncoding =
                        "84a60082825820169422f7193e3418318c2420590778e68619119403472f70c0bb9e9feb2b457100825820cba5f1dd03010380d5c1a6471e7223ac48a7baf75c76e3824896d4398fe0155e000183a2005839306665c42b15b35c7937381bd545c5e7b6b3a03a24cf0383d409ac4583381f757b787201d66ae47603d1abd06ceaa031188e923568c937e8bc01821a27aa98ffa1581c13a36080b2263de3bf122d69f680eff37f8f640dac951e6048abd664a1444b6f6a6e1a000927c0a200583930de685e72586c4269087e282c9c7e78ba22082bce4a674977b4000e99b494d35f236093e7caed75d2b99b1e523cde935a6f4a2d276b9fb40101821a27aa98ffa1581c13a36080b2263de3bf122d69f680eff37f8f640dac951e6048abd664a1444b6f6a6e1a00061a80a2005839000743d16cfe3c4fcc0c11c2403bbc10dbc7ecdd4477e053481a368e7a06e2ae44dff6770dc0f4ada3cf4cf2605008e27aecdb332ad349fda7011a27aa98fe021a0003ba51048182018201581cb494d35f236093e7caed75d2b99b1e523cde935a6f4a2d276b9fb40105a1581df0381f757b787201d66ae47603d1abd06ceaa031188e923568c937e8bc0009a1581c13a36080b2263de3bf122d69f680eff37f8f640dac951e6048abd664a1444b6f6a6e1a000f4240a10081825820abd0f26723a5de57c10eb483b14c0aec1c365d911d46ab38684c2b9b2fa4a4915840f2b04185587ed5af88cac6778b0a8392f1cd4d51e6c3722d96db62cae9d716f2d71a22aac6bde7ec097e1357b9e2ffa70eb9ab5d757d24180c843593fb302f09f5f6"

                    tx =
                        { auxiliaryData = Nothing
                        , body =
                            { newBody
                                | certificates = [ StakeDeregistration { delegator = ScriptHash (Bytes.fromStringUnchecked "b494d35f236093e7caed75d2b99b1e523cde935a6f4a2d276b9fb401") } ]
                                , fee = Just (N.fromSafeInt 244305)
                                , inputs =
                                    [ { outputIndex = 0, transactionId = Bytes.fromStringUnchecked "169422f7193e3418318c2420590778e68619119403472f70c0bb9e9feb2b4571" }
                                    , { outputIndex = 0, transactionId = Bytes.fromStringUnchecked "cba5f1dd03010380d5c1a6471e7223ac48a7baf75c76e3824896d4398fe0155e" }
                                    ]
                                , mint = bytesMap (Dict.fromList [ ( "13a36080b2263de3bf122d69f680eff37f8f640dac951e6048abd664", bytesMap (Dict.fromList [ ( "4b6f6a6e", Integer.fromSafeInt 1000000 ) ]) ) ])
                                , outputs =
                                    [ { address =
                                            Address.base Testnet
                                                (ScriptHash (Bytes.fromStringUnchecked "6665c42b15b35c7937381bd545c5e7b6b3a03a24cf0383d409ac4583"))
                                                (ScriptHash (Bytes.fromStringUnchecked "381f757b787201d66ae47603d1abd06ceaa031188e923568c937e8bc"))
                                      , amount =
                                            { assets = bytesMap (Dict.fromList [ ( "13a36080b2263de3bf122d69f680eff37f8f640dac951e6048abd664", bytesMap (Dict.fromList [ ( "4b6f6a6e", N.fromSafeInt 600000 ) ]) ) ])
                                            , lovelace = bigNat [ 61511935, 9 ]
                                            }
                                      , datumOption = Nothing
                                      , referenceScript = Nothing
                                      }
                                    , { address =
                                            Address.base Testnet
                                                (ScriptHash (Bytes.fromStringUnchecked "de685e72586c4269087e282c9c7e78ba22082bce4a674977b4000e99"))
                                                (ScriptHash (Bytes.fromStringUnchecked "b494d35f236093e7caed75d2b99b1e523cde935a6f4a2d276b9fb401"))
                                      , amount =
                                            { assets = bytesMap (Dict.fromList [ ( "13a36080b2263de3bf122d69f680eff37f8f640dac951e6048abd664", bytesMap (Dict.fromList [ ( "4b6f6a6e", N.fromSafeInt 400000 ) ]) ) ])
                                            , lovelace = bigNat [ 61511935, 9 ]
                                            }
                                      , datumOption = Nothing
                                      , referenceScript = Nothing
                                      }
                                    , { address =
                                            Address.base Testnet
                                                (VKeyHash (Bytes.fromStringUnchecked "0743d16cfe3c4fcc0c11c2403bbc10dbc7ecdd4477e053481a368e7a"))
                                                (VKeyHash (Bytes.fromStringUnchecked "06e2ae44dff6770dc0f4ada3cf4cf2605008e27aecdb332ad349fda7"))
                                      , amount = { assets = bytesMap Dict.empty, lovelace = bigNat [ 61511934, 9 ] }
                                      , datumOption = Nothing
                                      , referenceScript = Nothing
                                      }
                                    ]
                                , withdrawals = [ ( { networkId = Testnet, stakeCredential = ScriptHash (Bytes.fromStringUnchecked "381f757b787201d66ae47603d1abd06ceaa031188e923568c937e8bc") }, N.zero ) ]
                            }
                        , isValid = True
                        , witnessSet =
                            { newWitnessSet
                                | vkeywitness =
                                    Just
                                        [ { signature = Bytes.fromStringUnchecked "f2b04185587ed5af88cac6778b0a8392f1cd4d51e6c3722d96db62cae9d716f2d71a22aac6bde7ec097e1357b9e2ffa70eb9ab5d757d24180c843593fb302f09"
                                          , vkey = Bytes.fromStringUnchecked "abd0f26723a5de57c10eb483b14c0aec1c365d911d46ab38684c2b9b2fa4a491"
                                          }
                                        ]
                            }
                        }
                in
                Transaction.serialize tx
                    |> Expect.equal (Bytes.fromStringUnchecked expectedEncoding)
        ]



-- Helpers


{-| Convert the internal representation of Natural, using a base 2^26, back into a Natural.
-}
bigNat : List Int -> Natural
bigNat xs =
    let
        step x ( n, base ) =
            ( N.add n (N.mul base (N.fromSafeInt x))
              -- base * 2**26
            , N.mul base (N.fromSafeInt 0x04000000)
            )
    in
    List.foldl step ( N.zero, N.one ) xs
        |> Tuple.first


bytesMap : Dict String v -> BytesMap k v
bytesMap keyValues =
    Dict.toList keyValues
        |> List.map (\( k, v ) -> ( Bytes.fromStringUnchecked k, v ))
        |> Bytes.Map.fromList
