module Cardano.HardwareWallet exposing (suite)

import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Map exposing (BytesMap)
import Cardano exposing (SpendSource(..), TxIntent(..), WitnessSource(..), finalize)
import Cardano.Address as Address exposing (Address, Credential(..), CredentialHash, NetworkId(..), StakeCredential(..))
import Cardano.Data as Data
import Cardano.Script as Script exposing (PlutusVersion(..))
import Cardano.Transaction as Transaction exposing (Certificate(..), newBody, newWitnessSet)
import Cardano.Utxo as Utxo exposing (DatumOption(..), Output, OutputReference)
import Cardano.Value as Value
import Dict exposing (Dict)
import Dict.Any
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
                                , fee = N.fromSafeInt 170869
                                , inputs = [ { outputIndex = 0, transactionId = Bytes.fromHexUnchecked "bc8bf52ea894fb8e442fe3eea628be87d0c9a37baef185b70eb00a5c8a849d3b" } ]
                                , outputs =
                                    [ { address =
                                            Address.base Mainnet
                                                (VKeyHash (Bytes.fromHexUnchecked "80f9e2c88e6c817008f3a812ed889b4a4da8e0bd103f86e7335422aa"))
                                                (VKeyHash (Bytes.fromHexUnchecked "122a946b9ad3d2ddf029d3a828f0468aece76895f15c9efbd69b4277"))
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
                    |> Expect.equal (Bytes.fromHexUnchecked expectedEncoding)
        , test "Complex transaction" <|
            \_ ->
                let
                    expectedEncoding =
                        "84a60082825820169422f7193e3418318c2420590778e68619119403472f70c0bb9e9feb2b457100825820cba5f1dd03010380d5c1a6471e7223ac48a7baf75c76e3824896d4398fe0155e000183a2005839306665c42b15b35c7937381bd545c5e7b6b3a03a24cf0383d409ac4583381f757b787201d66ae47603d1abd06ceaa031188e923568c937e8bc01821a27aa98ffa1581c13a36080b2263de3bf122d69f680eff37f8f640dac951e6048abd664a1444b6f6a6e1a000927c0a200583930de685e72586c4269087e282c9c7e78ba22082bce4a674977b4000e99b494d35f236093e7caed75d2b99b1e523cde935a6f4a2d276b9fb40101821a27aa98ffa1581c13a36080b2263de3bf122d69f680eff37f8f640dac951e6048abd664a1444b6f6a6e1a00061a80a2005839000743d16cfe3c4fcc0c11c2403bbc10dbc7ecdd4477e053481a368e7a06e2ae44dff6770dc0f4ada3cf4cf2605008e27aecdb332ad349fda7011a27aa98fe021a0003ba51048182018201581cb494d35f236093e7caed75d2b99b1e523cde935a6f4a2d276b9fb40105a1581df0381f757b787201d66ae47603d1abd06ceaa031188e923568c937e8bc0009a1581c13a36080b2263de3bf122d69f680eff37f8f640dac951e6048abd664a1444b6f6a6e1a000f4240a10081825820abd0f26723a5de57c10eb483b14c0aec1c365d911d46ab38684c2b9b2fa4a4915840f2b04185587ed5af88cac6778b0a8392f1cd4d51e6c3722d96db62cae9d716f2d71a22aac6bde7ec097e1357b9e2ffa70eb9ab5d757d24180c843593fb302f09f5f6"

                    tx =
                        { auxiliaryData = Nothing
                        , body =
                            { newBody
                                | certificates = [ StakeDeregistrationCert { delegator = ScriptHash (Bytes.fromHexUnchecked "b494d35f236093e7caed75d2b99b1e523cde935a6f4a2d276b9fb401") } ]
                                , fee = N.fromSafeInt 244305
                                , inputs =
                                    [ { outputIndex = 0, transactionId = Bytes.fromHexUnchecked "169422f7193e3418318c2420590778e68619119403472f70c0bb9e9feb2b4571" }
                                    , { outputIndex = 0, transactionId = Bytes.fromHexUnchecked "cba5f1dd03010380d5c1a6471e7223ac48a7baf75c76e3824896d4398fe0155e" }
                                    ]
                                , mint = bytesMap (Dict.fromList [ ( "13a36080b2263de3bf122d69f680eff37f8f640dac951e6048abd664", bytesMap (Dict.fromList [ ( "4b6f6a6e", Integer.fromSafeInt 1000000 ) ]) ) ])
                                , outputs =
                                    [ { address =
                                            Address.base Testnet
                                                (ScriptHash (Bytes.fromHexUnchecked "6665c42b15b35c7937381bd545c5e7b6b3a03a24cf0383d409ac4583"))
                                                (ScriptHash (Bytes.fromHexUnchecked "381f757b787201d66ae47603d1abd06ceaa031188e923568c937e8bc"))
                                      , amount =
                                            { assets = bytesMap (Dict.fromList [ ( "13a36080b2263de3bf122d69f680eff37f8f640dac951e6048abd664", bytesMap (Dict.fromList [ ( "4b6f6a6e", N.fromSafeInt 600000 ) ]) ) ])
                                            , lovelace = bigNat [ 61511935, 9 ]
                                            }
                                      , datumOption = Nothing
                                      , referenceScript = Nothing
                                      }
                                    , { address =
                                            Address.base Testnet
                                                (ScriptHash (Bytes.fromHexUnchecked "de685e72586c4269087e282c9c7e78ba22082bce4a674977b4000e99"))
                                                (ScriptHash (Bytes.fromHexUnchecked "b494d35f236093e7caed75d2b99b1e523cde935a6f4a2d276b9fb401"))
                                      , amount =
                                            { assets = bytesMap (Dict.fromList [ ( "13a36080b2263de3bf122d69f680eff37f8f640dac951e6048abd664", bytesMap (Dict.fromList [ ( "4b6f6a6e", N.fromSafeInt 400000 ) ]) ) ])
                                            , lovelace = bigNat [ 61511935, 9 ]
                                            }
                                      , datumOption = Nothing
                                      , referenceScript = Nothing
                                      }
                                    , { address =
                                            Address.base Testnet
                                                (VKeyHash (Bytes.fromHexUnchecked "0743d16cfe3c4fcc0c11c2403bbc10dbc7ecdd4477e053481a368e7a"))
                                                (VKeyHash (Bytes.fromHexUnchecked "06e2ae44dff6770dc0f4ada3cf4cf2605008e27aecdb332ad349fda7"))
                                      , amount = { assets = bytesMap Dict.empty, lovelace = bigNat [ 61511934, 9 ] }
                                      , datumOption = Nothing
                                      , referenceScript = Nothing
                                      }
                                    ]
                                , withdrawals = [ ( { networkId = Testnet, stakeCredential = ScriptHash (Bytes.fromHexUnchecked "381f757b787201d66ae47603d1abd06ceaa031188e923568c937e8bc") }, N.zero ) ]
                            }
                        , isValid = True
                        , witnessSet =
                            { newWitnessSet
                                | vkeywitness =
                                    Just
                                        [ { signature = Bytes.fromHexUnchecked "f2b04185587ed5af88cac6778b0a8392f1cd4d51e6c3722d96db62cae9d716f2d71a22aac6bde7ec097e1357b9e2ffa70eb9ab5d757d24180c843593fb302f09"
                                          , vkey = Bytes.fromHexUnchecked "abd0f26723a5de57c10eb483b14c0aec1c365d911d46ab38684c2b9b2fa4a491"
                                          }
                                        ]
                            }
                        }
                in
                Transaction.serialize tx
                    |> Expect.equal (Bytes.fromHexUnchecked expectedEncoding)
        , test "Tx with tricky asset order" <|
            \_ ->
                let
                    expectedEncoding =
                        "84a500818258205ecbc2e3779c88ef83193e0788bd0ca3ee8e839695736ca2bd1a0d5d17c74d66000181a20058390074976c54afaf444f7cd499bd8519aac6592b13b22b9d5817f0da5c5203d205532089ad2f7816892e2ef42849b7b52788e41b3fd43a6e01cf01821b000000e8d4c51c9ba1581c2e2f143b3ccbe339145183dc2a799a469e92ab56e0d5b0bd04f54f15a34018644568616c6c6f19012c4611223322110018c8021a0002cc9d031a029b6d6f09a1581c2e2f143b3ccbe339145183dc2a799a469e92ab56e0d5b0bd04f54f15a14568616c6c6f19012ca0f5f6"

                    tx =
                        { auxiliaryData = Nothing
                        , body =
                            { newBody
                                | auxiliaryDataHash = Nothing
                                , fee = N.fromSafeInt 183453
                                , inputs =
                                    [ { outputIndex = 0
                                      , transactionId = Bytes.fromHexUnchecked "5ecbc2e3779c88ef83193e0788bd0ca3ee8e839695736ca2bd1a0d5d17c74d66"
                                      }
                                    ]
                                , mint = bytesMap (Dict.fromList [ ( "2e2f143b3ccbe339145183dc2a799a469e92ab56e0d5b0bd04f54f15", bytesMap (Dict.fromList [ ( "68616c6c6f", Integer.fromSafeInt 300 ) ]) ) ])
                                , outputs =
                                    [ { address =
                                            Address.base Testnet
                                                (VKeyHash (Bytes.fromHexUnchecked "74976c54afaf444f7cd499bd8519aac6592b13b22b9d5817f0da5c52"))
                                                (VKeyHash (Bytes.fromHexUnchecked "03d205532089ad2f7816892e2ef42849b7b52788e41b3fd43a6e01cf"))
                                      , amount =
                                            { assets = bytesMap (Dict.fromList [ ( "2e2f143b3ccbe339145183dc2a799a469e92ab56e0d5b0bd04f54f15", bytesMap (Dict.fromList [ ( "", N.fromSafeInt 100 ), ( "112233221100", N.fromSafeInt 200 ), ( "68616c6c6f", N.fromSafeInt 300 ) ]) ) ])
                                            , lovelace = bigNat [ 12917915, 14901 ]
                                            }
                                      , datumOption = Nothing
                                      , referenceScript = Nothing
                                      }
                                    ]
                                , ttl = Just (N.fromSafeInt 43740527)
                            }
                        , isValid = True
                        , witnessSet = newWitnessSet
                        }
                in
                Transaction.serialize tx
                    |> Expect.equal (Bytes.fromHexUnchecked expectedEncoding)
        , test "Tx with plutus script" <|
            \_ ->
                let
                    ada =
                        -- Asset amounts are typed with unbounded Natural numbers
                        { one = Value.onlyLovelace (N.fromSafeString "1000000")
                        , two = Value.onlyLovelace (N.fromSafeString "2000000")
                        , four = Value.onlyLovelace (N.fromSafeString "4000000")
                        , ten = Value.onlyLovelace (N.fromSafeString "10000000")
                        }

                    globalStateUtxos =
                        Utxo.refDictFromList
                            [ makeAdaOutput 0 me 2 --   2 ada at my address
                            , makeAdaOutput 1 me 10 -- 10 ada at my address
                            , makeAdaOutput 2 me 5 --   5 ada at my address
                            ]

                    me =
                        makeWalletAddress "me"

                    utxoBeingSpent =
                        makeRef "previouslySentToLock" 0

                    findSpendingUtxo inputs =
                        case inputs of
                            [] ->
                                0

                            ( id, ref ) :: next ->
                                if ref == utxoBeingSpent then
                                    id

                                else
                                    findSpendingUtxo next

                    ( myKeyCred, myStakeCred ) =
                        ( Address.extractPubKeyHash me
                            |> Maybe.withDefault (Bytes.fromText "should not fail")
                        , Address.extractStakeCredential me
                        )

                    -- Lock script made with Aiken
                    lock =
                        { script = Script.plutusScriptFromBytes PlutusV3 (Bytes.fromHexUnchecked "58b501010032323232323225333002323232323253330073370e900118041baa0011323232533300a3370e900018059baa00113322323300100100322533301100114a0264a66601e66e3cdd718098010020a5113300300300130130013758601c601e601e601e601e601e601e601e601e60186ea801cdd7180718061baa00116300d300e002300c001300937540022c6014601600460120026012004600e00260086ea8004526136565734aae7555cf2ab9f5742ae881")
                        , scriptHash = Bytes.fromHexUnchecked "3ff0b1bb5815347c6f0c05328556d80c1f83ca47ac410d25ffb4a330"
                        }

                    -- Combining the script hash with our stake credential
                    -- to keep the locked ada staked.
                    lockScriptAddress =
                        Address.Shelley
                            { networkId = Mainnet
                            , paymentCredential = ScriptHash lock.scriptHash
                            , stakeCredential = myStakeCred
                            }

                    -- Build a redeemer that contains the index of the spent script input.
                    redeemer txBody =
                        List.indexedMap Tuple.pair txBody.inputs
                            |> findSpendingUtxo
                            |> (Data.Int << Integer.fromSafeInt)

                    -- Helper function to create an output at the lock script address.
                    -- It contains our key credential in the datum.
                    makeLockedOutput adaAmount =
                        { address = lockScriptAddress
                        , amount = adaAmount
                        , datumOption = Just (DatumValue (Data.Bytes <| Bytes.toAny myKeyCred))
                        , referenceScript = Nothing
                        }

                    -- Add to local state utxos some previously sent 4 ada.
                    localStateUtxos =
                        globalStateUtxos
                            |> Dict.Any.insert utxoBeingSpent (makeLockedOutput ada.four)

                    txFinalizedResult =
                        -- Collect 2 ada from the lock script
                        [ Spend <|
                            FromPlutusScript
                                { spentInput = utxoBeingSpent
                                , datumWitness = Nothing
                                , plutusScriptWitness =
                                    { script = ( Script.plutusVersion lock.script, WitnessByValue <| Script.cborWrappedBytes lock.script )
                                    , redeemerData = redeemer
                                    , requiredSigners = [ myKeyCred ]
                                    }
                                }
                        , SendTo me ada.two

                        -- Return the other 2 ada to the lock script (there was 4 ada initially)
                        , SendToOutput (makeLockedOutput ada.two)
                        ]
                            |> finalize localStateUtxos []

                    expectedEncoding =
                        "84a8008282582031303030303030303030303030303030303030303030303030303030303030300182582070726576696f75736c7953656e74546f4c6f636b303030303030303030303030000182a3005839113ff0b1bb5815347c6f0c05328556d80c1f83ca47ac410d25ffb4a3306d653030303030303030303030303030303030303030303030303030011a001e8480028201d818581e581c6d653030303030303030303030303030303030303030303030303030a2005839016d6530303030303030303030303030303030303030303030303030306d653030303030303030303030303030303030303030303030303030011a00b435dd021a0002e5230b5820c919321018dd46f33c279f39431d5723e50f555170a41c32bb8e2e2d796ca40a0d818258203030303030303030303030303030303030303030303030303030303030303030000e81581c6d65303030303030303030303030303030303030303030303030303010a2005839016d6530303030303030303030303030303030303030303030303030306d653030303030303030303030303030303030303030303030303030011a001a2ccb111a000457b5a205a18200018201821951171a0061b0d9078158b758b501010032323232323225333002323232323253330073370e900118041baa0011323232533300a3370e900018059baa00113322323300100100322533301100114a0264a66601e66e3cdd718098010020a5113300300300130130013758601c601e601e601e601e601e601e601e601e60186ea801cdd7180718061baa00116300d300e002300c001300937540022c6014601600460120026012004600e00260086ea8004526136565734aae7555cf2ab9f5742ae881f5f6"
                in
                Result.map (\{ tx } -> Transaction.serialize tx) txFinalizedResult
                    |> Expect.equal (Ok (Bytes.fromHexUnchecked expectedEncoding))
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
        |> List.map (\( k, v ) -> ( Bytes.fromHexUnchecked k, v ))
        |> Bytes.Map.fromList



-- Helper functions to build stuff


dummyCredentialHash : String -> Bytes CredentialHash
dummyCredentialHash str =
    Bytes.dummy 28 str


makeWalletAddress : String -> Address
makeWalletAddress name =
    Address.Shelley
        { networkId = Mainnet
        , paymentCredential = VKeyHash (dummyCredentialHash name)
        , stakeCredential = Just (InlineCredential (VKeyHash <| dummyCredentialHash name))
        }


makeRef : String -> Int -> OutputReference
makeRef id index =
    { transactionId = Bytes.dummy 32 id
    , outputIndex = index
    }


makeAdaOutput : Int -> Address -> Int -> ( OutputReference, Output )
makeAdaOutput index address amount =
    ( makeRef (String.fromInt index) index
    , Utxo.fromLovelace address (N.fromSafeInt <| 1000000 * amount)
    )
