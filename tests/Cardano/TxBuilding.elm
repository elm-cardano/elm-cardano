module Cardano.TxBuilding exposing (suite)

import Blake2b exposing (blake2b224)
import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Map as Map
import Cardano exposing (ActionProposal(..), CertificateIntent(..), CredentialWitness(..), Fee(..), GovernanceState, ScriptWitness(..), SpendSource(..), TxFinalizationError(..), TxIntent(..), TxOtherInfo(..), VoterWitness(..), WitnessSource(..), dummyBytes, finalizeAdvanced)
import Cardano.Address as Address exposing (Address, Credential(..), CredentialHash, NetworkId(..), StakeCredential(..))
import Cardano.CoinSelection as CoinSelection exposing (Error(..))
import Cardano.Data as Data
import Cardano.Gov as Gov exposing (Drep(..), Vote(..), Voter(..), noParamUpdate)
import Cardano.Metadatum as Metadatum
import Cardano.MultiAsset as MultiAsset
import Cardano.Redeemer exposing (Redeemer)
import Cardano.Script as Script exposing (NativeScript(..), PlutusVersion(..))
import Cardano.Transaction as Transaction exposing (Certificate(..), Transaction, newBody, newWitnessSet)
import Cardano.Uplc as Uplc
import Cardano.Utxo as Utxo exposing (DatumOption(..), Output, OutputReference)
import Cardano.Value as Value exposing (Value)
import Cbor.Encode as E
import Expect exposing (Expectation)
import Integer
import Natural exposing (Natural)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Cardano Tx building"
        [ okTxBuilding
        , failTxBuilding
        ]


okTxBuilding : Test
okTxBuilding =
    describe "Successfull"
        [ okTxTest "with just manual fees"
            { govState = Cardano.emptyGovernanceState
            , localStateUtxos = [ makeAdaOutput 0 testAddr.me 2 ]
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents = []
            }
            (\_ ->
                { newTx
                    | body =
                        { newBody
                            | fee = ada 2
                            , inputs = [ makeRef "0" 0 ]
                        }
                    , witnessSet =
                        { newWitnessSet
                            | vkeywitness = Just [ { vkey = dummyBytes 32 "VKEYkey-me", signature = dummyBytes 64 "SIGNATUREkey-me" } ]
                        }
                }
            )
        , okTxTest "with just auto fees"
            { govState = Cardano.emptyGovernanceState
            , localStateUtxos = [ makeAdaOutput 0 testAddr.me 2 ]
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = autoFee
            , txOtherInfo = []
            , txIntents = []
            }
            (\tx ->
                let
                    feeAmount =
                        Transaction.computeFees Transaction.defaultTxFeeParams { refScriptBytes = 0 } tx
                            |> (\{ txSizeFee, scriptExecFee, refScriptSizeFee } ->
                                    Natural.add txSizeFee scriptExecFee |> Natural.add refScriptSizeFee
                               )

                    adaLeft =
                        Natural.sub (ada 2) feeAmount
                in
                { newTx
                    | body =
                        { newBody
                            | fee = feeAmount
                            , inputs = [ makeRef "0" 0 ]
                            , outputs = [ Utxo.fromLovelace testAddr.me adaLeft ]
                        }
                    , witnessSet =
                        { newWitnessSet
                            | vkeywitness = Just [ { vkey = dummyBytes 32 "VKEYkey-me", signature = dummyBytes 64 "SIGNATUREkey-me" } ]
                        }
                }
            )
        , okTxTest "with spending from, and sending to the same address"
            { govState = Cardano.emptyGovernanceState
            , localStateUtxos = [ makeAdaOutput 0 testAddr.me 5 ]
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents =
                [ Spend <| FromWallet testAddr.me (Value.onlyLovelace <| ada 1)
                , SendTo testAddr.me (Value.onlyLovelace <| ada 1)
                ]
            }
            (\_ ->
                { newTx
                    | body =
                        { newBody
                            | fee = ada 2
                            , inputs = [ makeRef "0" 0 ]
                            , outputs = [ Utxo.fromLovelace testAddr.me (ada 3) ]
                        }
                    , witnessSet =
                        { newWitnessSet
                            | vkeywitness = Just [ { vkey = dummyBytes 32 "VKEYkey-me", signature = dummyBytes 64 "SIGNATUREkey-me" } ]
                        }
                }
            )
        , test "simple finalization is able to find fee source" <|
            \_ ->
                let
                    localStateUtxos =
                        Utxo.refDictFromList [ makeAdaOutput 0 testAddr.me 5 ]

                    txIntents =
                        [ Spend <| FromWallet testAddr.me (Value.onlyLovelace <| ada 1)
                        , SendTo testAddr.me (Value.onlyLovelace <| ada 1)
                        ]
                in
                Expect.ok (Cardano.finalize localStateUtxos [] txIntents)
        , okTxTest "send 1 ada from me to you"
            { govState = Cardano.emptyGovernanceState
            , localStateUtxos = [ makeAdaOutput 0 testAddr.me 5 ]
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents =
                [ Spend <| FromWallet testAddr.me (Value.onlyLovelace <| ada 1)
                , SendTo testAddr.you (Value.onlyLovelace <| ada 1)
                ]
            }
            (\_ ->
                { newTx
                    | body =
                        { newBody
                            | fee = ada 2
                            , inputs = [ makeRef "0" 0 ]
                            , outputs =
                                [ Utxo.fromLovelace testAddr.you (ada 1)
                                , Utxo.fromLovelace testAddr.me (ada 2)
                                ]
                        }
                    , witnessSet =
                        { newWitnessSet
                            | vkeywitness = Just [ { vkey = dummyBytes 32 "VKEYkey-me", signature = dummyBytes 64 "SIGNATUREkey-me" } ]
                        }
                }
            )
        , okTxTest "I pay the fees for your ada transfer to me"
            { govState = Cardano.emptyGovernanceState
            , localStateUtxos =
                [ makeAdaOutput 0 testAddr.me 5
                , makeAdaOutput 1 testAddr.you 7
                ]
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents =
                [ Spend <| FromWallet testAddr.you (Value.onlyLovelace <| ada 1)
                , SendTo testAddr.me (Value.onlyLovelace <| ada 1)
                ]
            }
            (\_ ->
                { newTx
                    | body =
                        { newBody
                            | fee = ada 2
                            , inputs = [ makeRef "0" 0, makeRef "1" 1 ]
                            , outputs =
                                [ Utxo.fromLovelace testAddr.you (ada 6)
                                , Utxo.fromLovelace testAddr.me (ada 4)
                                ]
                        }
                    , witnessSet =
                        { newWitnessSet
                            | vkeywitness =
                                Just
                                    -- Two keys since I pay the fee, and spend your utxo
                                    [ { vkey = dummyBytes 32 "VKEYkey-me", signature = dummyBytes 64 "SIGNATUREkey-me" }
                                    , { vkey = dummyBytes 32 "VKEYkey-you", signature = dummyBytes 64 "SIGNATUREkey-you" }
                                    ]
                        }
                }
            )
        , let
            threeCat =
                Value.onlyToken cat.policyId cat.assetName Natural.three

            threeCatTwoAda =
                { threeCat | lovelace = ada 2 }
          in
          okTxTest "send 3 cat with 2 ada from me to you"
            { govState = Cardano.emptyGovernanceState
            , localStateUtxos =
                [ ( makeRef "0" 0, Utxo.fromLovelace testAddr.me (ada 5) )
                , ( makeRef "1" 1, Utxo.simpleOutput testAddr.me threeCat )
                ]
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents =
                [ Spend <| FromWallet testAddr.me threeCatTwoAda
                , SendTo testAddr.you threeCatTwoAda
                ]
            }
            (\_ ->
                { newTx
                    | body =
                        { newBody
                            | fee = ada 2
                            , inputs = [ makeRef "0" 0, makeRef "1" 1 ]
                            , outputs =
                                [ Utxo.simpleOutput testAddr.you threeCatTwoAda
                                , Utxo.fromLovelace testAddr.me (ada 1)
                                ]
                        }
                    , witnessSet =
                        { newWitnessSet
                            | vkeywitness = Just [ { vkey = dummyBytes 32 "VKEYkey-me", signature = dummyBytes 64 "SIGNATUREkey-me" } ]
                        }
                }
            )
        , let
            threeCat =
                Value.onlyToken cat.policyId cat.assetName Natural.three

            minAda =
                Utxo.minAdaForAssets testAddr.you threeCat.assets

            threeCatMinAda =
                { threeCat | lovelace = minAda }
          in
          okTxTest "send 3 cat with minAda from me to you"
            { govState = Cardano.emptyGovernanceState
            , localStateUtxos =
                [ ( makeRef "0" 0, Utxo.fromLovelace testAddr.me (ada 5) )
                , ( makeRef "1" 1, Utxo.simpleOutput testAddr.me threeCat )
                ]
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents =
                [ Spend <| FromWallet testAddr.me threeCatMinAda
                , SendTo testAddr.you threeCatMinAda
                ]
            }
            (\_ ->
                { newTx
                    | body =
                        { newBody
                            | fee = ada 2
                            , inputs = [ makeRef "0" 0, makeRef "1" 1 ]
                            , outputs =
                                [ Utxo.simpleOutput testAddr.you threeCatMinAda
                                , Utxo.fromLovelace testAddr.me (Natural.sub (ada 3) minAda)
                                ]
                        }
                    , witnessSet =
                        { newWitnessSet
                            | vkeywitness = Just [ { vkey = dummyBytes 32 "VKEYkey-me", signature = dummyBytes 64 "SIGNATUREkey-me" } ]
                        }
                }
            )
        , okTxTest "mint 1 dog and burn 1 cat"
            { govState = Cardano.emptyGovernanceState
            , localStateUtxos =
                [ makeAdaOutput 0 testAddr.me 5
                , makeAsset 1 testAddr.me cat.policyIdStr cat.assetNameStr 3
                , ( dog.scriptRef, dog.refOutput )
                , ( cat.scriptRef, cat.refOutput )
                ]
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents =
                -- minting 1 dog
                [ MintBurn
                    { policyId = dog.policyId
                    , assets = Map.singleton dog.assetName Integer.one
                    , scriptWitness = NativeWitness { script = WitnessReference dog.scriptRef, expectedSigners = [] }
                    }
                , SendTo testAddr.me (Value.onlyToken dog.policyId dog.assetName Natural.one)

                -- burning 1 cat
                , Spend <| FromWallet testAddr.me (Value.onlyToken cat.policyId cat.assetName Natural.one)
                , MintBurn
                    { policyId = cat.policyId
                    , assets = Map.singleton cat.assetName Integer.negativeOne
                    , scriptWitness = NativeWitness { script = WitnessReference cat.scriptRef, expectedSigners = [] }
                    }
                ]
            }
            (\_ ->
                { newTx
                    | body =
                        { newBody
                            | fee = ada 2
                            , inputs = [ makeRef "0" 0, makeRef "1" 1 ]
                            , referenceInputs = [ cat.scriptRef, dog.scriptRef ]
                            , mint =
                                MultiAsset.mintAdd
                                    (MultiAsset.onlyToken dog.policyId dog.assetName Integer.one)
                                    (MultiAsset.onlyToken cat.policyId cat.assetName Integer.negativeOne)
                            , outputs =
                                [ { address = testAddr.me
                                  , amount =
                                        Value.onlyLovelace (ada 3)
                                            -- 1 minted dog
                                            |> Value.add (Value.onlyToken dog.policyId dog.assetName Natural.one)
                                            -- 2 cat left after burning 1 from the utxo with 3 cat
                                            |> Value.add (Value.onlyToken cat.policyId cat.assetName Natural.two)
                                  , datumOption = Nothing
                                  , referenceScript = Nothing
                                  }
                                ]
                        }
                    , witnessSet =
                        { newWitnessSet
                            | vkeywitness = Just [ { vkey = dummyBytes 32 "VKEYkey-me", signature = dummyBytes 64 "SIGNATUREkey-me" } ]
                        }
                }
            )

        -- Test with plutus script spending
        , let
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
                ( Address.extractPubKeyHash testAddr.me
                    |> Maybe.withDefault (Bytes.fromText "should not fail")
                , Address.extractStakeCredential testAddr.me
                )

            -- Lock script made with Aiken
            lock =
                { scriptBytes = Bytes.fromHexUnchecked "58b501010032323232323225333002323232323253330073370e900118041baa0011323232533300a3370e900018059baa00113322323300100100322533301100114a0264a66601e66e3cdd718098010020a5113300300300130130013758601c601e601e601e601e601e601e601e601e60186ea801cdd7180718061baa00116300d300e002300c001300937540022c6014601600460120026012004600e00260086ea8004526136565734aae7555cf2ab9f5742ae881"
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
            redeemer inputsOutputs =
                List.indexedMap Tuple.pair inputsOutputs.spentInputs
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

            localStateUtxos =
                [ makeAdaOutput 0 testAddr.me 5
                , ( utxoBeingSpent, makeLockedOutput <| Value.onlyLovelace <| ada 4 )
                ]
          in
          okTxTest "spend 2 ada from a plutus script holding 4 ada"
            { govState = Cardano.emptyGovernanceState
            , localStateUtxos = localStateUtxos
            , evalScriptsCosts = Uplc.evalScriptsCosts Uplc.defaultVmConfig
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents =
                -- Collect 2 ada from the lock script
                [ Spend <|
                    FromPlutusScript
                        { spentInput = utxoBeingSpent
                        , datumWitness = Nothing
                        , plutusScriptWitness =
                            { script = ( PlutusV3, WitnessValue lock.scriptBytes )
                            , redeemerData = redeemer
                            , requiredSigners = [ myKeyCred ]
                            }
                        }
                , SendTo testAddr.me (Value.onlyLovelace <| ada 2)

                -- Return the other 2 ada to the lock script (there was 4 ada initially)
                , SendToOutput (makeLockedOutput <| Value.onlyLovelace <| ada 2)
                ]
            }
            (\tx ->
                { newTx
                    | body =
                        { newBody
                            | fee = ada 2
                            , inputs = [ makeRef "0" 0, utxoBeingSpent ]
                            , requiredSigners = [ myKeyCred ]
                            , outputs =
                                [ makeLockedOutput <| Value.onlyLovelace <| ada 2
                                , Utxo.fromLovelace testAddr.me (ada 5)
                                ]

                            -- script stuff
                            , scriptDataHash = tx.body.scriptDataHash

                            -- collateral would cost 3 ada for 2 ada fees, so return 5-3=2 ada
                            , collateral = [ makeRef "0" 0 ]
                            , totalCollateral = Just 3000000
                            , collateralReturn = Just (Utxo.fromLovelace testAddr.me (ada 2))
                        }
                    , witnessSet =
                        { newWitnessSet
                            | vkeywitness = Just [ { vkey = dummyBytes 32 "VKEYkey-me", signature = dummyBytes 64 "SIGNATUREkey-me" } ]
                            , plutusV3Script = Just [ lock.scriptBytes ]
                            , redeemer =
                                Uplc.evalScriptsCosts Uplc.defaultVmConfig (Utxo.refDictFromList localStateUtxos) tx
                                    |> Result.toMaybe
                        }
                }
            )

        -- Test with stake registration, pool delegation and drep delegation
        , let
            myStakeKeyHash =
                Address.extractStakeKeyHash testAddr.me
                    |> Maybe.withDefault (dummyCredentialHash "ERROR")
          in
          okTxTest "Test with stake registration, pool delegation and drep delegation"
            { govState = Cardano.emptyGovernanceState
            , localStateUtxos =
                [ ( makeRef "0" 0, Utxo.fromLovelace testAddr.me (ada 5) )
                ]
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents =
                [ Spend <| FromWallet testAddr.me <| Value.onlyLovelace (ada 2) -- 2 ada for the registration deposit
                , IssueCertificate <| RegisterStake { delegator = WithKey myStakeKeyHash, deposit = ada 2 }
                , IssueCertificate <| DelegateStake { delegator = WithKey myStakeKeyHash, poolId = dummyBytes 28 "poolId" }
                , IssueCertificate <| DelegateVotes { delegator = WithKey myStakeKeyHash, drep = VKeyHash <| dummyCredentialHash "drep" }
                ]
            }
            (\_ ->
                { newTx
                    | body =
                        { newBody
                            | fee = ada 2
                            , inputs = [ makeRef "0" 0 ]
                            , outputs = [ Utxo.fromLovelace testAddr.me (ada 1) ]
                            , certificates =
                                [ RegCert { delegator = VKeyHash myStakeKeyHash, deposit = Natural.fromSafeInt 2000000 }
                                , StakeDelegationCert { delegator = VKeyHash myStakeKeyHash, poolId = dummyBytes 28 "poolId" }
                                , VoteDelegCert { delegator = VKeyHash myStakeKeyHash, drep = DrepCredential <| VKeyHash <| dummyCredentialHash "drep" }
                                ]
                        }
                    , witnessSet =
                        { newWitnessSet
                            | vkeywitness =
                                Just
                                    [ { vkey = dummyBytes 32 "VKEYkey-me", signature = dummyBytes 64 "SIGNATUREkey-me" }
                                    , { vkey = dummyBytes 32 "VKEYstk-me", signature = dummyBytes 64 "SIGNATUREstk-me" }
                                    ]
                        }
                }
            )

        -- Test with 6 different proposals
        , let
            myStakeKeyHash =
                Address.extractStakeKeyHash testAddr.me
                    |> Maybe.withDefault (dummyCredentialHash "ERROR")

            myStakeAddress =
                { networkId = Mainnet
                , stakeCredential = VKeyHash myStakeKeyHash
                }

            ada100K =
                ada 100000

            propose govAction offchainInfo =
                Propose
                    { govAction = govAction
                    , offchainInfo = offchainInfo
                    , deposit = ada100K
                    , depositReturnAccount = myStakeAddress
                    }

            -- Current guardrails script info retrieved from the devs docs:
            -- https://developers.cardano.org/docs/get-started/cardano-cli/governance/create%20governance%20actions/#the-guardrails-script
            -- I removed one bytes wrapping of the script cbor so that it works
            guardrailsScriptBytes =
                Bytes.fromHexUnchecked "5908510101003232323232323232323232323232323232323232323232323232323232323232323232323232323232259323255333573466e1d20000011180098111bab357426ae88d55cf00104554ccd5cd19b87480100044600422c6aae74004dd51aba1357446ae88d55cf1baa3255333573466e1d200a35573a002226ae84d5d11aab9e00111637546ae84d5d11aba235573c6ea800642b26006003149a2c8a4c301f801c0052000c00e0070018016006901e4070c00e003000c00d20d00fc000c0003003800a4005801c00e003002c00d20c09a0c80e1801c006001801a4101b5881380018000600700148013003801c006005801a410100078001801c006001801a4101001f8001800060070014801b0038018096007001800600690404002600060001801c0052008c00e006025801c006001801a41209d8001800060070014802b003801c006005801a410112f501c3003800c00300348202b7881300030000c00e00290066007003800c00b003482032ad7b806038403060070014803b00380180960003003800a4021801c00e003002c00d20f40380e1801c006001801a41403f800100a0c00e0029009600f0030078040c00e002900a600f003800c00b003301a483403e01a600700180060066034904801e00060001801c0052016c01e00600f801c006001801980c2402900e30000c00e002901060070030128060c00e00290116007003800c00b003483c0ba03860070018006006906432e00040283003800a40498003003800a404d802c00e00f003800c00b003301a480cb0003003800c003003301a4802b00030001801c01e0070018016006603490605c0160006007001800600660349048276000600030000c00e0029014600b003801c00c04b003800c00300348203a2489b00030001801c00e006025801c006001801a4101b11dc2df80018000c0003003800a4055802c00e007003012c00e003000c00d2080b8b872c000c0006007003801809600700180060069040607e4155016000600030000c00e00290166007003012c00e003000c00d2080c001c000c0003003800a405d801c00e003002c00d20c80180e1801c006001801a412007800100a0c00e00290186007003013c0006007001480cb005801801e006003801800e00600500403003800a4069802c00c00f003001c00c007003803c00e003002c00c05300333023480692028c0004014c00c00b003003c00c00f003003c00e00f003800c00b00301480590052008003003800a406d801c00e003002c00d2000c00d2006c00060070018006006900a600060001801c0052038c00e007001801600690006006901260003003800c003003483281300020141801c005203ac00e006027801c006001801a403d800180006007001480f3003801804e00700180060069040404af3c4e302600060001801c005203ec00e006013801c006001801a4101416f0fd20b80018000600700148103003801c006005801a403501c3003800c0030034812b00030000c00e0029021600f003800c00a01ac00e003000c00ccc08d20d00f4800b00030000c0000000000803c00c016008401e006009801c006001801807e0060298000c000401e006007801c0060018018074020c000400e00f003800c00b003010c000802180020070018006006019801805e0003000400600580180760060138000800c00b00330134805200c400e00300080330004006005801a4001801a410112f58000801c00600901260008019806a40118002007001800600690404a75ee01e00060008018046000801801e000300c4832004c025201430094800a0030028052003002c00d2002c000300648010c0092002300748028c0312000300b48018c0292012300948008c0212066801a40018000c0192008300a2233335573e00250002801994004d55ce800cd55cf0008d5d08014c00cd5d10011263009222532900389800a4d2219002912c80344c01526910c80148964cc04cdd68010034564cc03801400626601800e0071801226601800e01518010096400a3000910c008600444002600244004a664600200244246466004460044460040064600444600200646a660080080066a00600224446600644b20051800484ccc02600244666ae68cdc3801000c00200500a91199ab9a33710004003000801488ccd5cd19b89002001800400a44666ae68cdc4801000c00a00122333573466e20008006005000912a999ab9a3371200400222002220052255333573466e2400800444008440040026eb400a42660080026eb000a4264666015001229002914801c8954ccd5cd19b8700400211333573466e1c00c006001002118011229002914801c88cc044cdc100200099b82002003245200522900391199ab9a3371066e08010004cdc1001001c002004403245200522900391199ab9a3371266e08010004cdc1001001c00a00048a400a45200722333573466e20cdc100200099b820020038014000912c99807001000c40062004912c99807001000c400a2002001199919ab9a357466ae880048cc028dd69aba1003375a6ae84008d5d1000934000dd60010a40064666ae68d5d1800c0020052225933006003357420031330050023574400318010600a444aa666ae68cdc3a400000222c22aa666ae68cdc4000a4000226600666e05200000233702900000088994004cdc2001800ccdc20010008cc010008004c01088954ccd5cd19b87480000044400844cc00c004cdc300100091119803112c800c60012219002911919806912c800c4c02401a442b26600a004019130040018c008002590028c804c8888888800d1900991111111002a244b267201722222222008001000c600518000001112a999ab9a3370e004002230001155333573466e240080044600823002229002914801c88ccd5cd19b893370400800266e0800800e00100208c8c0040048c0088cc008008005"

            -- Add a 600K ada utxo to the local state
            -- for the 6 x 100K deposits + 10 for fees etc.
            localStateUtxos =
                [ ( makeRef "0" 0, Utxo.fromLovelace testAddr.me (ada 600010) ) ]
          in
          okTxTest "Test with 6 different proposals"
            { govState =
                { guardrailsScript =
                    Just
                        { policyId = Bytes.fromHexUnchecked "fa24fb305126805cf2164c161d852a0e7330cf988f1fe558cf7d4a64"
                        , plutusVersion = PlutusV3
                        , scriptWitness = WitnessValue guardrailsScriptBytes
                        }
                , lastEnactedCommitteeAction = Nothing
                , lastEnactedConstitutionAction = Nothing
                , lastEnactedHardForkAction = Nothing
                , lastEnactedProtocolParamUpdateAction = Nothing
                }
            , localStateUtxos = localStateUtxos
            , evalScriptsCosts =
                Uplc.evalScriptsCosts
                    { budget = Uplc.conwayDefaultBudget
                    , slotConfig = Uplc.slotConfigMainnet
                    , costModels = Uplc.conwayDefaultCostModels
                    }
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents =
                [ -- 600K deposit for all the gov actions
                  Spend <| FromWallet testAddr.me <| Value.onlyLovelace (Natural.mul Natural.six ada100K)

                -- Change minPoolCost to 0
                , propose
                    (ParameterChange { noParamUpdate | minPoolCost = Just Natural.zero })
                    { url = "param-url", dataHash = dummyBytes 32 "param-hash-" }

                -- Withdraw 1M ada from the treasury
                , propose
                    (TreasuryWithdrawals [ { destination = myStakeAddress, amount = ada 1000000 } ])
                    { url = "withdraw-url", dataHash = dummyBytes 32 "withdraw-hash-" }

                -- Change the constitution to not have a guardrails script anymore
                , propose
                    (NewConstitution
                        { anchor = { url = "constitution-url", dataHash = dummyBytes 32 "const-hash-" }
                        , scripthash = Nothing
                        }
                    )
                    { url = "new-const-url", dataHash = dummyBytes 32 "new-const-hash-" }

                -- Change to a state of No Confidence
                , propose NoConfidence
                    { url = "no-conf-url", dataHash = dummyBytes 32 "no-conf-hash-" }

                -- Ask an info poll about pineapple pizza
                , propose Info
                    { url = "info-url", dataHash = dummyBytes 32 "info-hash-" }

                -- Finally, suggest a hard fork
                , propose (HardForkInitiation ( 14, 0 ))
                    { url = "hf-url", dataHash = dummyBytes 32 "hf-hash-" }
                ]
            }
            (\tx ->
                let
                    makeProposalProcedure shortname govAction =
                        { deposit = ada100K
                        , depositReturnAccount = myStakeAddress
                        , anchor = { url = shortname ++ "-url", dataHash = dummyBytes 32 (shortname ++ "-hash-") }
                        , govAction = govAction
                        }
                in
                { newTx
                    | body =
                        { newBody
                            | fee = ada 2
                            , inputs = [ makeRef "0" 0 ]
                            , outputs = [ Utxo.fromLovelace testAddr.me (ada 8) ]

                            -- proposals
                            , proposalProcedures =
                                [ makeProposalProcedure "param"
                                    (Gov.ParameterChange
                                        { latestEnacted = Nothing
                                        , guardrailsPolicy = Just <| Bytes.fromHexUnchecked "fa24fb305126805cf2164c161d852a0e7330cf988f1fe558cf7d4a64"
                                        , protocolParamUpdate = { noParamUpdate | minPoolCost = Just Natural.zero }
                                        }
                                    )
                                , makeProposalProcedure "withdraw"
                                    (Gov.TreasuryWithdrawals
                                        { withdrawals = [ ( myStakeAddress, ada 1000000 ) ]
                                        , guardrailsPolicy = Just <| Bytes.fromHexUnchecked "fa24fb305126805cf2164c161d852a0e7330cf988f1fe558cf7d4a64"
                                        }
                                    )
                                , makeProposalProcedure "new-const"
                                    (Gov.NewConstitution
                                        { latestEnacted = Nothing
                                        , constitution =
                                            { anchor = { url = "constitution-url", dataHash = dummyBytes 32 "const-hash-" }
                                            , scripthash = Nothing
                                            }
                                        }
                                    )
                                , makeProposalProcedure "no-conf" (Gov.NoConfidence { latestEnacted = Nothing })
                                , makeProposalProcedure "info" Gov.Info
                                , makeProposalProcedure "hf" (Gov.HardForkInitiation { latestEnacted = Nothing, protocolVersion = ( 14, 0 ) })
                                ]

                            -- script stuff
                            , scriptDataHash = tx.body.scriptDataHash

                            -- collateral would cost 3 ada for 2 ada fees, so return 600010-3=600007 ada
                            , collateral = [ makeRef "0" 0 ]
                            , totalCollateral = Just 3000000
                            , collateralReturn = Just (Utxo.fromLovelace testAddr.me (ada 600007))
                        }
                    , witnessSet =
                        { newWitnessSet
                            | vkeywitness = Just [ { vkey = dummyBytes 32 "VKEYkey-me", signature = dummyBytes 64 "SIGNATUREkey-me" } ]
                            , plutusV3Script = Just [ guardrailsScriptBytes ]
                            , redeemer =
                                Uplc.evalScriptsCosts Uplc.defaultVmConfig (Utxo.refDictFromList localStateUtxos) tx
                                    |> Result.toMaybe
                        }
                }
            )

        -- Test with multiple votes
        , let
            myStakeKeyHash =
                Address.extractStakeKeyHash testAddr.me
                    |> Maybe.withDefault (dummyCredentialHash "ERROR")

            -- Action being voted on
            actionId index =
                { transactionId = dummyBytes 32 "actionTx"
                , govActionIndex = index
                }

            -- Trivial native script drep that always succeeds
            drepScript =
                ScriptAll []

            drepScriptCbor =
                E.encode (Script.encodeNativeScript drepScript)
                    |> Bytes.fromBytes

            drepScriptHash =
                blake2b224 Nothing (Bytes.toU8 drepScriptCbor)
                    |> Bytes.fromU8

            -- Define different voters
            withMyDrepCred =
                WithDrepCred (WithKey myStakeKeyHash)

            withMyPoolCred =
                WithPoolCred (dummyCredentialHash "poolId")

            withMyDrepScript =
                WithDrepCred (WithScript drepScriptHash <| NativeWitness { script = WitnessValue drepScript, expectedSigners = [] })
          in
          okTxTest "Test with multiple votes"
            { govState = Cardano.emptyGovernanceState
            , localStateUtxos =
                [ ( makeRef "0" 0, Utxo.fromLovelace testAddr.me (ada 5) )
                ]
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents =
                [ Vote withMyDrepCred
                    [ { actionId = actionId 0, vote = VoteYes, rationale = Nothing }
                    , { actionId = actionId 1, vote = VoteYes, rationale = Nothing }
                    ]
                , Vote withMyPoolCred
                    [ { actionId = actionId 1, vote = VoteNo, rationale = Nothing }
                    , { actionId = actionId 0, vote = VoteNo, rationale = Nothing }
                    ]

                -- action 1 will be overwritten by action 0, because same Voter
                , Vote withMyDrepScript [ { actionId = actionId 1, vote = VoteAbstain, rationale = Nothing } ]
                , Vote withMyDrepScript [ { actionId = actionId 0, vote = VoteAbstain, rationale = Nothing } ]
                ]
            }
            (\_ ->
                { newTx
                    | body =
                        { newBody
                            | fee = ada 2
                            , inputs = [ makeRef "0" 0 ]
                            , outputs = [ Utxo.fromLovelace testAddr.me (ada 3) ]
                            , votingProcedures =
                                [ ( VoterDrepCred (ScriptHash drepScriptHash), [ ( actionId 0, { vote = VoteAbstain, anchor = Nothing } ) ] )
                                , ( VoterDrepCred (VKeyHash myStakeKeyHash)
                                  , [ ( actionId 0, { vote = VoteYes, anchor = Nothing } )
                                    , ( actionId 1, { vote = VoteYes, anchor = Nothing } )
                                    ]
                                  )
                                , ( VoterPoolId (dummyCredentialHash "poolId")
                                  , [ ( actionId 1, { vote = VoteNo, anchor = Nothing } )
                                    , ( actionId 0, { vote = VoteNo, anchor = Nothing } )
                                    ]
                                  )
                                ]
                        }
                    , witnessSet =
                        { newWitnessSet
                            | vkeywitness =
                                Just
                                    [ { vkey = dummyBytes 32 "VKEYkey-me", signature = dummyBytes 64 "SIGNATUREkey-me" }
                                    , { vkey = dummyBytes 32 "VKEYpoolId", signature = dummyBytes 64 "SIGNATUREpoolId" }
                                    , { vkey = dummyBytes 32 "VKEYstk-me", signature = dummyBytes 64 "SIGNATUREstk-me" }
                                    ]
                            , nativeScripts = Just [ drepScript ]
                        }
                }
            )
        ]


okTxTest :
    String
    ->
        { govState : GovernanceState
        , localStateUtxos : List ( OutputReference, Output )
        , evalScriptsCosts : Utxo.RefDict Output -> Transaction -> Result String (List Redeemer)
        , fee : Fee
        , txOtherInfo : List TxOtherInfo
        , txIntents : List TxIntent
        }
    -> (Transaction -> Transaction)
    -> Test
okTxTest description { govState, localStateUtxos, evalScriptsCosts, fee, txOtherInfo, txIntents } expectTransaction =
    test description <|
        \_ ->
            let
                buildingConfig =
                    { govState = govState
                    , localStateUtxos = Utxo.refDictFromList localStateUtxos
                    , coinSelectionAlgo = CoinSelection.largestFirst
                    , evalScriptsCosts = evalScriptsCosts
                    , costModels = Uplc.conwayDefaultCostModels
                    }
            in
            case finalizeAdvanced buildingConfig fee txOtherInfo txIntents of
                Err error ->
                    Expect.fail (Debug.toString error)

                Ok tx ->
                    Expect.equal tx <| expectTransaction tx


failTxBuilding : Test
failTxBuilding =
    describe "Detected failure"
        [ test "simple finalization cannot find fee source without enough info in Tx intents" <|
            \_ ->
                let
                    localStateUtxos =
                        Utxo.refDictFromList [ makeAdaOutput 0 testAddr.me 5 ]
                in
                Expect.equal (Err UnableToGuessFeeSource) (Cardano.finalize localStateUtxos [] [])
        , failTxTest "when there is no utxo in local state"
            { govState = Cardano.emptyGovernanceState
            , localStateUtxos = []
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents = []
            }
            (\error ->
                case error of
                    FailedToPerformCoinSelection (UTxOBalanceInsufficient _) ->
                        Expect.pass

                    _ ->
                        Expect.fail ("I didn’t expect this failure: " ++ Debug.toString error)
            )
        , failTxTest "when there is insufficient manual fee (0.1 ada here)"
            { govState = Cardano.emptyGovernanceState
            , localStateUtxos = [ makeAdaOutput 0 testAddr.me 5 ]
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = ManualFee [ { paymentSource = testAddr.me, exactFeeAmount = Natural.fromSafeInt 100000 } ]
            , txOtherInfo = []
            , txIntents = []
            }
            (\error ->
                case error of
                    InsufficientManualFee _ ->
                        Expect.pass

                    _ ->
                        Expect.fail ("I didn’t expect this failure: " ++ Debug.toString error)
            )
        , failTxTest "when inputs are missing from local state"
            { govState = Cardano.emptyGovernanceState
            , localStateUtxos = []
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents =
                [ Spend <| FromWalletUtxo (makeRef "0" 0)
                , SendTo testAddr.me (Value.onlyLovelace <| ada 1)
                ]
            }
            (\error ->
                case error of
                    ReferenceOutputsMissingFromLocalState [ ref ] ->
                        Expect.equal ref (makeRef "0" 0)

                    _ ->
                        Expect.fail ("I didn’t expect this failure: " ++ Debug.toString error)
            )
        , failTxTest "when Tx intents are unbalanced (too much spend here)"
            { govState = Cardano.emptyGovernanceState
            , localStateUtxos = [ makeAdaOutput 0 testAddr.me 5 ]
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents = [ Spend <| FromWallet testAddr.me (Value.onlyLovelace <| ada 1) ]
            }
            (\error ->
                case error of
                    UnbalancedIntents _ ->
                        Expect.pass

                    _ ->
                        Expect.fail ("I didn’t expect this failure: " ++ Debug.toString error)
            )
        , failTxTest "when Tx intents are unbalanced (too much send here)"
            { govState = Cardano.emptyGovernanceState
            , localStateUtxos = [ makeAdaOutput 0 testAddr.me 5 ]
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents = [ SendTo testAddr.me (Value.onlyLovelace <| ada 1) ]
            }
            (\error ->
                case error of
                    UnbalancedIntents _ ->
                        Expect.pass

                    _ ->
                        Expect.fail ("I didn’t expect this failure: " ++ Debug.toString error)
            )
        , failTxTest "when there is not enough minAda in created output (100 lovelaces here)"
            { govState = Cardano.emptyGovernanceState
            , localStateUtxos = [ makeAdaOutput 0 testAddr.me 5 ]
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents =
                [ Spend <| FromWallet testAddr.me (Value.onlyLovelace <| Natural.fromSafeInt 100)
                , SendToOutput (Utxo.fromLovelace testAddr.me <| Natural.fromSafeInt 100)
                ]
            }
            (\error ->
                case error of
                    NotEnoughMinAda _ ->
                        Expect.pass

                    _ ->
                        Expect.fail ("I didn’t expect this failure: " ++ Debug.toString error)
            )
        , failTxTest "when we send CNT without Ada"
            { govState = Cardano.emptyGovernanceState
            , localStateUtxos =
                [ makeAdaOutput 0 testAddr.me 5
                , makeAsset 1 testAddr.me cat.policyIdStr cat.assetNameStr 3
                ]
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents =
                [ Spend <| FromWallet testAddr.me (Value.onlyToken cat.policyId cat.assetName Natural.three)
                , SendTo testAddr.you (Value.onlyToken cat.policyId cat.assetName Natural.three)
                ]
            }
            (\error ->
                case error of
                    NotEnoughMinAda _ ->
                        Expect.pass

                    _ ->
                        Expect.fail ("I didn’t expect this failure: " ++ Debug.toString error)
            )
        , failTxTest "when there are duplicated metadata tags (tag 0 here)"
            { govState = Cardano.emptyGovernanceState
            , localStateUtxos = [ makeAdaOutput 0 testAddr.me 5 ]
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo =
                [ TxMetadata { tag = Natural.zero, metadata = Metadatum.Int Integer.one }
                , TxMetadata { tag = Natural.zero, metadata = Metadatum.Int Integer.two }
                ]
            , txIntents = []
            }
            (\error ->
                case error of
                    DuplicatedMetadataTags 0 ->
                        Expect.pass

                    _ ->
                        Expect.fail ("I didn’t expect this failure: " ++ Debug.toString error)
            )
        , failTxTest "when validity range is incorrect (start > end)"
            { govState = Cardano.emptyGovernanceState
            , localStateUtxos = [ makeAdaOutput 0 testAddr.me 5 ]
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = [ TxTimeValidityRange { start = 1, end = Natural.zero } ]
            , txIntents = []
            }
            (\error ->
                case error of
                    IncorrectTimeValidityRange _ ->
                        Expect.pass

                    _ ->
                        Expect.fail ("I didn’t expect this failure: " ++ Debug.toString error)
            )

        -- TODO: test for collateral selection error
        ]


failTxTest :
    String
    ->
        { govState : GovernanceState
        , localStateUtxos : List ( OutputReference, Output )
        , evalScriptsCosts : Utxo.RefDict Output -> Transaction -> Result String (List Redeemer)
        , fee : Fee
        , txOtherInfo : List TxOtherInfo
        , txIntents : List TxIntent
        }
    -> (TxFinalizationError -> Expectation)
    -> Test
failTxTest description { govState, localStateUtxos, evalScriptsCosts, fee, txOtherInfo, txIntents } expectedFailure =
    test description <|
        \_ ->
            let
                buildingConfig =
                    { govState = govState
                    , localStateUtxos = Utxo.refDictFromList localStateUtxos --   2 ada at my address
                    , coinSelectionAlgo = CoinSelection.largestFirst
                    , evalScriptsCosts = evalScriptsCosts
                    , costModels = Uplc.conwayDefaultCostModels
                    }
            in
            case finalizeAdvanced buildingConfig fee txOtherInfo txIntents of
                Err error ->
                    expectedFailure error

                Ok _ ->
                    Expect.fail "This Tx building was not supposed to succeed"


newTx =
    Transaction.new



-- Test data


testAddr =
    { me = makeWalletAddress "me"
    , you = makeWalletAddress "you"
    }


dog =
    { policyId = dummyCredentialHash "dog"
    , policyIdStr = "dog"
    , assetName = Bytes.fromText "yksoh"
    , assetNameStr = "yksoh"
    , scriptRef = makeRef "dogScriptRef" 0
    , refOutput =
        { address = makeAddress "dogScriptRefAddress"
        , amount = Value.onlyLovelace (ada 5)
        , datumOption = Nothing
        , referenceScript = Just <| Script.Native <| Script.ScriptAll [] -- dummy
        }
    }


cat =
    { policyId = dummyCredentialHash "cat"
    , policyIdStr = "cat"
    , assetName = Bytes.fromText "felix"
    , assetNameStr = "felix"
    , scriptRef = makeRef "catScriptRef" 0
    , refOutput =
        { address = makeAddress "catScriptRefAddress"
        , amount = Value.onlyLovelace (ada 6)
        , datumOption = Nothing
        , referenceScript = Just <| Script.Native <| Script.ScriptAll [] -- dummy
        }
    }



-- Fee


twoAdaFee =
    ManualFee [ { paymentSource = testAddr.me, exactFeeAmount = ada 2 } ]


autoFee =
    AutoFee { paymentSource = testAddr.me }



-- Helper functions


dummyCredentialHash : String -> Bytes CredentialHash
dummyCredentialHash str =
    dummyBytes 28 str


makeWalletAddress : String -> Address
makeWalletAddress name =
    Address.Shelley
        { networkId = Mainnet
        , paymentCredential = VKeyHash (dummyCredentialHash <| "key-" ++ name)
        , stakeCredential = Just (InlineCredential (VKeyHash <| dummyCredentialHash <| "stk-" ++ name))
        }


makeAddress : String -> Address
makeAddress name =
    Address.enterprise Mainnet (dummyCredentialHash name)


makeRef : String -> Int -> OutputReference
makeRef id index =
    { transactionId = dummyBytes 32 id
    , outputIndex = index
    }


makeAsset : Int -> Address -> String -> String -> Int -> ( OutputReference, Output )
makeAsset index address policyId name amount =
    ( makeRef (String.fromInt index) index
    , { address = address
      , amount = makeToken policyId name amount
      , datumOption = Nothing
      , referenceScript = Nothing
      }
    )


makeAdaOutput : Int -> Address -> Int -> ( OutputReference, Output )
makeAdaOutput index address amount =
    ( makeRef (String.fromInt index) index
    , Utxo.fromLovelace address (ada amount)
    )


makeToken : String -> String -> Int -> Value
makeToken policyId name amount =
    Value.onlyToken (dummyCredentialHash policyId) (Bytes.fromText name) (Natural.fromSafeInt amount)


ada : Int -> Natural
ada n =
    Natural.fromSafeInt n
        |> Natural.mul (Natural.fromSafeInt 1000000)
