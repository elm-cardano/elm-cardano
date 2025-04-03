module Cardano.TxBuilding exposing (suite)

import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Map as Map
import Cardano exposing (ActionProposal(..), CertificateIntent(..), CredentialWitness(..), Fee(..), GovernanceState, ScriptWitness(..), SpendSource(..), TxFinalizationError(..), TxFinalized, TxIntent(..), TxOtherInfo(..), VoterWitness(..), WitnessSource(..), finalizeAdvanced)
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
                { tx =
                    { newTx
                        | body =
                            { newBody
                                | fee = ada 2
                                , inputs = [ makeRef "0" 0 ]
                            }
                    }
                , expectedSignatures = [ dummyCredentialHash "key-me" ]
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
            (\{ tx } ->
                let
                    placeholderSignedTx =
                        { tx
                            | witnessSet =
                                { newWitnessSet
                                    | vkeywitness =
                                        Just [ { vkey = Bytes.dummy 32 "", signature = Bytes.dummy 64 "" } ]
                                }
                        }

                    feeAmount =
                        Transaction.computeFees Transaction.defaultTxFeeParams { refScriptBytes = 0 } placeholderSignedTx
                            |> (\{ txSizeFee, scriptExecFee, refScriptSizeFee } ->
                                    Natural.add txSizeFee scriptExecFee |> Natural.add refScriptSizeFee
                               )

                    adaLeft =
                        Natural.sub (ada 2) feeAmount
                in
                { tx =
                    { newTx
                        | body =
                            { newBody
                                | fee = feeAmount
                                , inputs = [ makeRef "0" 0 ]
                                , outputs = [ Utxo.fromLovelace testAddr.me adaLeft ]
                            }
                    }
                , expectedSignatures = [ dummyCredentialHash "key-me" ]
                }
            )
        , okTxTest "with spending from, and sending to the same address"
            { govState = Cardano.emptyGovernanceState
            , localStateUtxos = [ makeAdaOutput 0 testAddr.me 5 ]
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents =
                [ Spend <| FromWallet { address = testAddr.me, value = Value.onlyLovelace <| ada 1, guaranteedUtxos = [] }
                , SendTo testAddr.me (Value.onlyLovelace <| ada 1)
                ]
            }
            (\_ ->
                { tx =
                    { newTx
                        | body =
                            { newBody
                                | fee = ada 2
                                , inputs = [ makeRef "0" 0 ]
                                , outputs = [ Utxo.fromLovelace testAddr.me (ada 3) ]
                            }
                    }
                , expectedSignatures = [ dummyCredentialHash "key-me" ]
                }
            )
        , test "simple finalization is able to find fee source" <|
            \_ ->
                let
                    localStateUtxos =
                        Utxo.refDictFromList [ makeAdaOutput 0 testAddr.me 5 ]

                    txIntents =
                        [ Spend <| FromWallet { address = testAddr.me, value = Value.onlyLovelace <| ada 1, guaranteedUtxos = [] }
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
                [ Spend <| FromWallet { address = testAddr.me, value = Value.onlyLovelace <| ada 1, guaranteedUtxos = [] }
                , SendTo testAddr.you (Value.onlyLovelace <| ada 1)
                ]
            }
            (\_ ->
                { tx =
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
                    }
                , expectedSignatures = [ dummyCredentialHash "key-me" ]
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
                [ Spend <| FromWallet { address = testAddr.you, value = Value.onlyLovelace <| ada 1, guaranteedUtxos = [] }
                , SendTo testAddr.me (Value.onlyLovelace <| ada 1)
                ]
            }
            (\_ ->
                { tx =
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
                    }
                , expectedSignatures =
                    [ dummyCredentialHash "key-me"
                    , dummyCredentialHash "key-you"
                    ]
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
                [ Spend <| FromWallet { address = testAddr.me, value = threeCatTwoAda, guaranteedUtxos = [] }
                , SendTo testAddr.you threeCatTwoAda
                ]
            }
            (\_ ->
                { tx =
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
                    }
                , expectedSignatures = [ dummyCredentialHash "key-me" ]
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
                [ Spend <| FromWallet { address = testAddr.me, value = threeCatMinAda, guaranteedUtxos = [] }
                , SendTo testAddr.you threeCatMinAda
                ]
            }
            (\_ ->
                { tx =
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
                    }
                , expectedSignatures = [ dummyCredentialHash "key-me" ]
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
                    , scriptWitness = NativeWitness { script = WitnessByReference dog.scriptRef, expectedSigners = [] }
                    }
                , SendTo testAddr.me (Value.onlyToken dog.policyId dog.assetName Natural.one)

                -- burning 1 cat
                , Spend <|
                    FromWallet
                        { address = testAddr.me
                        , value = Value.onlyToken cat.policyId cat.assetName Natural.one
                        , guaranteedUtxos = []
                        }
                , MintBurn
                    { policyId = cat.policyId
                    , assets = Map.singleton cat.assetName Integer.negativeOne
                    , scriptWitness = NativeWitness { script = WitnessByReference cat.scriptRef, expectedSigners = [] }
                    }
                ]
            }
            (\_ ->
                { tx =
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
                    }
                , expectedSignatures = [ dummyCredentialHash "key-me" ]
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

                    ( id, ( ref, _ ) ) :: next ->
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
            redeemer txBody =
                List.indexedMap Tuple.pair txBody.inputs
                    |> findSpendingUtxo
                    |> (Data.Int << Integer.fromSafeInt)

            -- Helper function to create an output at the lock script address.
            -- It contains our key credential in the datum.
            makeLockedOutput adaAmount =
                { address = lockScriptAddress
                , amount = adaAmount
                , datumOption = Just (Utxo.datumValueFromData <| Data.Bytes <| Bytes.toAny myKeyCred)
                , referenceScript = Nothing
                }

            localStateUtxos =
                [ makeAdaOutput 0 testAddr.me 14
                , makeAdaOutput 1 testAddr.me 8
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
                            { script = ( PlutusV3, WitnessByValue lock.scriptBytes )
                            , redeemerData = redeemer
                            , requiredSigners = [ myKeyCred ]
                            }
                        }
                , SendTo testAddr.me (Value.onlyLovelace <| ada 2)

                -- Return the other 2 ada to the lock script (there was 4 ada initially)
                , SendToOutput (makeLockedOutput <| Value.onlyLovelace <| ada 2)
                ]
            }
            (\{ tx } ->
                { tx =
                    { newTx
                        | body =
                            { newBody
                                | fee = ada 2
                                , inputs = [ makeRef "0" 0, utxoBeingSpent ]
                                , requiredSigners = [ myKeyCred ]
                                , outputs =
                                    [ makeLockedOutput <| Value.onlyLovelace <| ada 2
                                    , Utxo.fromLovelace testAddr.me (ada 14)
                                    ]

                                -- script stuff
                                , scriptDataHash = tx.body.scriptDataHash

                                -- collateral would cost 3 ada for 2 ada fees, so return 8-3=5 ada
                                , collateral = [ makeRef "1" 1 ]
                                , totalCollateral = Just 3000000
                                , collateralReturn = Just (Utxo.fromLovelace testAddr.me (ada 5))
                            }
                        , witnessSet =
                            { newWitnessSet
                                | plutusV3Script = Just [ lock.scriptBytes ]
                                , redeemer =
                                    Uplc.evalScriptsCosts Uplc.defaultVmConfig (Utxo.refDictFromList localStateUtxos) tx
                                        |> Result.toMaybe
                            }
                    }
                , expectedSignatures = [ dummyCredentialHash "key-me" ]
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
                [ Spend <|
                    FromWallet
                        { address = testAddr.me
                        , value = Value.onlyLovelace (ada 2) -- 2 ada for the registration deposit
                        , guaranteedUtxos = []
                        }
                , IssueCertificate <| RegisterStake { delegator = WithKey myStakeKeyHash, deposit = ada 2 }
                , IssueCertificate <| DelegateStake { delegator = WithKey myStakeKeyHash, poolId = Bytes.dummy 28 "poolId" }
                , IssueCertificate <| DelegateVotes { delegator = WithKey myStakeKeyHash, drep = VKeyHash <| dummyCredentialHash "drep" }
                ]
            }
            (\_ ->
                { tx =
                    { newTx
                        | body =
                            { newBody
                                | fee = ada 2
                                , inputs = [ makeRef "0" 0 ]
                                , outputs = [ Utxo.fromLovelace testAddr.me (ada 1) ]
                                , certificates =
                                    [ RegCert { delegator = VKeyHash myStakeKeyHash, deposit = Natural.fromSafeInt 2000000 }
                                    , StakeDelegationCert { delegator = VKeyHash myStakeKeyHash, poolId = Bytes.dummy 28 "poolId" }
                                    , VoteDelegCert { delegator = VKeyHash myStakeKeyHash, drep = DrepCredential <| VKeyHash <| dummyCredentialHash "drep" }
                                    ]
                            }
                    }
                , expectedSignatures =
                    [ dummyCredentialHash "key-me"
                    , dummyCredentialHash "stk-me"
                    ]
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
                        , scriptWitness = WitnessByValue guardrailsScriptBytes
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
                  Spend <|
                    FromWallet
                        { address = testAddr.me
                        , value = Value.onlyLovelace (Natural.mul Natural.six ada100K)
                        , guaranteedUtxos = []
                        }

                -- Change minPoolCost to 0
                , propose
                    (ParameterChange { noParamUpdate | minPoolCost = Just Natural.zero })
                    { url = "param-url", dataHash = Bytes.dummy 32 "param-hash-" }

                -- Withdraw 1M ada from the treasury
                , propose
                    (TreasuryWithdrawals [ { destination = myStakeAddress, amount = ada 1000000 } ])
                    { url = "withdraw-url", dataHash = Bytes.dummy 32 "withdraw-hash-" }

                -- Change the constitution to not have a guardrails script anymore
                , propose
                    (NewConstitution
                        { anchor = { url = "constitution-url", dataHash = Bytes.dummy 32 "const-hash-" }
                        , scripthash = Nothing
                        }
                    )
                    { url = "new-const-url", dataHash = Bytes.dummy 32 "new-const-hash-" }

                -- Change to a state of No Confidence
                , propose NoConfidence
                    { url = "no-conf-url", dataHash = Bytes.dummy 32 "no-conf-hash-" }

                -- Ask an info poll about pineapple pizza
                , propose Info
                    { url = "info-url", dataHash = Bytes.dummy 32 "info-hash-" }

                -- Finally, suggest a hard fork
                , propose (HardForkInitiation ( 14, 0 ))
                    { url = "hf-url", dataHash = Bytes.dummy 32 "hf-hash-" }
                ]
            }
            (\{ tx } ->
                let
                    makeProposalProcedure shortname govAction =
                        { deposit = ada100K
                        , depositReturnAccount = myStakeAddress
                        , anchor = { url = shortname ++ "-url", dataHash = Bytes.dummy 32 (shortname ++ "-hash-") }
                        , govAction = govAction
                        }
                in
                { tx =
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
                                                { anchor = { url = "constitution-url", dataHash = Bytes.dummy 32 "const-hash-" }
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
                                | plutusV3Script = Just [ guardrailsScriptBytes ]
                                , redeemer =
                                    Uplc.evalScriptsCosts Uplc.defaultVmConfig (Utxo.refDictFromList localStateUtxos) tx
                                        |> Result.toMaybe
                            }
                    }
                , expectedSignatures = [ dummyCredentialHash "key-me" ]
                }
            )

        -- Test with multiple votes
        , let
            myStakeKeyHash =
                Address.extractStakeKeyHash testAddr.me
                    |> Maybe.withDefault (dummyCredentialHash "ERROR")

            -- Action being voted on
            actionId index =
                { transactionId = Bytes.dummy 32 "actionTx"
                , govActionIndex = index
                }

            -- Trivial native script drep that always succeeds
            drepScript =
                ScriptAll []

            drepScriptHash =
                Script.hash (Script.Native drepScript)

            -- Define different voters
            withMyDrepCred =
                WithDrepCred (WithKey myStakeKeyHash)

            withMyPoolCred =
                WithPoolCred (dummyCredentialHash "poolId")

            withMyDrepScript =
                WithDrepCred (WithScript drepScriptHash <| NativeWitness { script = WitnessByValue drepScript, expectedSigners = [] })
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
                { tx =
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
                            { newWitnessSet | nativeScripts = Just [ drepScript ] }
                    }
                , expectedSignatures =
                    [ dummyCredentialHash "key-me"
                    , dummyCredentialHash "poolId"
                    , dummyCredentialHash "stk-me"
                    ]
                }
            )

        -- Test with a script checking that each redeemer is an Int that corresponds
        -- to the actual index of the redeemer in the script context.
        -- This order is important to know, to make sure we build the TxContext correctly.
        -- CF tweet: https://x.com/phil_uplc/status/1907456382061723818
        -- CF Haskell code: https://github.com/IntersectMBO/cardano-ledger/blob/d79d41e09da6ab93067acddf624d1a540a3e4e8d/eras/conway/impl/src/Cardano/Ledger/Conway/Scripts.hs#L188
        , let
            spendIntents redeemer =
                [ Spend <|
                    FromPlutusScript
                        { spentInput = makeRef "1" 1
                        , datumWitness = Nothing
                        , plutusScriptWitness = indexedScript.witness redeemer
                        }
                , SendTo testAddr.me (Value.onlyLovelace <| ada 2)
                ]

            mintIntents redeemer =
                [ MintBurn
                    { policyId = indexedScript.hash
                    , assets = Map.singleton Bytes.empty Integer.one
                    , scriptWitness = PlutusWitness <| indexedScript.witness redeemer
                    }
                , SendTo testAddr.me <|
                    Value.onlyToken indexedScript.hash Bytes.empty Natural.one
                ]

            withdrawIntents redeemer =
                [ WithdrawRewards
                    { stakeCredential =
                        { networkId = Testnet
                        , stakeCredential = ScriptHash indexedScript.hash
                        }
                    , amount = Natural.zero
                    , scriptWitness = Just <| PlutusWitness <| indexedScript.witness redeemer
                    }
                ]

            certificateIntents redeemer =
                [ IssueCertificate <|
                    RegisterStake
                        { delegator =
                            WithScript indexedScript.hash <|
                                PlutusWitness (indexedScript.witness redeemer)
                        , deposit = Natural.zero
                        }
                ]

            voteIntents redeemer =
                let
                    voter =
                        WithDrepCred <|
                            WithScript indexedScript.hash <|
                                PlutusWitness (indexedScript.witness redeemer)

                    dummyVote =
                        { actionId =
                            { transactionId = Bytes.dummy 32 "txid"
                            , govActionIndex = 0
                            }
                        , vote = VoteNo
                        , rationale = Nothing
                        }
                in
                [ Vote voter [ dummyVote ] ]
          in
          okTxTest "where the redeemers are correctly sorted"
            { govState = Cardano.emptyGovernanceState
            , localStateUtxos =
                [ makeAdaOutput 0 testAddr.me 5
                , makeAdaOutput 1 indexedScript.address 2
                ]
            , evalScriptsCosts = Uplc.evalScriptsCosts Uplc.defaultVmConfig
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents =
                List.concat
                    -- The indices here should correspond to their index
                    -- in the list of redeemers in the script context.
                    [ spendIntents 0
                    , mintIntents 1
                    , certificateIntents 2
                    , withdrawIntents 3
                    , voteIntents 4

                    -- Propose would be 5 (but not easy to check for real)
                    ]
            }
            (\{ tx } ->
                { tx = tx
                , expectedSignatures = [ dummyCredentialHash "key-me" ]
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
    -> (TxFinalized -> TxFinalized)
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
                [ Spend <|
                    FromWallet
                        { address = testAddr.me
                        , value = Value.onlyLovelace <| ada 1
                        , guaranteedUtxos = [ makeRef "0" 0 ] -- Non-existent UTxO
                        }
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
            , txIntents = [ Spend <| FromWallet { address = testAddr.me, value = Value.onlyLovelace <| ada 1, guaranteedUtxos = [] } ]
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
                [ Spend <| FromWallet { address = testAddr.me, value = Value.onlyLovelace <| Natural.fromSafeInt 100, guaranteedUtxos = [] }
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
                [ Spend <|
                    FromWallet
                        { address = testAddr.me
                        , value = Value.onlyToken cat.policyId cat.assetName Natural.three
                        , guaranteedUtxos = []
                        }
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

        -- Test for collateral selection error
        , let
            utxoBeingSpent =
                makeRef "previouslySentToLock" 0

            findSpendingUtxo inputs =
                case inputs of
                    [] ->
                        0

                    ( id, ( ref, _ ) ) :: next ->
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
            redeemer txBody =
                List.indexedMap Tuple.pair txBody.inputs
                    |> findSpendingUtxo
                    |> (Data.Int << Integer.fromSafeInt)

            -- Helper function to create an output at the lock script address.
            -- It contains our key credential in the datum.
            makeLockedOutput adaAmount =
                { address = lockScriptAddress
                , amount = adaAmount
                , datumOption = Just (Utxo.datumValueFromData <| Data.Bytes <| Bytes.toAny myKeyCred)
                , referenceScript = Nothing
                }

            -- We put only 2 ada in local UTxOs, but we need 3 ada for the collateral
            -- because the Tx fees is 2 ada.
            localStateUtxos =
                [ makeAdaOutput 0 testAddr.me 2
                , ( utxoBeingSpent, makeLockedOutput <| Value.onlyLovelace <| ada 4 )
                ]
          in
          failTxTest "when collateral is insufficient in: spend 2 ada from a plutus script holding 4 ada"
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
                            { script = ( PlutusV3, WitnessByValue lock.scriptBytes )
                            , redeemerData = redeemer
                            , requiredSigners = [ myKeyCred ]
                            }
                        }
                , SendTo testAddr.me (Value.onlyLovelace <| ada 2)

                -- Return the other 2 ada to the lock script (there was 4 ada initially)
                , SendToOutput (makeLockedOutput <| Value.onlyLovelace <| ada 2)
                ]
            }
            (\error ->
                case error of
                    CollateralSelectionError (CoinSelection.UTxOBalanceInsufficient _) ->
                        Expect.pass

                    _ ->
                        Expect.fail ("I didn’t expect this failure: " ++ Debug.toString error)
            )

        -- Test failing when providing the wrong native script in witness
        , let
            correctScript =
                ScriptAny []

            wrongScript =
                ScriptAll []

            utxoBeingSpent =
                makeRef "previouslySentToLock" 0

            localStateUtxos =
                [ makeAdaOutput 0 testAddr.me 5
                , ( utxoBeingSpent
                  , { address = Address.script Mainnet (Script.hash <| Script.Native correctScript)
                    , amount = Value.onlyLovelace (ada 4)
                    , datumOption = Nothing
                    , referenceScript = Nothing
                    }
                  )
                ]
          in
          failTxTest "when providing the wrong native script witness"
            { govState = Cardano.emptyGovernanceState
            , localStateUtxos = localStateUtxos
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents =
                [ Spend <|
                    FromNativeScript
                        { spentInput = utxoBeingSpent
                        , nativeScriptWitness = WitnessByValue wrongScript
                        }
                , SendTo testAddr.me (Value.onlyLovelace <| ada 4)
                ]
            }
            (\error ->
                case error of
                    ScriptHashMismatch _ _ ->
                        Expect.pass

                    _ ->
                        Expect.fail ("I didn’t expect this failure: " ++ Debug.toString error)
            )

        -- Test failing when providing the wrong native script in witness by reference
        , let
            correctScript =
                ScriptAny []

            wrongScript =
                ScriptAll []

            utxoBeingSpent =
                makeRef "previouslySentToLock" 0

            utxoWithScriptRef =
                makeRef "scriptRef" 0

            localStateUtxos =
                [ makeAdaOutput 0 testAddr.me 5
                , ( utxoBeingSpent
                  , { address = Address.script Mainnet (Script.hash <| Script.Native correctScript)
                    , amount = Value.onlyLovelace (ada 4)
                    , datumOption = Nothing
                    , referenceScript = Nothing
                    }
                  )
                , ( utxoWithScriptRef
                  , { address = testAddr.me
                    , amount = Value.onlyLovelace (ada 2)
                    , datumOption = Nothing
                    , referenceScript = Just <| Script.refFromScript <| Script.Native wrongScript
                    }
                  )
                ]
          in
          failTxTest "when providing the wrong native script witness by reference"
            { govState = Cardano.emptyGovernanceState
            , localStateUtxos = localStateUtxos
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents =
                [ Spend <|
                    FromNativeScript
                        { spentInput = utxoBeingSpent
                        , nativeScriptWitness = WitnessByReference utxoWithScriptRef
                        }
                , SendTo testAddr.me (Value.onlyLovelace <| ada 4)
                ]
            }
            (\error ->
                case error of
                    ScriptHashMismatch _ _ ->
                        Expect.pass

                    _ ->
                        Expect.fail ("I didn’t expect this failure: " ++ Debug.toString error)
            )

        -- Test failing when providing a witness by reference with no reference script
        , let
            utxoBeingSpent =
                makeRef "previouslySentToLock" 0

            utxoWithoutScriptRef =
                makeRef "scriptRef" 0

            localStateUtxos =
                [ makeAdaOutput 0 testAddr.me 5
                , ( utxoBeingSpent
                  , { address = Address.script Mainnet (Script.hash <| Script.Native <| ScriptAny [])
                    , amount = Value.onlyLovelace (ada 4)
                    , datumOption = Nothing
                    , referenceScript = Nothing
                    }
                  )
                , ( utxoWithoutScriptRef
                  , { address = testAddr.me
                    , amount = Value.onlyLovelace (ada 2)
                    , datumOption = Nothing
                    , referenceScript = Nothing
                    }
                  )
                ]
          in
          failTxTest "when providing a witness by reference with no reference script"
            { govState = Cardano.emptyGovernanceState
            , localStateUtxos = localStateUtxos
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents =
                [ Spend <|
                    FromNativeScript
                        { spentInput = utxoBeingSpent
                        , nativeScriptWitness = WitnessByReference utxoWithoutScriptRef
                        }
                , SendTo testAddr.me (Value.onlyLovelace <| ada 4)
                ]
            }
            (\error ->
                case error of
                    MissingReferenceScript _ ->
                        Expect.pass

                    _ ->
                        Expect.fail ("I didn’t expect this failure: " ++ Debug.toString error)
            )

        -- Test that provided plutus script witness value has the expected hash
        , let
            correctScript =
                Script.plutusScriptFromBytes PlutusV3 <| Bytes.fromHexUnchecked ""

            wrongScript =
                ( PlutusV2, WitnessByValue <| Bytes.fromHexUnchecked "" )

            utxoBeingSpent =
                makeRef "previouslySentToLock" 0

            localStateUtxos =
                [ makeAdaOutput 0 testAddr.me 5
                , ( utxoBeingSpent
                  , { address = Address.script Mainnet (Script.hash <| Script.Plutus correctScript)
                    , amount = Value.onlyLovelace <| ada 4
                    , datumOption = Nothing
                    , referenceScript = Nothing
                    }
                  )
                ]
          in
          failTxTest "when providing the wrong plutus script witness"
            { govState = Cardano.emptyGovernanceState
            , localStateUtxos = localStateUtxos
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents =
                [ Spend <|
                    FromPlutusScript
                        { spentInput = utxoBeingSpent
                        , datumWitness = Nothing
                        , plutusScriptWitness =
                            { script = wrongScript
                            , redeemerData = \_ -> Data.List []
                            , requiredSigners = []
                            }
                        }
                , SendTo testAddr.me (Value.onlyLovelace <| ada 4)
                ]
            }
            (\error ->
                case error of
                    ScriptHashMismatch _ _ ->
                        Expect.pass

                    _ ->
                        Expect.fail ("I didn’t expect this failure: " ++ Debug.toString error)
            )

        -- Test that there is no extraneous datum witness
        , let
            script =
                Script.plutusScriptFromBytes PlutusV3 <| Bytes.fromHexUnchecked ""

            scriptWitness =
                ( PlutusV3, WitnessByValue <| Bytes.fromHexUnchecked "" )

            extraneousDatumWitness =
                WitnessByValue <| Data.List []

            utxoBeingSpent =
                makeRef "previouslySentToLock" 0

            localStateUtxos =
                [ makeAdaOutput 0 testAddr.me 5
                , ( utxoBeingSpent
                  , { address = Address.script Mainnet (Script.hash <| Script.Plutus script)
                    , amount = Value.onlyLovelace <| ada 4

                    -- No datum at the script utxo
                    , datumOption = Nothing
                    , referenceScript = Nothing
                    }
                  )
                ]
          in
          failTxTest "when there is an extraneous datum witness (no datum at script utxo)"
            { govState = Cardano.emptyGovernanceState
            , localStateUtxos = localStateUtxos
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents =
                [ Spend <|
                    FromPlutusScript
                        { spentInput = utxoBeingSpent

                        -- Extraneous datum witness here
                        , datumWitness = Just extraneousDatumWitness
                        , plutusScriptWitness =
                            { script = scriptWitness
                            , redeemerData = \_ -> Data.List []
                            , requiredSigners = []
                            }
                        }
                , SendTo testAddr.me (Value.onlyLovelace <| ada 4)
                ]
            }
            (\error ->
                case error of
                    ExtraneousDatumWitness _ _ ->
                        Expect.pass

                    _ ->
                        Expect.fail ("I didn’t expect this failure: " ++ Debug.toString error)
            )

        -- Test that there is no extraneous datum witness (already by value in script utxo)
        , let
            script =
                Script.plutusScriptFromBytes PlutusV3 <| Bytes.fromHexUnchecked ""

            scriptWitness =
                ( PlutusV3, WitnessByValue <| Bytes.fromHexUnchecked "" )

            extraneousDatumWitness =
                WitnessByValue <| Data.List []

            utxoBeingSpent =
                makeRef "previouslySentToLock" 0

            localStateUtxos =
                [ makeAdaOutput 0 testAddr.me 5
                , ( utxoBeingSpent
                  , { address = Address.script Mainnet (Script.hash <| Script.Plutus script)
                    , amount = Value.onlyLovelace <| ada 4

                    -- Datum already provided by value
                    , datumOption = Just <| Utxo.datumValueFromData <| Data.List []
                    , referenceScript = Nothing
                    }
                  )
                ]
          in
          failTxTest "when there is an extraneous datum witness (datum value already at script utxo)"
            { govState = Cardano.emptyGovernanceState
            , localStateUtxos = localStateUtxos
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents =
                [ Spend <|
                    FromPlutusScript
                        { spentInput = utxoBeingSpent

                        -- Extraneous datum witness here
                        , datumWitness = Just extraneousDatumWitness
                        , plutusScriptWitness =
                            { script = scriptWitness
                            , redeemerData = \_ -> Data.List []
                            , requiredSigners = []
                            }
                        }
                , SendTo testAddr.me (Value.onlyLovelace <| ada 4)
                ]
            }
            (\error ->
                case error of
                    ExtraneousDatumWitness _ _ ->
                        Expect.pass

                    _ ->
                        Expect.fail ("I didn’t expect this failure: " ++ Debug.toString error)
            )

        -- Test that the datum witness is not missing
        , let
            script =
                Script.plutusScriptFromBytes PlutusV3 <| Bytes.fromHexUnchecked ""

            scriptWitness =
                ( PlutusV3, WitnessByValue <| Bytes.fromHexUnchecked "" )

            utxoBeingSpent =
                makeRef "previouslySentToLock" 0

            datum =
                Data.List []

            localStateUtxos =
                [ makeAdaOutput 0 testAddr.me 5
                , ( utxoBeingSpent
                  , { address = Address.script Mainnet (Script.hash <| Script.Plutus script)
                    , amount = Value.onlyLovelace <| ada 4

                    -- Datum provided by hash
                    , datumOption = Just <| DatumHash <| Data.hash datum
                    , referenceScript = Nothing
                    }
                  )
                ]
          in
          failTxTest "when the datum witness is missing"
            { govState = Cardano.emptyGovernanceState
            , localStateUtxos = localStateUtxos
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents =
                [ Spend <|
                    FromPlutusScript
                        { spentInput = utxoBeingSpent
                        , datumWitness = Nothing
                        , plutusScriptWitness =
                            { script = scriptWitness
                            , redeemerData = \_ -> Data.List []
                            , requiredSigners = []
                            }
                        }
                , SendTo testAddr.me (Value.onlyLovelace <| ada 4)
                ]
            }
            (\error ->
                case error of
                    MissingDatumWitness _ _ ->
                        Expect.pass

                    _ ->
                        Expect.fail ("I didn’t expect this failure: " ++ Debug.toString error)
            )

        -- Test that the datum witness is not missing (by ref)
        , let
            script =
                Script.plutusScriptFromBytes PlutusV3 <| Bytes.fromHexUnchecked ""

            scriptWitness =
                ( PlutusV3, WitnessByValue <| Bytes.fromHexUnchecked "" )

            utxoBeingSpent =
                makeRef "previouslySentToLock" 0

            utxoRefWithoutDatum =
                makeRef "datumRef" 0

            datum =
                Data.List []

            localStateUtxos =
                [ makeAdaOutput 0 testAddr.me 5
                , ( utxoBeingSpent
                  , { address = Address.script Mainnet (Script.hash <| Script.Plutus script)
                    , amount = Value.onlyLovelace <| ada 4

                    -- Datum provided by hash
                    , datumOption = Just <| DatumHash <| Data.hash datum
                    , referenceScript = Nothing
                    }
                  )
                , ( utxoRefWithoutDatum
                  , { address = Address.script Mainnet (Script.hash <| Script.Plutus script)
                    , amount = Value.onlyLovelace <| ada 4
                    , datumOption = Nothing
                    , referenceScript = Nothing
                    }
                  )
                ]
          in
          failTxTest "when the datum witness is missing (by ref)"
            { govState = Cardano.emptyGovernanceState
            , localStateUtxos = localStateUtxos
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents =
                [ Spend <|
                    FromPlutusScript
                        { spentInput = utxoBeingSpent
                        , datumWitness = Just <| WitnessByReference utxoRefWithoutDatum
                        , plutusScriptWitness =
                            { script = scriptWitness
                            , redeemerData = \_ -> Data.List []
                            , requiredSigners = []
                            }
                        }
                , SendTo testAddr.me (Value.onlyLovelace <| ada 4)
                ]
            }
            (\error ->
                case error of
                    MissingDatumWitness _ _ ->
                        Expect.pass

                    _ ->
                        Expect.fail ("I didn’t expect this failure: " ++ Debug.toString error)
            )

        -- Test that the datum witness is matching (by value)
        , let
            script =
                Script.plutusScriptFromBytes PlutusV3 <| Bytes.fromHexUnchecked ""

            scriptWitness =
                ( PlutusV3, WitnessByValue <| Bytes.fromHexUnchecked "" )

            utxoBeingSpent =
                makeRef "previouslySentToLock" 0

            correctDatum =
                Data.List []

            wrongDatum =
                Data.List [ Data.List [] ]

            localStateUtxos =
                [ makeAdaOutput 0 testAddr.me 5
                , ( utxoBeingSpent
                  , { address = Address.script Mainnet (Script.hash <| Script.Plutus script)
                    , amount = Value.onlyLovelace <| ada 4

                    -- Datum provided by hash
                    , datumOption = Just <| DatumHash <| Data.hash correctDatum
                    , referenceScript = Nothing
                    }
                  )
                ]
          in
          failTxTest "when the datum witness is not matching (by value)"
            { govState = Cardano.emptyGovernanceState
            , localStateUtxos = localStateUtxos
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents =
                [ Spend <|
                    FromPlutusScript
                        { spentInput = utxoBeingSpent
                        , datumWitness = Just <| WitnessByValue wrongDatum
                        , plutusScriptWitness =
                            { script = scriptWitness
                            , redeemerData = \_ -> Data.List []
                            , requiredSigners = []
                            }
                        }
                , SendTo testAddr.me (Value.onlyLovelace <| ada 4)
                ]
            }
            (\error ->
                case error of
                    DatumHashMismatch _ _ ->
                        Expect.pass

                    _ ->
                        Expect.fail ("I didn’t expect this failure: " ++ Debug.toString error)
            )

        -- Test that the datum witness is matching (by ref)
        , let
            script =
                Script.plutusScriptFromBytes PlutusV3 <| Bytes.fromHexUnchecked ""

            scriptWitness =
                ( PlutusV3, WitnessByValue <| Bytes.fromHexUnchecked "" )

            utxoBeingSpent =
                makeRef "previouslySentToLock" 0

            utxoRef =
                makeRef "datumRef" 0

            correctDatum =
                Data.List []

            wrongDatum =
                Data.List [ Data.List [] ]

            localStateUtxos =
                [ makeAdaOutput 0 testAddr.me 5
                , ( utxoBeingSpent
                  , { address = Address.script Mainnet (Script.hash <| Script.Plutus script)
                    , amount = Value.onlyLovelace <| ada 4

                    -- Datum provided by hash
                    , datumOption = Just <| DatumHash <| Data.hash correctDatum
                    , referenceScript = Nothing
                    }
                  )
                , ( utxoRef
                  , { address = Address.script Mainnet (Script.hash <| Script.Plutus script)
                    , amount = Value.onlyLovelace <| ada 4
                    , datumOption = Just <| Utxo.datumValueFromData wrongDatum
                    , referenceScript = Nothing
                    }
                  )
                ]
          in
          failTxTest "when the datum witness is not matching (by ref)"
            { govState = Cardano.emptyGovernanceState
            , localStateUtxos = localStateUtxos
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents =
                [ Spend <|
                    FromPlutusScript
                        { spentInput = utxoBeingSpent
                        , datumWitness = Just <| WitnessByReference utxoRef
                        , plutusScriptWitness =
                            { script = scriptWitness
                            , redeemerData = \_ -> Data.List []
                            , requiredSigners = []
                            }
                        }
                , SendTo testAddr.me (Value.onlyLovelace <| ada 4)
                ]
            }
            (\error ->
                case error of
                    DatumHashMismatch _ _ ->
                        Expect.pass

                    _ ->
                        Expect.fail ("I didn’t expect this failure: " ++ Debug.toString error)
            )
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


indexedScript =
    -- A V3 script where the Mint redeemer checks that all redeemers in the transaction
    -- are just Integers corresponding to the index in the redeemers’ list of the script context.
    let
        plutusScript =
            Script.plutusScriptFromBytes PlutusV3 <|
                Bytes.fromHexUnchecked
                    "59041201010029800aba4aba2aba1aba0aab9faab9eaab9dab9cab9a4888888888c96600264646644b30013370e900018041baa0018cc004dd7180618049baa00199198008009bab300d300e300e300e300e300e300e300e300e300e300a3754601a00a44b30010018a5eb8226601a6ea0c966002003008804402226eb40060108088c02cc038004cc008008c03c00500c4dc02400291129980519b964910b72656465656d6572733a20003732646466446530010019ba7007a44100400444464b30010038991919911980500119b8a48901280059800800c4cdc52441035b5d2900006899b8a489035b5f20009800800ccdc52441025d2900006914c00402a00530070014029229800805400a002805100920325980099b880014803a266e0120f2010018acc004cdc4000a41000513370066e01208014001480362c80990131bac3016002375a60280026466ec0dd4180a0009ba73015001375400713259800800c4cdc52441027b7d00003899b8a489037b5f20003232330010010032259800800c400e264b30010018994c00402a6032003337149101023a200098008054c06800600a805100a180e00144ca6002015301900199b8a489023a200098008054c068006600e66008008004805100a180e0012034301c001406466e29220102207d0000340586eac00e264b3001001899b8a489025b5d00003899b8a489035b5f20009800800ccdc52441015d00003914c00401e0053004001401d229800803c00a0028039006202c3758007133006375a0060051323371491102682700329800800cc02cdc68014cdc52450127000044004444b300133710004900044006264664530010069808002ccdc599b800025980099b88002480522903045206e406066e2ccdc0000acc004cdc4000a4029148182290372030004401866e0c00520203370c002901019b8e00400240546eb800d0191b8a4881022c2000223233001001003225980099b8700148002266e292210130000038acc004cdc4000a40011337149101012d0033002002337029000000c4cc014cdc2000a402866e2ccdc019b85001480512060003403c80788888c8cc004004014896600200310058992cc004006266008603000400d1330053018002330030030014058603000280a8c0040048896600266e2400920008800c6600200733708004900a4cdc599b803370a004900a240c0002801900c099baf374e6464660020029000112cc004cdc4001800c52f5c113301137500026600400466e00005200240306002646600200200644b30010018a40011337009001198010011809000a01e374e0048a5140186014002601460160026014002600a6ea802e293454cc00d2411856616c696461746f722072657475726e65642066616c7365001365640082a6600492011765787065637420696e6465783a20496e646578203d2072001601"

        hash =
            Bytes.fromHexUnchecked "a790039850292ae166a5b79ff6bea7ac03cdc3337ce9107150fda0e6"
    in
    { plutus = plutusScript
    , hash = hash
    , address = Address.script Testnet hash
    , witness =
        \index ->
            { script =
                ( Script.plutusVersion plutusScript
                , WitnessByValue <| Script.cborWrappedBytes plutusScript
                )
            , redeemerData = \_ -> Data.Int <| Integer.fromSafeInt index
            , requiredSigners = []
            }
    }


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
        , referenceScript = Just <| Script.refFromScript <| Script.Native <| Script.ScriptAll [] -- dummy
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
        , referenceScript = Just <| Script.refFromScript <| Script.Native <| Script.ScriptAll [] -- dummy
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
    Bytes.dummy 28 str


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
    { transactionId = Bytes.dummy 32 id
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
