module Cardano.TransactionTests exposing (suite)

import Bytes.Comparable as Bytes
import Bytes.Map exposing (BytesMap)
import Cardano.Address as Address exposing (NetworkId(..))
import Cardano.AuxiliaryData exposing (AuxiliaryData)
import Cardano.Gov exposing (Nonce(..), noParamUpdate)
import Cardano.Metadatum as Metadatum
import Cardano.Pool as Pool
import Cardano.Script exposing (NativeScript(..))
import Cardano.Transaction as Transaction exposing (TransactionBody, WitnessSet, newBody, newWitnessSet)
import Cardano.Value as Value
import Dict exposing (Dict)
import Expect
import Integer
import Natural as N exposing (Natural)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Cardano.Transaction"
        [ describe "encode"
            [ Test.test "TODO: add encoding tests" <| \_ -> Expect.pass
            ]
        , describe "decode"
            -- Shelley transactions
            [ decode79acf081
            , decode871b14fb
            , decodef3a0835d
            , decode841cca81
            , decode896cf8fe
            , decode3dd8be52
            , decode3c03090c
            , decode35d2728e
            , decodea2d8a927
            , decode2383af05
            , decode1bcd8fa7
            , decodec220e20c

            -- Allegra transactions
            , decode254685a8
            , decode4a3f8676

            -- Mary transactions
            , decodee252be4c
            , decodefc863a44

            -- Alonzo transactions and later are split into another test file
            ]
        ]



-- Shelley transactions


{-| First Shelley transaction!

Tx id: 79acf08126546b68d0464417af9530473b8c56c63b2a937bf6451e96e55cb96a
Previous block intersection:

  - slot: 4492880
  - id: 23fd3b638e8f286978681567d52597b73f7567e18719cef2cbd66bba31303d98

-}
decode79acf081 : Test
decode79acf081 =
    test "Tx id 79acf08126546b68d0464417af9530473b8c56c63b2a937bf6451e96e55cb96a" <|
        \_ ->
            Bytes.fromHexUnchecked "83a40085825820397eb970e7980e6ac1eb17fcb26a8df162db4e101f776138d74bbd09ad1a9dee008258207898478682dc3f692bc9bd28cb0df0eaac09a695e17dd563e4d1cc2ad2dc034e008258208db10dfb1c7c68f12c0d7159a39334cad649085e312b5b58ee4dcdad8e0f8fb00082582098fb88bc08d29243d7d10359182a901fdad8a8383bcf52e52af5614787ad394a00825820c1f3b0e99710ada0e479d6f75d60699ce08257b74a18ac170ed86f8dae82911c000185825839019566a8f301fb8a046e44557bb38dfb9080a1213f17f200dcd380816949f14106ef746c2d3597381d1d5d1c65c91e933acd1baef3fc915f0b1b0000005dd4a8a70782583901e54a5d5488b0cfc55a85a806ad5338bd945c500e5f9b1913c0fc5d4149f14106ef746c2d3597381d1d5d1c65c91e933acd1baef3fc915f0b1b0000000945c8cb00825839010ae8c86b7d82139749fc39cbe0ed8756de9285970899d0019db0990f49f14106ef746c2d3597381d1d5d1c65c91e933acd1baef3fc915f0b1a7732df0a82583901211c082781577c6b8a4832d29011baab323947e59fbd6ec8995b6c5a49f14106ef746c2d3597381d1d5d1c65c91e933acd1baef3fc915f0b1a000f4240825839016a39fc5175611d03cea47f580f465275bf4fc322b61987e83b0b977849f14106ef746c2d3597381d1d5d1c65c91e933acd1baef3fc915f0b1b000000174876e800021a000356f9031a0044aa70a10283845820f202012360fa94af83651a8b8b9592bcda2bee5e187c40d4263a838107c27ae858401c19005d3966c6d3734bde29ec60db8a493c89cb37a2bcbd569efaadd623b28f396de340f40f08b4660dfa728d0cc6ccab627324ad854f1b6b2d16854f1eab095820a1bbc30cf781c0a81b1afc059b7362111f70c45409ca71fc9e165a78e9c978965822a101581e581c8d9b2a782a9b394eff8857682bcd29b048835b93f9a2edf33ede73dd84582054e11b8cc512ce2b45b1b053e8f0e87ba9157a796f5653c20ddfef5af89a18e458406e2b18dde52deabb476ae171ec56b3620c63c0d6a00069f3bd7160ca0af7dd923cbf1ff1b414cb88d824b843b97fce5df70b477fad1fafd62aa1a2556e13f207582052fbfa9e0aca2281d1ef0359f9880b381e8b045273c59c9827f9762e939e768c5822a101581e581c8d9b2a782a9b390866febb68228dd65728c218e7fd86142c4d2736b3845820f5c382061f453618c6c957bb5345860b2736fee4fc29133215134fd92a26d03f5840d26bb37ef4efc404c59865b4f76d51d76c2c3d659430e74f7566884d1cffe5061da326be3aaf54dfddb9e89d69e31289dbee63dabbc5a71ea279bf7b10f8300758203ab2c5f5262ff16de9fe44821853b266d599906090662c40785355931d965f0c5822a101581e581c8d9b2a782a9b392ef1c4a1683cff72b7b6a02cd5dca8afb5c1a15973f6"
                |> Transaction.deserialize
                |> Expect.equal
                    (Just
                        { body = txBody79acf081
                        , witnessSet = txWitnessSet79acf081
                        , isValid = True
                        , auxiliaryData = Nothing
                        }
                    )


txBody79acf081 : TransactionBody
txBody79acf081 =
    { newBody
        | inputs =
            [ { transactionId = Bytes.fromHexUnchecked "397eb970e7980e6ac1eb17fcb26a8df162db4e101f776138d74bbd09ad1a9dee"
              , outputIndex = 0
              }
            , { transactionId = Bytes.fromHexUnchecked "7898478682dc3f692bc9bd28cb0df0eaac09a695e17dd563e4d1cc2ad2dc034e"
              , outputIndex = 0
              }
            , { transactionId = Bytes.fromHexUnchecked "8db10dfb1c7c68f12c0d7159a39334cad649085e312b5b58ee4dcdad8e0f8fb0"
              , outputIndex = 0
              }
            , { transactionId = Bytes.fromHexUnchecked "98fb88bc08d29243d7d10359182a901fdad8a8383bcf52e52af5614787ad394a"
              , outputIndex = 0
              }
            , { transactionId = Bytes.fromHexUnchecked "c1f3b0e99710ada0e479d6f75d60699ce08257b74a18ac170ed86f8dae82911c"
              , outputIndex = 0
              }
            ]
        , outputs =
            [ { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromHexUnchecked "9566a8f301fb8a046e44557bb38dfb9080a1213f17f200dcd3808169")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromHexUnchecked "49f14106ef746c2d3597381d1d5d1c65c91e933acd1baef3fc915f0b")))
                        }
              , amount = Value.onlyLovelace (N.fromSafeInt 402999781127)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            , { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromHexUnchecked "e54a5d5488b0cfc55a85a806ad5338bd945c500e5f9b1913c0fc5d41")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromHexUnchecked "49f14106ef746c2d3597381d1d5d1c65c91e933acd1baef3fc915f0b")))
                        }
              , amount = Value.onlyLovelace (N.fromSafeInt 39825492736)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            , { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromHexUnchecked "0ae8c86b7d82139749fc39cbe0ed8756de9285970899d0019db0990f")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromHexUnchecked "49f14106ef746c2d3597381d1d5d1c65c91e933acd1baef3fc915f0b")))
                        }
              , amount = Value.onlyLovelace (N.fromSafeInt 1999822602)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            , { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromHexUnchecked "211c082781577c6b8a4832d29011baab323947e59fbd6ec8995b6c5a")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromHexUnchecked "49f14106ef746c2d3597381d1d5d1c65c91e933acd1baef3fc915f0b")))
                        }
              , amount = Value.onlyLovelace (N.fromSafeInt 1000000)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            , { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromHexUnchecked "6a39fc5175611d03cea47f580f465275bf4fc322b61987e83b0b9778")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromHexUnchecked "49f14106ef746c2d3597381d1d5d1c65c91e933acd1baef3fc915f0b")))
                        }
              , amount = Value.onlyLovelace (N.fromSafeInt 100000000000)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            ]
        , fee = N.fromSafeInt 218873
        , ttl = Just (N.fromSafeInt 4500080)
    }


txWitnessSet79acf081 : Transaction.WitnessSet
txWitnessSet79acf081 =
    { newWitnessSet
        | bootstrapWitness =
            Just
                [ { publicKey = Bytes.fromHexUnchecked "f202012360fa94af83651a8b8b9592bcda2bee5e187c40d4263a838107c27ae8"
                  , signature = Bytes.fromHexUnchecked "1c19005d3966c6d3734bde29ec60db8a493c89cb37a2bcbd569efaadd623b28f396de340f40f08b4660dfa728d0cc6ccab627324ad854f1b6b2d16854f1eab09"
                  , chainCode = Bytes.fromHexUnchecked "a1bbc30cf781c0a81b1afc059b7362111f70c45409ca71fc9e165a78e9c97896"
                  , attributes = Bytes.fromHexUnchecked "a101581e581c8d9b2a782a9b394eff8857682bcd29b048835b93f9a2edf33ede73dd"
                  }
                , { publicKey = Bytes.fromHexUnchecked "54e11b8cc512ce2b45b1b053e8f0e87ba9157a796f5653c20ddfef5af89a18e4"
                  , signature = Bytes.fromHexUnchecked "6e2b18dde52deabb476ae171ec56b3620c63c0d6a00069f3bd7160ca0af7dd923cbf1ff1b414cb88d824b843b97fce5df70b477fad1fafd62aa1a2556e13f207"
                  , chainCode = Bytes.fromHexUnchecked "52fbfa9e0aca2281d1ef0359f9880b381e8b045273c59c9827f9762e939e768c"
                  , attributes = Bytes.fromHexUnchecked "a101581e581c8d9b2a782a9b390866febb68228dd65728c218e7fd86142c4d2736b3"
                  }
                , { publicKey = Bytes.fromHexUnchecked "f5c382061f453618c6c957bb5345860b2736fee4fc29133215134fd92a26d03f"
                  , signature = Bytes.fromHexUnchecked "d26bb37ef4efc404c59865b4f76d51d76c2c3d659430e74f7566884d1cffe5061da326be3aaf54dfddb9e89d69e31289dbee63dabbc5a71ea279bf7b10f83007"
                  , chainCode = Bytes.fromHexUnchecked "3ab2c5f5262ff16de9fe44821853b266d599906090662c40785355931d965f0c"
                  , attributes = Bytes.fromHexUnchecked "a101581e581c8d9b2a782a9b392ef1c4a1683cff72b7b6a02cd5dca8afb5c1a15973"
                  }
                ]
    }


{-| Next Shelley failure.

Tx id: 871b14fbe5abb6cacc63f922187c4f10ea9499055a972eb5d3d4e8771af643df
Block height: 4490537
Previous block intersection:

  - slot: 4493320
  - id: c7da3ab6cc9cac8044d391552acbf72ad0b8d70b1db90ea3c2264418acdac436

-}
decode871b14fb : Test
decode871b14fb =
    test "Tx id 871b14fbe5abb6cacc63f922187c4f10ea9499055a972eb5d3d4e8771af643df" <|
        \_ ->
            Bytes.fromHexUnchecked "83a400818258209a822a5601a29f7a880948cf3b6491c24d861df18dbbe6ea2ba293f9878f965f0001828258390184f8618344721d55a4dd743a08e9628aa098c0c056bcc0ae794a992444adc04f00e3f9af407f93763dec952c12b7e9249a5e98ecd7baa9f11b00000068eb1078688258390149d3b2a1cc633fd909591be7cef70d5fe0b2729620d6dd3aac2e54650e5b086df87a2a0c5c398b41d413f84176c527da5e5cb641f45988441a000f4240021a00029201031a0044ac28a100818258200607a454923b9bd5fec2897ce7f2b9ca2874ee545d750624084ba0fc9ef06dd558408c5a9d2ab5343b70afdb9a6b6580c1aea3dc4132968620ef21fdf14f6fd2a97601d8c2e9cc0d7596560c91ae56e9ddc03bbb5efebb6163c59781fafd28d4580af6"
                |> Transaction.deserialize
                |> Expect.equal
                    (Just
                        { body = txBody871b14fb
                        , witnessSet = txWitnessSet871b14fb
                        , isValid = True
                        , auxiliaryData = Nothing
                        }
                    )


txBody871b14fb : TransactionBody
txBody871b14fb =
    { newBody
        | inputs =
            [ { transactionId = Bytes.fromHexUnchecked "9a822a5601a29f7a880948cf3b6491c24d861df18dbbe6ea2ba293f9878f965f"
              , outputIndex = 0
              }
            ]
        , outputs =
            [ { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromHexUnchecked "84f8618344721d55a4dd743a08e9628aa098c0c056bcc0ae794a9924")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromHexUnchecked "44adc04f00e3f9af407f93763dec952c12b7e9249a5e98ecd7baa9f1")))
                        }
              , amount = Value.onlyLovelace (N.fromSafeInt 450620323944)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            , { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromHexUnchecked "49d3b2a1cc633fd909591be7cef70d5fe0b2729620d6dd3aac2e5465")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromHexUnchecked "0e5b086df87a2a0c5c398b41d413f84176c527da5e5cb641f4598844")))
                        }
              , amount = Value.onlyLovelace (N.fromSafeInt 1000000)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            ]
        , fee = N.fromSafeInt 168449
        , ttl = Just (N.fromSafeInt 4500520)
    }


txWitnessSet871b14fb : Transaction.WitnessSet
txWitnessSet871b14fb =
    { newWitnessSet
        | vkeywitness =
            Just
                [ { vkey = Bytes.fromHexUnchecked "0607a454923b9bd5fec2897ce7f2b9ca2874ee545d750624084ba0fc9ef06dd5"
                  , signature = Bytes.fromHexUnchecked "8c5a9d2ab5343b70afdb9a6b6580c1aea3dc4132968620ef21fdf14f6fd2a97601d8c2e9cc0d7596560c91ae56e9ddc03bbb5efebb6163c59781fafd28d4580a"
                  }
                ]
    }


{-| Next Shelley failure.

Tx id: f3a0835d9359ed79f8301ba61ff263188c180ffd6dfddaba60a7e31b8366c38e
Block height: 4490544
Previous block intersection:

  - slot: 4493460
  - id: afc9552e4a47d6d1a900516749686457b8c864bc4d61c87345db4a3f7ebeb70c

-}
decodef3a0835d : Test
decodef3a0835d =
    test "Tx id f3a0835d9359ed79f8301ba61ff263188c180ffd6dfddaba60a7e31b8366c38e" <|
        \_ ->
            Bytes.fromHexUnchecked "83a50081825820c0810285e7cffd0ea65851008392d41dd4cdf223d9263ca7a33e28a7e7b410b80001818258390149d3b2a1cc633fd909591be7cef70d5fe0b2729620d6dd3aac2e54650e5b086df87a2a0c5c398b41d413f84176c527da5e5cb641f45988441b00000068eaea4873021a0002ba0d031a0044b790048182008200581c0e5b086df87a2a0c5c398b41d413f84176c527da5e5cb641f4598844a10082825820473f36674fcde1ff195076774decda62f4b0ba860f9fcc0c51d63abee8b1e1285840e52151c3a0e54701b9917c1bd21e0e0b1724a474229809308af9b69f83ee3cd1f1377d88166edfcfc159e2441dc272c60700b1f60452910686b88e020af1ba048258209b718dfb7f2b75ac0a9ab4c9fccaa2befdeecdfde9a0970cbc3f41776ca1939558409d386af0ae47af0521316af8cd7d25984dec0d85d00cd2c9c3571ef48fc26a5db7c9056126a8cc6769e72ecd39a0808f33de8680e5997b5ba4169f7007b5c101f6"
                |> Transaction.deserialize
                |> Expect.equal
                    (Just
                        { body = txBodyf3a0835d
                        , witnessSet = txWitnessSetf3a0835d
                        , isValid = True
                        , auxiliaryData = Nothing
                        }
                    )


txBodyf3a0835d : TransactionBody
txBodyf3a0835d =
    { newBody
        | inputs =
            [ { transactionId = Bytes.fromHexUnchecked "c0810285e7cffd0ea65851008392d41dd4cdf223d9263ca7a33e28a7e7b410b8"
              , outputIndex = 0
              }
            ]
        , outputs =
            [ { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromHexUnchecked "49d3b2a1cc633fd909591be7cef70d5fe0b2729620d6dd3aac2e5465")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromHexUnchecked "0e5b086df87a2a0c5c398b41d413f84176c527da5e5cb641f4598844")))
                        }
              , amount = Value.onlyLovelace (N.fromSafeInt 450617821299)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            ]
        , fee = N.fromSafeInt 178701
        , ttl = Just (N.fromSafeInt 4503440)
        , certificates =
            [ Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "0e5b086df87a2a0c5c398b41d413f84176c527da5e5cb641f4598844") }
            ]
    }


txWitnessSetf3a0835d : Transaction.WitnessSet
txWitnessSetf3a0835d =
    { newWitnessSet
        | vkeywitness =
            Just
                [ { vkey = Bytes.fromHexUnchecked "473f36674fcde1ff195076774decda62f4b0ba860f9fcc0c51d63abee8b1e128"
                  , signature = Bytes.fromHexUnchecked "e52151c3a0e54701b9917c1bd21e0e0b1724a474229809308af9b69f83ee3cd1f1377d88166edfcfc159e2441dc272c60700b1f60452910686b88e020af1ba04"
                  }
                , { vkey = Bytes.fromHexUnchecked "9b718dfb7f2b75ac0a9ab4c9fccaa2befdeecdfde9a0970cbc3f41776ca19395"
                  , signature = Bytes.fromHexUnchecked "9d386af0ae47af0521316af8cd7d25984dec0d85d00cd2c9c3571ef48fc26a5db7c9056126a8cc6769e72ecd39a0808f33de8680e5997b5ba4169f7007b5c101"
                  }
                ]
    }


{-| Next Shelley failure.

Tx id: 841cca81da918feb9fa7257a34630eac95794be712ed3faae6df64f215ce25f2
Block height: 4490550
Previous block intersection:

  - slot: 4493580
  - id: af4fbe98c50ed01fb0b3f0ed2833fd2a3809f4ca2e367fe71665846e599e96fe

-}
decode841cca81 : Test
decode841cca81 =
    test "Tx id 841cca81da918feb9fa7257a34630eac95794be712ed3faae6df64f215ce25f2" <|
        \_ ->
            Bytes.fromHexUnchecked "83a50081825820f3a0835d9359ed79f8301ba61ff263188c180ffd6dfddaba60a7e31b8366c38e0001818258390149d3b2a1cc633fd909591be7cef70d5fe0b2729620d6dd3aac2e54650e5b086df87a2a0c5c398b41d413f84176c527da5e5cb641f45988441b00000068cd19f78e021a0002ebe5031a0044b81c04828a03581c153806dbcd134ddee69a8c5204e38ac80448f62342f8c23cfe4b7edf58200220a5d08adbfe9554b52d7b2993be5892ac3ff340e674a377dea3e22ad1778b1b00000068c61714001a1443fd00d81e820318c8581de10e5b086df87a2a0c5c398b41d413f84176c527da5e5cb641f459884481581c0e5b086df87a2a0c5c398b41d413f84176c527da5e5cb641f4598844818400190bba4436dc1428f682783f68747470733a2f2f7261772e67697468756275736572636f6e74656e742e636f6d2f4f6374616c75732f63617264616e6f2f6d61737465722f702e6a736f6e5820ca7d12decf886e31f5226b5946c62edc81a7e40af95ce7cd6465122e309d562683028200581c0e5b086df87a2a0c5c398b41d413f84176c527da5e5cb641f4598844581c153806dbcd134ddee69a8c5204e38ac80448f62342f8c23cfe4b7edfa10083825820473f36674fcde1ff195076774decda62f4b0ba860f9fcc0c51d63abee8b1e1285840fabb3bdeb0c845517bfb6af55299f08cb4e41e648a032e193274315a0fe027728162b63c4b78729281cd2d9340c4df2b01bb06b4a4eff5bbe0942852fbd5c10b825820e2e08f93a46e151637138038990d37f6891d4620b668891bb89de56f46ffd48758407d487a26ebfc66501b43b64f386ae31c82ee9b9c3108fe52ed5ddf9473f4f719a367522ee048136c68a7ffdd90ded3cad316f0aa4a0bc49eacab11ea9ef4af028258209b718dfb7f2b75ac0a9ab4c9fccaa2befdeecdfde9a0970cbc3f41776ca1939558400b1bcde4d965a7a756121e2335b3a31c83fc4ef800c2f576764539f384e007ec6af1ae7bfe108c68f813d884e066a43f42be2bd2eb8dba802d100d8335f7db09f6"
                |> Transaction.deserialize
                |> Expect.equal
                    (Just
                        { body = txBody841cca81
                        , witnessSet = txWitnessSet841cca81
                        , isValid = True
                        , auxiliaryData = Nothing
                        }
                    )


txBody841cca81 : TransactionBody
txBody841cca81 =
    { newBody
        | inputs =
            [ { transactionId = Bytes.fromHexUnchecked "f3a0835d9359ed79f8301ba61ff263188c180ffd6dfddaba60a7e31b8366c38e"
              , outputIndex = 0
              }
            ]
        , outputs =
            [ { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromHexUnchecked "49d3b2a1cc633fd909591be7cef70d5fe0b2729620d6dd3aac2e5465")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromHexUnchecked "0e5b086df87a2a0c5c398b41d413f84176c527da5e5cb641f4598844")))
                        }
              , amount = Value.onlyLovelace (N.fromSafeInt 450117629838)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            ]
        , fee = N.fromSafeInt 191461
        , ttl = Just (N.fromSafeInt 4503580)
        , certificates =
            [ Transaction.PoolRegistrationCert
                { operator = Bytes.fromHexUnchecked "153806dbcd134ddee69a8c5204e38ac80448f62342f8c23cfe4b7edf"
                , vrfKeyHash = Bytes.fromHexUnchecked "0220a5d08adbfe9554b52d7b2993be5892ac3ff340e674a377dea3e22ad1778b"
                , pledge = N.fromSafeString "450000000000"
                , cost = N.fromSafeInt 340000000
                , margin = { denominator = 200, numerator = 3 }
                , rewardAccount =
                    { networkId = Mainnet
                    , stakeCredential = Address.VKeyHash (Bytes.fromHexUnchecked "0e5b086df87a2a0c5c398b41d413f84176c527da5e5cb641f4598844")
                    }
                , poolOwners = [ Bytes.fromHexUnchecked "0e5b086df87a2a0c5c398b41d413f84176c527da5e5cb641f4598844" ]
                , relays =
                    [ Pool.SingleHostAddr
                        { ipv4 = Just (Bytes.fromHexUnchecked "36dc1428")
                        , ipv6 = Nothing
                        , port_ = Just 3002
                        }
                    ]
                , poolMetadata =
                    Just
                        { poolMetadataHash = Bytes.fromHexUnchecked "ca7d12decf886e31f5226b5946c62edc81a7e40af95ce7cd6465122e309d5626"
                        , url = "https://raw.githubusercontent.com/Octalus/cardano/master/p.json"
                        }
                }
            , Transaction.StakeDelegationCert
                { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "0e5b086df87a2a0c5c398b41d413f84176c527da5e5cb641f4598844")
                , poolId = Bytes.fromHexUnchecked "153806dbcd134ddee69a8c5204e38ac80448f62342f8c23cfe4b7edf"
                }
            ]
    }


txWitnessSet841cca81 : WitnessSet
txWitnessSet841cca81 =
    { newWitnessSet
        | vkeywitness =
            Just
                [ { vkey = Bytes.fromHexUnchecked "473f36674fcde1ff195076774decda62f4b0ba860f9fcc0c51d63abee8b1e128"
                  , signature = Bytes.fromHexUnchecked "fabb3bdeb0c845517bfb6af55299f08cb4e41e648a032e193274315a0fe027728162b63c4b78729281cd2d9340c4df2b01bb06b4a4eff5bbe0942852fbd5c10b"
                  }
                , { vkey = Bytes.fromHexUnchecked "e2e08f93a46e151637138038990d37f6891d4620b668891bb89de56f46ffd487"
                  , signature = Bytes.fromHexUnchecked "7d487a26ebfc66501b43b64f386ae31c82ee9b9c3108fe52ed5ddf9473f4f719a367522ee048136c68a7ffdd90ded3cad316f0aa4a0bc49eacab11ea9ef4af02"
                  }
                , { vkey = Bytes.fromHexUnchecked "9b718dfb7f2b75ac0a9ab4c9fccaa2befdeecdfde9a0970cbc3f41776ca19395"
                  , signature = Bytes.fromHexUnchecked "0b1bcde4d965a7a756121e2335b3a31c83fc4ef800c2f576764539f384e007ec6af1ae7bfe108c68f813d884e066a43f42be2bd2eb8dba802d100d8335f7db09"
                  }
                ]
    }


{-| Next Shelley failure.

Tx id: 896cf8fefad1eaf0fa056ba3adf28bfb26b06d1beed64cf790deb595dcb2687a
Block height: 4491210
Previous block intersection:

  - slot: 4506780
  - id: 13bacb73a31ce721831bd82810ed0bc43b7d2190fcad72d0c53b6cc618763dbd

-}
decode896cf8fe : Test
decode896cf8fe =
    test "Tx id 896cf8fefad1eaf0fa056ba3adf28bfb26b06d1beed64cf790deb595dcb2687a" <|
        \_ ->
            Bytes.fromHexUnchecked "83a50081825820e1d1d61ecd706790ee31a1c07fc87827f7b9738d374ef705d1c8cff8295c8cf0000181825839016a8aba085ef5781bf8ea58c5e92408c0bfba7bcc7ca84da90dffcf90d33cabe9bc7a7646243c03f062881d06744b3d53983823178973b9b01b0000000be2a11012021a0002bb6d031a0044c88404818304581c469fbad36a8a68c5f62f3505ecbe6a461ea262ae20b4de8d39ff08ab18d1a100828258206f0d127f3f5122171fec5f91e85df66bac1314680f703abf5caefddffd55a2855840dbf9707786dc151be98e0c189a879d397d9b0aa5bdaad2ff4831fcf53e04651eb1028e78da8f315680056e5435e8bf48917c5cb545eeb0039e5b57a205609f0b82582085640c4b0cb50c31b797c26a8745fc9c9fea7d90dbc3ae241971c141c43ef59758402d0423835dbd83f2118916ce403b2492027f8a4b10752ab5b59a787081af0a547d5c0d72efc521022fd1371a53f00de87ab3816206ba59009530a6bd4cc40f05f6"
                |> Transaction.deserialize
                |> Expect.equal
                    (Just
                        { body = txBody896cf8fe
                        , witnessSet = txWitnessSet896cf8fe
                        , isValid = True
                        , auxiliaryData = Nothing
                        }
                    )


txBody896cf8fe : TransactionBody
txBody896cf8fe =
    { newBody
        | inputs =
            [ { transactionId = Bytes.fromHexUnchecked "e1d1d61ecd706790ee31a1c07fc87827f7b9738d374ef705d1c8cff8295c8cf0"
              , outputIndex = 0
              }
            ]
        , outputs =
            [ { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromHexUnchecked "6a8aba085ef5781bf8ea58c5e92408c0bfba7bcc7ca84da90dffcf90")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromHexUnchecked "d33cabe9bc7a7646243c03f062881d06744b3d53983823178973b9b0")))
                        }
              , amount = Value.onlyLovelace (N.fromSafeInt 51046846482)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            ]
        , fee = N.fromSafeInt 179053
        , ttl = Just (N.fromSafeInt 4507780)
        , certificates =
            [ Transaction.PoolRetirementCert
                { poolId = Bytes.fromHexUnchecked "469fbad36a8a68c5f62f3505ecbe6a461ea262ae20b4de8d39ff08ab"
                , epoch = N.fromSafeInt 209
                }
            ]
    }


txWitnessSet896cf8fe : WitnessSet
txWitnessSet896cf8fe =
    { newWitnessSet
        | vkeywitness =
            Just
                [ { vkey = Bytes.fromHexUnchecked "6f0d127f3f5122171fec5f91e85df66bac1314680f703abf5caefddffd55a285"
                  , signature = Bytes.fromHexUnchecked "dbf9707786dc151be98e0c189a879d397d9b0aa5bdaad2ff4831fcf53e04651eb1028e78da8f315680056e5435e8bf48917c5cb545eeb0039e5b57a205609f0b"
                  }
                , { vkey = Bytes.fromHexUnchecked "85640c4b0cb50c31b797c26a8745fc9c9fea7d90dbc3ae241971c141c43ef597"
                  , signature = Bytes.fromHexUnchecked "2d0423835dbd83f2118916ce403b2492027f8a4b10752ab5b59a787081af0a547d5c0d72efc521022fd1371a53f00de87ab3816206ba59009530a6bd4cc40f05"
                  }
                ]
    }


{-| Next Shelley failure.

Tx id: 3dd8be52247b1cfcdc9cd07ef994aabe5c5455c649b0ce28ad101f1c940df0a6
Block height: 4491354
Previous block intersection:

  - slot: 4509660
  - id: c672e5c15afe9cdcb9035e5ca581c263080ba88d689ca8adfbda13503d59a9e2

-}
decode3dd8be52 : Test
decode3dd8be52 =
    test "Tx id 3dd8be52247b1cfcdc9cd07ef994aabe5c5455c649b0ce28ad101f1c940df0a6" <|
        \_ ->
            Bytes.fromHexUnchecked "83a500818258202f744483e15d31c9a3e7407970bb01f9a4542a6cf3ab6d921477cadbc865aa670101818258390158340a42cb6d376ab4b9528767f234330cfb529d7c58dace948bb83988fdf9622c4619c0109132410e76c272013ef07f786e8f2cc24379601b000000746a86b675021a00036b15031a0044f6d804848a03581c3dc5218de6bf5a29ad6203d9da31b7128c414872e53b8f09767764d458208655a2c115f703f5ffe2eeed634e7f0347c73c90ffcc359d2df7940d526579381b10a741a4627800001a1443fd00d81e820718c8581de1bc000424731d5cfdb41da8bbb2e8bfbdcf05becd055a3f831a0ccdfa83581c09799da84e15b9a32da2bb284586f32fe357afdccecd2fcb8f88dbb5581c88fdf9622c4619c0109132410e76c272013ef07f786e8f2cc2437960581cd1290c017327065ac2279e989735990efac7539823eab4370efde3238284001915294433514501f684001915294433514502f682783468747470733a2f2f756e697465647374616b65736f6663617264616e6f2e696f2f75736130312e6d657461646174612e6a736f6e5820d6bed5fb3aea9ce8b2829859704728ef2987da4b2be7ce07607799a3485ed94983028200581cd1290c017327065ac2279e989735990efac7539823eab4370efde323581c3dc5218de6bf5a29ad6203d9da31b7128c414872e53b8f09767764d483028200581c09799da84e15b9a32da2bb284586f32fe357afdccecd2fcb8f88dbb5581c3dc5218de6bf5a29ad6203d9da31b7128c414872e53b8f09767764d483028200581c88fdf9622c4619c0109132410e76c272013ef07f786e8f2cc2437960581c3dc5218de6bf5a29ad6203d9da31b7128c414872e53b8f09767764d4a10086825820643cb214fcfd8886e744e0fdf2ccefe89001c83366ba077394c6d4930158577858406a368d10744c8ca8d84e3f664a1e0aac6b3c5e53bd6b32f5fa9490213310eb6b5d4c5d9487cd6705dc11b44558a3df9d0538a6f8fc05858c9fa40d48d8dc560a8258200df5d54d4e7e95c2ff9b9a3b24b93b8580a1716a522169edf3e07cf0b0e77f6758409f99ffce567794e750e61f410497e6de4d2e7a0fd8e5a8bf3ef5ad6f4b0f1210260220dcedc1443bc48255a72058007d9a4215718f06c73cee914330e1c28f0a825820ddbe1fe594b884d6fb3fe420927a982ed6d5707fb43b59eee0111e0368ffc6ff58400fdc80b27449d5a3e4f9e78c893bce9b10e0fa8c64e70211af77c6df8661b708066d108f28117e1ec3e37d594ebacf4c4c73b1e2dcbdb85b7dd1fec4e75c6605825820e6edd6de6fd75832cdd7429c4eeee5d1fbac17f4306d5b0466d5b4459d0b7e645840f15f4dcf625891e883b714b066aa2b8cbd8a050133b2f0b50164ed928cbf127420ccfd2f59eb32e7ba5a34d65b165807e87cf7f09bfafc4c4d388ed8163f1e08825820c9fbdb7aed0d908da7e3a2d479743233a21b0fdc3b40028ff01c6a590f85a79c58404559a292949757c09daca5eb6dcab3e4e077906fc556445977cd5f5fafb4ef77c64c642a2bd3aefdb5fc2fb6c0249a1b999cc681acdd77ce25627f2850656c01825820d83ff6f61e037f091ab2ce1d5fd925c06f2ea7b0e2d10802ef09c948923650d758408700a2b72d80d6be994a54fc54bc4f212e76a3572b0e6bf8b7b1c1efd99bf348c6a628160b2fcd0c0c7d3e6fa6c6b27a3d2123d9e33b0036abd9118536787b02f6"
                |> Transaction.deserialize
                |> Expect.equal
                    (Just
                        { body = txBody3dd8be52
                        , witnessSet = txWitnessSet3dd8be52
                        , isValid = True
                        , auxiliaryData = Nothing
                        }
                    )


txBody3dd8be52 : TransactionBody
txBody3dd8be52 =
    { newBody
        | inputs = [ { outputIndex = 1, transactionId = Bytes.fromHexUnchecked "2f744483e15d31c9a3e7407970bb01f9a4542a6cf3ab6d921477cadbc865aa67" } ]
        , outputs =
            [ { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromHexUnchecked "58340a42cb6d376ab4b9528767f234330cfb529d7c58dace948bb839")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromHexUnchecked "88fdf9622c4619c0109132410e76c272013ef07f786e8f2cc2437960")))
                        }
              , amount = Value.onlyLovelace (N.fromSafeInt 500003419765)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            ]
        , fee = N.fromSafeInt 224021
        , ttl = Just (N.fromSafeInt 4519640)
        , certificates =
            [ Transaction.PoolRegistrationCert
                { cost = N.fromSafeInt 340000000
                , margin = { denominator = 200, numerator = 7 }
                , operator = Bytes.fromHexUnchecked "3dc5218de6bf5a29ad6203d9da31b7128c414872e53b8f09767764d4"
                , pledge = N.fromSafeString "1200000000000000000"
                , poolMetadata = Just { poolMetadataHash = Bytes.fromHexUnchecked "d6bed5fb3aea9ce8b2829859704728ef2987da4b2be7ce07607799a3485ed949", url = "https://unitedstakesofcardano.io/usa01.metadata.json" }
                , poolOwners = [ Bytes.fromHexUnchecked "09799da84e15b9a32da2bb284586f32fe357afdccecd2fcb8f88dbb5", Bytes.fromHexUnchecked "88fdf9622c4619c0109132410e76c272013ef07f786e8f2cc2437960", Bytes.fromHexUnchecked "d1290c017327065ac2279e989735990efac7539823eab4370efde323" ]
                , relays = [ Pool.SingleHostAddr { ipv4 = Just (Bytes.fromHexUnchecked "33514501"), ipv6 = Nothing, port_ = Just 5417 }, Pool.SingleHostAddr { ipv4 = Just (Bytes.fromHexUnchecked "33514502"), ipv6 = Nothing, port_ = Just 5417 } ]
                , rewardAccount = { networkId = Mainnet, stakeCredential = Address.VKeyHash (Bytes.fromHexUnchecked "bc000424731d5cfdb41da8bbb2e8bfbdcf05becd055a3f831a0ccdfa") }
                , vrfKeyHash = Bytes.fromHexUnchecked "8655a2c115f703f5ffe2eeed634e7f0347c73c90ffcc359d2df7940d52657938"
                }
            , Transaction.StakeDelegationCert
                { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "d1290c017327065ac2279e989735990efac7539823eab4370efde323")
                , poolId = Bytes.fromHexUnchecked "3dc5218de6bf5a29ad6203d9da31b7128c414872e53b8f09767764d4"
                }
            , Transaction.StakeDelegationCert
                { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "09799da84e15b9a32da2bb284586f32fe357afdccecd2fcb8f88dbb5")
                , poolId = Bytes.fromHexUnchecked "3dc5218de6bf5a29ad6203d9da31b7128c414872e53b8f09767764d4"
                }
            , Transaction.StakeDelegationCert
                { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "88fdf9622c4619c0109132410e76c272013ef07f786e8f2cc2437960")
                , poolId = Bytes.fromHexUnchecked "3dc5218de6bf5a29ad6203d9da31b7128c414872e53b8f09767764d4"
                }
            ]
    }


txWitnessSet3dd8be52 : WitnessSet
txWitnessSet3dd8be52 =
    { newWitnessSet
        | vkeywitness =
            Just
                [ { signature = Bytes.fromHexUnchecked "6a368d10744c8ca8d84e3f664a1e0aac6b3c5e53bd6b32f5fa9490213310eb6b5d4c5d9487cd6705dc11b44558a3df9d0538a6f8fc05858c9fa40d48d8dc560a"
                  , vkey = Bytes.fromHexUnchecked "643cb214fcfd8886e744e0fdf2ccefe89001c83366ba077394c6d49301585778"
                  }
                , { signature = Bytes.fromHexUnchecked "9f99ffce567794e750e61f410497e6de4d2e7a0fd8e5a8bf3ef5ad6f4b0f1210260220dcedc1443bc48255a72058007d9a4215718f06c73cee914330e1c28f0a"
                  , vkey = Bytes.fromHexUnchecked "0df5d54d4e7e95c2ff9b9a3b24b93b8580a1716a522169edf3e07cf0b0e77f67"
                  }
                , { signature = Bytes.fromHexUnchecked "0fdc80b27449d5a3e4f9e78c893bce9b10e0fa8c64e70211af77c6df8661b708066d108f28117e1ec3e37d594ebacf4c4c73b1e2dcbdb85b7dd1fec4e75c6605"
                  , vkey = Bytes.fromHexUnchecked "ddbe1fe594b884d6fb3fe420927a982ed6d5707fb43b59eee0111e0368ffc6ff"
                  }
                , { signature = Bytes.fromHexUnchecked "f15f4dcf625891e883b714b066aa2b8cbd8a050133b2f0b50164ed928cbf127420ccfd2f59eb32e7ba5a34d65b165807e87cf7f09bfafc4c4d388ed8163f1e08"
                  , vkey = Bytes.fromHexUnchecked "e6edd6de6fd75832cdd7429c4eeee5d1fbac17f4306d5b0466d5b4459d0b7e64"
                  }
                , { signature = Bytes.fromHexUnchecked "4559a292949757c09daca5eb6dcab3e4e077906fc556445977cd5f5fafb4ef77c64c642a2bd3aefdb5fc2fb6c0249a1b999cc681acdd77ce25627f2850656c01"
                  , vkey = Bytes.fromHexUnchecked "c9fbdb7aed0d908da7e3a2d479743233a21b0fdc3b40028ff01c6a590f85a79c"
                  }
                , { signature = Bytes.fromHexUnchecked "8700a2b72d80d6be994a54fc54bc4f212e76a3572b0e6bf8b7b1c1efd99bf348c6a628160b2fcd0c0c7d3e6fa6c6b27a3d2123d9e33b0036abd9118536787b02"
                  , vkey = Bytes.fromHexUnchecked "d83ff6f61e037f091ab2ce1d5fd925c06f2ea7b0e2d10802ef09c948923650d7"
                  }
                ]
    }


{-| Next Shelley failure.

Tx id: 3c03090c2f54a0f678ed03ef6545b404a1e9482d0f715638c3c85be40e688423
Block height: 4492234
Previous block intersection:

  - slot: 4527260
  - id: 23e88dc5fd58fa910b0ca51d0227b359cbe1dbb01dfd685afbcde5ddf03f22f9

-}
decode3c03090c : Test
decode3c03090c =
    test "Tx id 3c03090c2f54a0f678ed03ef6545b404a1e9482d0f715638c3c85be40e688423" <|
        \_ ->
            Bytes.fromHexUnchecked "83a50081825820f9785983a5480c77407ad35b2e533256f26859a99567e4259ed5a5d6d7b002380001818258390115aba9c73a963934db35eecce266137e58a8f07224bfaea624f0371414ae6f9c8def33a4c0f9704eed2b16afe8b9f8b501f8ac6794bed2de1a11b486f2021a0002cd79031a0045165404828a03581c490353aa6b85efb28922acd9e0ee1dcf6d0c269b9f0583718b0274ba582076be64e699141b87885a9fb2570cb8f84d308fc48e44d906bc4852afe345ce701b00000045d964b8001a1443fd00d81e82131903e8581de114ae6f9c8def33a4c0f9704eed2b16afe8b9f8b501f8ac6794bed2de81581c14ae6f9c8def33a4c0f9704eed2b16afe8b9f8b501f8ac6794bed2de80f683028200581c14ae6f9c8def33a4c0f9704eed2b16afe8b9f8b501f8ac6794bed2de581c490353aa6b85efb28922acd9e0ee1dcf6d0c269b9f0583718b0274baa10083825820e934912569b9a308ef190137e09726a2b09610f3f95be89ba5bc80f7aded804b584072bbd8363b2da559d75722353ae5a0791cd76ca804718c2c4c99a37c7c2add4b8c4fe0ba4d1fa80f837d595b8404b12f6301a3d73b8b88f6f7b811c60cf67b08825820af3afdfa62d84da02ee03f87143a49e9766dc20d0bc3758ccf4bbed26928ef315840e5fa09f4440566513808e5cf7ef87891dc8e8cc03b252b993031692853e3fd8092dc571cb8826af0234aa416e071600a3961e7e2f01b5594961f4814dace240a8258209e8cc888dd0fa99d0dc588b5c5793dbb443dbb8531b18e7f4adeb8e2f548a0535840dc30a47b0a188aa1450e8b7108cdc00ceedc4c4e1bbe013d840da3c22f5c06f87a9c992cc047a8d935ed576cd2c659a9989914727d9556bfa158cc783a894605f6"
                |> Transaction.deserialize
                |> Expect.equal
                    (Just
                        { body = txBody3c03090c
                        , witnessSet = txWitnessSet3c03090c
                        , isValid = True
                        , auxiliaryData = Nothing
                        }
                    )


txBody3c03090c : TransactionBody
txBody3c03090c =
    { newBody
        | inputs = [ { outputIndex = 0, transactionId = Bytes.fromHexUnchecked "f9785983a5480c77407ad35b2e533256f26859a99567e4259ed5a5d6d7b00238" } ]
        , outputs =
            [ { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromHexUnchecked "15aba9c73a963934db35eecce266137e58a8f07224bfaea624f03714")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromHexUnchecked "14ae6f9c8def33a4c0f9704eed2b16afe8b9f8b501f8ac6794bed2de")))
                        }
              , amount = Value.onlyLovelace (N.fromSafeInt 297043698)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            ]
        , fee = N.fromSafeInt 183673
        , ttl = Just (N.fromSafeInt 4527700)
        , certificates =
            [ Transaction.PoolRegistrationCert
                { cost = N.fromSafeInt 340000000
                , margin = { denominator = 1000, numerator = 19 }
                , operator = Bytes.fromHexUnchecked "490353aa6b85efb28922acd9e0ee1dcf6d0c269b9f0583718b0274ba"
                , pledge = N.fromSafeInt 300000000000
                , poolMetadata = Nothing
                , poolOwners = [ Bytes.fromHexUnchecked "14ae6f9c8def33a4c0f9704eed2b16afe8b9f8b501f8ac6794bed2de" ]
                , relays = []
                , rewardAccount = { networkId = Mainnet, stakeCredential = Address.VKeyHash (Bytes.fromHexUnchecked "14ae6f9c8def33a4c0f9704eed2b16afe8b9f8b501f8ac6794bed2de") }
                , vrfKeyHash = Bytes.fromHexUnchecked "76be64e699141b87885a9fb2570cb8f84d308fc48e44d906bc4852afe345ce70"
                }
            , Transaction.StakeDelegationCert
                { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "14ae6f9c8def33a4c0f9704eed2b16afe8b9f8b501f8ac6794bed2de")
                , poolId = Bytes.fromHexUnchecked "490353aa6b85efb28922acd9e0ee1dcf6d0c269b9f0583718b0274ba"
                }
            ]
    }


txWitnessSet3c03090c : WitnessSet
txWitnessSet3c03090c =
    { newWitnessSet
        | vkeywitness =
            Just
                [ { signature = Bytes.fromHexUnchecked "72bbd8363b2da559d75722353ae5a0791cd76ca804718c2c4c99a37c7c2add4b8c4fe0ba4d1fa80f837d595b8404b12f6301a3d73b8b88f6f7b811c60cf67b08"
                  , vkey = Bytes.fromHexUnchecked "e934912569b9a308ef190137e09726a2b09610f3f95be89ba5bc80f7aded804b"
                  }
                , { signature = Bytes.fromHexUnchecked "e5fa09f4440566513808e5cf7ef87891dc8e8cc03b252b993031692853e3fd8092dc571cb8826af0234aa416e071600a3961e7e2f01b5594961f4814dace240a"
                  , vkey = Bytes.fromHexUnchecked "af3afdfa62d84da02ee03f87143a49e9766dc20d0bc3758ccf4bbed26928ef31"
                  }
                , { signature = Bytes.fromHexUnchecked "dc30a47b0a188aa1450e8b7108cdc00ceedc4c4e1bbe013d840da3c22f5c06f87a9c992cc047a8d935ed576cd2c659a9989914727d9556bfa158cc783a894605"
                  , vkey = Bytes.fromHexUnchecked "9e8cc888dd0fa99d0dc588b5c5793dbb443dbb8531b18e7f4adeb8e2f548a053"
                  }
                ]
    }


{-| Next Shelley failure.

Tx id: 35d2728ea6ad89bf809565c9ed698bb1c5cddf83591ba2e8bba951cb8fee0035
Block height: 4494062
Previous block intersection:

  - slot: 4563820
  - id: 78f46f9ceff8741ce744bfb6d6b4d7715963b08503f9a32ebb639acd6487362d

-}
decode35d2728e : Test
decode35d2728e =
    test "Tx id 35d2728ea6ad89bf809565c9ed698bb1c5cddf83591ba2e8bba951cb8fee0035" <|
        \_ ->
            Bytes.fromHexUnchecked "83a500818258202dcf5b56aa5d63bd346b1196b8dadc3bb32cafa3a7e080b04339d105e4637d1700018182581d61d80fe69ded1ff90f41e526d0332a2ff98ba8a0d85ceb8941b51784201b0000000f0afe195b021a000dbba0031a00989680049f82068200bf8200581c00de27c8609df0a7f7ae090224ace1d68cc5d338a335f1c6bb583d721a82c151d18200581c017a39dc9198c69f55e61e9edfcd4506d38c41703d7454cf4cb6a8821a18d1ceab8200581c0477455a3a4a0b87c1be90c77c6b376841f8f4c52e42b47691baac201adb33ca2f8200581c04b4af2f4536466c9d0010ea94c7319db6eff7a97e7f26cdb1e773d41b000000015934add28200581c058b6e56f91b7c34e367fa2fd65a64b7a679bd7bfa62e36e210d80d81b000000028093f4288200581c06cac706110395e66ac8913077e093539bbdcab1313433bf503b60c61b000000024b78a3148200581c06cf8d573a5d9ff295eafcc48938212c9296871d13ff905db1a604f11a461cf6af8200581c090a63ca4e67569d49e7e9dc499ebeb847d8e60fb37d1963581368c61b00000002098ad30a8200581c0bf0271768bb0cb210d400f5352a578121aba403c5ec00405a2948801b0000000335dc588c8200581c0cf972e71a248dc9a59bad9b2244bbc82f89060352e28b22ac8d46551b0000002c2dddf50c8200581c1010834c8e3f8d7b6eb04a9181f0ff40e780429dd513662bf32c9c801b0000004991302ea88200581c13ce7ba99b254954ddc361445ff6bb3673320bd3d5da3615d9277b191b0000000314b15db28200581c13ee747308779b78902d5f7b6e7608b9523885d9b7c500db0459b73e1b000000057f8692518200581c17fac4618ea56ec6e12e9f6f818e8af5d98ea18c1681ba78aac44e1e1b0000000416c9197f8200581c19a6a21c3411bb8065859b2c91f74331f8d5a036b3b79184b6689ce41b0000006f0091f82f8200581c19ebdabf80584f3df8640922ab156ead84eafd23b89dde7e19d6a7fa1b000000015a9901758200581c1d1692b16cd0fbe005e45abb77383d02bee4ed86ef1eb2886659498d1b00000002a0057cb38200581c1d6a5923d8c491a898cb9de9ec1f84d6d3b4332c9610e4924b4cf5b71a02c5e7e78200581c1dfc6bf6eddfa4120690126942972974ed3a0a931c1dac6fd051cf8e1b00000001a62a9de38200581c1f69478b9a753e969d5085e40439993fc4358e94db8cc174ddddfa981a6fa982c68200581c21227adb44903d2de668696200ec709ee4f92d0009130123ef19c3c31b000000020d1ded668200581c22a7719fe0a30b4977467de59a04d7b5e9d56caff57e665db0ac9f811b000000015d1d7abe8200581c22abaff6207509c0bccdba86dceaeb3ef450452a98831d913c7cb53b1b00000006505e18598200581c233f410282657b90069dccdf0c03a78de31074869756f20bc417aebb1a941b00568200581c24074f532b2ae266b0ab5331b0d6a8383e31fa15145691b6d9f579981a803f47d18200581c24c7ee461f15e9cb6df13427b4685c01b5c5b5ef2954dc13e14aa74f1ad439f0408200581c2545642a9c1bfedf95bf30438849080aabc4219a227f209a84b72ee31ae5dcde0c8200581c25606e7ec45d235b74c5a0dcf2866929f955f1803a4d4da6b538e0391b00000003d12047bd8200581c27978020d4d87c686e3a37fd4cba304eb5d6af4ed9e6ab4457d5afcd1ab2dc623c8200581c2be784dfd7253aebb3e8750ec0d9ef33054269dbacf8364d854f41ef1a2d90650d8200581c2c459301ae68445b96991ca0eaef73d7869991f1c283af360e2208851a04ec2af88200581c2e54bf801f57a14174679d6772011bbff179188bc9b68e9e073a54721b0000001a5104abbf8200581c2fe954dd15471788fc832ac41415a185e21d0cfc6f5d94ffc9756e6d1a763311bb8200581c31b1f312087655265e40461acd2f4dda62cd69c0613570b2f91efc321b00000001a70c9b068200581c33682102a3202c26cebf11d6e360b96291c2dead24560914bc15e1e71b00000003d4ba1ab98200581c33e85b999cdd061ec49ea20228112dd92a108e21e067ec48a167b82f1b00000002a27380e18200581c3581b417908dea107b5abf821dcce6dd19f732af44ed6b6bb2d749651a27bb3aae8200581c35ef3f2e454f77519961026ecf25d29616db1b600c6d123d0e3a0b581b0000000159ddc5e78200581c36eafaae91e48b329f24eb2ca7bb71c8ca5ea41c9582e8500d4b63e51a2f1759c48200581c37557722eb102c5737b85f2332d5cd6baea6257ede6f0945389c03b11b000000015f9a34438200581c388ede6f04652931f309e1901e7cc8959e2809390ef4fb3f4f7a7dea1b000000032030a4e78200581c391825387fdff28dc24a4f2a7748d590ffbc4b4d8a8a6f0f836baf581a43b8d2188200581c39ff4977c8c3d37dba57d2044867650266f6e4c75c6a1665ac3765c61b0000002cc2fcada48200581c3c417ea0dc93cb8b1f8c92b2e473434bb20c18979e800f4ec531eebb1a1e08910a8200581c3f986f0b9fdc99a418c9a0f972f9c6b9454c5754a81bdccec40dc09d1b00000003825c64448200581c41e22a8c23ff59767ab1c6f4285350fb3c8b7bb3e5308d8cd98138171b0000000269e34f908200581c438ea0cc2a75da6a3d60708a5dd879a7b1d63891a81dc42bdd2908db1b00000001e374cde98200581c446e15940325aae70c01aa0c96d079bd68a0b2b1a6324879491210c91a5612c9968200581c45796e1560faa06b777598d53faf54cd8fb77169cbd48eb5731bb9201b00000002b90f59488200581c4597ec4cc1c4d012dc8d9a8c4320773d88b739449467b4f445d34bf41b0000000280c03ec88200581c499fb62d49ef0c0183e176166b543feaceb7af7f9074a17e078f71061a3e2e29c48200581c4d1307081d1362fb9e3debd029adac66d3ec947579905d266cccc5691a278b7e5d8200581c4d33eda5f702de4deed05ed99ee0a8bfc456742a4c8bfa4020f345481a042b1bdb8200581c4decd172f4102fffda2562fad82ce6926d386977d8e6d5c94907801b1b000000015828ac5c8200581c4e83fbacea4d13d1c985dbea19286c849a5ea04cb68a611c5ca3b3751a47216eb58200581c4ed709c5054447bf326aebb31ba47f4ddb82acf81f00172c37116cfc1a33aeacba8200581c4f8dda033362e9c8840206563925b9896263b103d3e45e9a0c28d4551a041214ea8200581c50756852527d09065c47572e9b29fb5a63bc3f393b6326cb19e34c211b00000001f32baaee8200581c5b728b0bcf80bf766688ea376c07f09d3dc9a265553f22b25b7542281b00000009aeafc25b8200581c5d3a2e1a6aee1687bde20b2b374ee427a8159b6b7cd1ff481037ea3e1b0000000200accbf08200581c5d5a181ff3fc68cf7361cba7b7382efff80470497d6e50eca7778b941b000000071ca0e3c38200581c5fba4a7603fbb18fb1d58c0c00dc01eb4b919f2ce7d1afc92a19e3991a2518332b8200581c62566a741704214440ef14350ec5ab1919f0e2f6dce0636f3d616c121b000000031ec941b38200581c62b3fade57272bfba2b7abd7877dabcf79b332a4b6fe373084ccd53e1b0000006439e863dd8200581c63181e6e8e5ee95f1c671cf38baee666721816efcb190c288e8fff9d1b000000049e15dc298200581c641ca9753c1d2ce01b5d1bd8d17196969558b9ceeb186f0f729ddd441a20dc49fd8200581c64cc50e1ad676b598aaaa552dd187a4a79a6567027236a19c19222d41a739935f38200581c67a93df9836b6a8b4a4424c4fa22f81310ae8393c14190c02e15a3841a0a3ba9aa8200581c695df5cfe3ea024049c4aef367ddd65bf89af62304a1327ab5fa38c71b00000002da66c6188200581c6a15a8cb2aba9468391e2f86cb425bff17fc37038c1b10f631dfff521b00000003f0a6bdb98200581c6a6f72f8b7bc856b7984da4b56aba5c03307c75b43d6e43854b458591b0000000795e031148200581c6aca00bf64ef28394337887845b98e58147a505f55a6cb4b3f99ddba1a06a518608200581c6b408dc175dd7193dfa9db64829da2b883841c625641e4da371235051b00000002b72163b08200581c6bedc05103b60a74da2ee66469af73e18eb8c0df2fdffe822150fb381b00000002ea02721d8200581c6c283f32874c3a72f50a1b7a1ea7e6c02ea32965ebeff0655f4067cc1b00000002ae8dc7df8200581c6ca5fab0747e89630b50b2da1fc95ec75490b78f124ec5d7256f2a381b00000003b57a8cfa8200581c6d3b82adb860d112d68829f71421732bf1edd403ea53b17f11a95c901a2916a8e38200581c6d9af9305f2537821371074abe1148d9ca45c04bdc85cf3428669e6d1a0b6a72e38200581c6e15b11603a7ecaadf948180cbe502ab91a6510436ab1f8735b86a621a947df89a8200581c6e209cdb8234d2c212bd037574f32006864f0362c008c05f032f87301b000000112391e6338200581c6ec83fee45f558942b2222d0bc063e4425098bc5c0b478a2a2128f051b0000000283af35008200581c70951c5b5d889e59e8b9c38b0db440fed40630a3f00bb191873339291af74dd4588200581c7234a73a063838b34887f132c24afbda0f059e39831520bc8d4d5a8d1a656cfead8200581c724ee90c9d307189ea2f15315672df7bf04520af50c96f975d0a1c4f1b00000002ad60651b8200581c72affb2b16c3e5b64a6abb5701c4ae4b85b8188b41fa6c1647b77cec1b0000003397f719508200581c73e586947a19208727f200a6dc74c9d1004ad8a338e8b1bb9831c27a1b000000013c02b9d98200581c748ec9a7bcecd63fff45a381030fe276352683389e96a11ae04b500c1b00000002063c52ef8200581c763bd12ec9b014338676bf3d9831e818d8f7858577a5e4c203b740bc1b000000010e8aa04b8200581c7851629ff085367e43c10a6c5165bf0b60e24c4a7081c2127f7bdab31a2797b0b58200581c791c0e0cc88c918c0c677d53303b3ed2ad7cbe8355800e66e33f43a31a00d3ac0f8200581c7d985e411578d9f394ce42a89a6fd9d4c91f08569c8d76281eb018a71b0000000159b603f68200581c7db29a8776226ea72595092da750a5a67182387a3110b2b01a20594d1b0000000777dc52a18200581c7e190374a6dc467568c274e3ad12d7e975440ebe6ddba66e8c27eb521b000000019c1fc2e38200581c7ebb7bee353d16731288bd7c85e507d80959fbafff403739d6663db11ab9d01a4a8200581c80850d1d8ed856124a78cadabeef2ad271101d1c8aa711f2922934ac1a0fb5af698200581c80d0755799e40c6179ecc774e0f5af2d2db6a031abbd94c53814655a1a1fc5e25a8200581c83e76c916dea1c2083371eb5c2aa92ed8a0836352054a009de5f012a1b00000004a59cb1a88200581c844ecacbc2e8d87cd270c038bbb15ee57d3ee7f7fc35a6a7140806081a4997dfe88200581c8565b09e73b318bb3f65babd29ed6377fcce728140a63d9f104f97d41a332928558200581c87690a80d7f59d6840fafa0689b80e6545eb1807af03baddc212c4071a0363506a8200581c87e30c728cc6b8d0d679eb4daf5ad181169238d48f3895037e10bb001ac396e2748200581c89c30d3582f2c49ceee849c5dd50c2542c87cdada58038d2041a94de1a5491d30e8200581c8b97f10f4388460cd798c25098978b320a9126e5aea114eaa019c7a71a00254fee8200581c8e449375adfa692b1616675ecf07128d6d2a5f55e68331e74c44eb901a9f6df2028200581c8fd507ce189686b9f911c82b39531b02549cb3a30a049dcb840e26741b0000000119b81b1f8200581c902b97f1e29034be53b0437a36d12dac16d351301a82f6ce543f03191a63bc63ac8200581c90b42898f9d24af72ca706b92f8962981c9029e19d76c0456994c3a91b00000001e8637b3c8200581c910bfbbb6d6bb8cffb92c2ef6f2811cb7a51d4e538670baba7a533aa1a373664c38200581c92e1c6528e39e4e706f4ddfd3ab46fb5e1ba86cca09e003c87fb79701aadbca2b48200581c9328af9a6f671ae8d16b485697e23b453252169e747684f00a0408711a4dd1c1528200581c938052389bf28701ed4d5dcbd75fca6ca8d3c82c2cdbd91baecc4d901b0000000167872f9f8200581c94690064ec0b4703ba56d0c7058c82b775b98e0a8abf55d816524acd1b00000007822f95f58200581c959eb0c50fb02199da22ee8ae6235f1e7634df1699d44749fa3675551af23fa7158200581c96f8728ecf0627a0cfbd17970950cba4055b2ca8068a82f0e264e7a61b0000000158f3af598200581c98aa41be6db0cb42219c53c133ee4152bd913ce67fcbcdaa4ee6f4ae1a20e086d78200581c9987bafb94c63269f6ad0e6e61dde7fe6c69965d1bf1e40624580f391b0000000c318607eb8200581c9a35241e655e6056dcec4e6821a7edb33e57c1e6dc82975664f1dd4c1b000000030c40c51c8200581c9b98b35297ad2fe34001364456634ea9da1bdc62bc9e9f0bb429443c1a074ec50e8200581c9ff135f2045ceddb5bd8e1a7aec05f6f7feccbef499cf7489f2ec3441af7a5766c8200581ca04ecc89fdc3d58c97da62edff57367e98d13e1562819e86965335821b000000060f5c5afe8200581ca15d19cecce1e73e09c534d971eb6d292f37ba70104f61ba2407935a1b00000004fee9bf3b8200581ca29839e1f99e66f815c839bab224577bd6914ea8263bcd0af493f1de1b000000013d7aa2bb8200581ca4ca6b70c0be443386916c9c3dd3f4031b73017f6fba647dfa604d261b000000351214b44e8200581ca4cb6c8e56ab4c71cd9a9e5b88d2705825e097fa58473585ef3eefd41b000000016fca3cfd8200581ca5da28796493691f0c45cd1d3e204d476b46b98a49db90da4e54cdd61b0000000ce2b8f5be8200581ca66d3c24bab6bb7d0f65209792682888f072b350ce3fb567f406eeec1a2a8bc9ae8200581ca85227c28dbbbaa49002f02b71caa6dec884a09b422b2ac832828a3b1b0000000b24540c6a8200581ca860252947c21b9d16ff6f63ebf9e8fc1fdee23e4dcec4ac5acaf5d31a011d839d8200581ca86abca03ba2b54b25f05ad0dfd02bdb194b4996607977fde211d2431b00000003267ac2f48200581ca88f757e9e177c39cfddcf015aa75d4fbb002e4daebb77e4ac1c099e1a284ac7718200581ca8c60b21a26fb53932ab7436590c08ddcd679d56d4f3c2f590d012fc1a3a2cdfd68200581caaec63d7c8a6b9a9f478e4bce149734818df63715a527ea37d102a1c1abec3a6188200581cab0c4b917f0b8c922c36621fa193dcce7895a8974c451e03181113511a021b24158200581cac482f1aca71a41bf8a4f36cd8b2e4e926a2d7a3ed7704b40fedc2f01b000000349efe15ef8200581cac6e6ced26048ea810d84b33cce61e817f2e7fc9a183c7ba2d45a60b1af7a360978200581cac8434c64cfd18e6d806ba3c0f55120927277cc5e36f53054f3d9a3a1a0417a3ac8200581cac899acee744ec0d790ac650a78c12bf7f0d8c224c7e41716863d6561b000000041fcc4fd28200581cac92d85538432b20d18d1196398cae90943a437fa22da1d9eb318b671aed14fc978200581caddeda62a36f8724910416536bef64a5dc2b59235cd3d04377d560311a26b3da4c8200581cae4adfa24df9da6e6d0cd04154abd222904cc8915595c3c0157659d81b0000000162bedf518200581cb0c81e93af4adf9af67e4df06bef2097ee137a9491d48021c68099fb1a2e8850928200581cb1156d7c21310ee196431264860cc8f22036ed182870ccff4881df1d1b000000049b069ca28200581cb12ac10f7a4aa34ebaa3dd2229afbc610b42732dec8a4605c05071aa1b0000000a29eda2e78200581cb26650089d0bc0405e3b595af3cb7853c43ab713eb98c059c0f1044c1b0000000200dfb5288200581cb2ddc0f557f738ba1d55ab955f4fdc10e7a1d82fe6daa2c5f84a94551b00000014e05315ae8200581cb3823a62b98e60221ec3039570f78c235d4a0a84800de56ecaecfdb71a6c1e695f8200581cb4b52a7c6fd4ea957a30dce0294626a4fcc1473b07b03b088f9619e61ad7dbaed28200581cb6fcb972aabe45a4d6e202c49924713d120d237d8d1a18bd60e052901a966256aa8200581cb7b964c39e00e8789df147d0e792066bc64c77a5e509e82fa784801c1b00000001aa16e6048200581cba83704986bff4e9a997b84fb5b5bea94c1bda4450188b14dc2a98321a1b61a8f48200581cbaf524f9631f2baa1b6171be65f6d65360d554fcfe4584c486f3bca51a8c04c7208200581cbce4f7002c53c01c6962006f3e9b2fbc3ab0d70342b8a21dba5f68d31b00000002b8027e178200581cbebb1b0bd5dbdef3bf165424a930bd274a2c91e7c4457d3a4d88f4251a00ff7e068200581cbf7515f690ad911cb1cf7654aafca00de7e31e35ed64180e0eb4c8c81b00000002c700fd838200581cc150420070296f74a77983162f3e36c8067b94cf390bbb086759b8a81b0000001cd0c6d3678200581cc41e4f71342c58b9b3b4c42c916ccbdf3b2a342d44c7f2c67c91a3d71b0000000548fb83b98200581cc4aef88847774c66d96850df7d63df294eed34fd7a550b409d9976731b00000015454900708200581cc62d6b0badd636522d651c78e074f5dac8c6ce597f869dd78e674eb91b000000014f1931d48200581cc631165c7c41675a1cf3feed316829147ffaf05f0cd42cf60b2024a71a43301abd8200581cc71bbf37b84106fe6208f3a54d1eead0063753353fde5a731e0995291abfac947f8200581cc7cdfe51df919c1a3919779383b9f1231c7857e1c325da3b1869643e1a6189158c8200581ccca83cb796d6ebc3ead2039824058963d5a5100d626c174245fe96ba1a0fd8e7928200581cce66658c36cbd0daf19982cf5a125b262da3bf2b12ce053cce5e28471a52f5a7108200581ccf040215e6df1129342b99039740e984806addfb8b954980e3d5ba891b0000004797b0c2fe8200581cd30ba2531eff5bd174675cb1d9b3c4f2b3d66d947f0897007b41465b1b00000001f3cddf7b8200581cd55c9a81d5be5317ceedffd0328848bd72d638e84ac8631589396eb61a2c74c86c8200581cd8cdd04f5f91bb57e6cee3ea3a487b928a8f1003170fdd91421c879e1a043ce66e8200581cd9edc067383928ede002f78cdf700e8f5ad0e1682066440066622f8b1a28bfe7e58200581cdb63546e634bec5828998b4a3dbb0930f664c4d1bcce586b9aa0b3371a3466e2e38200581cdd0f9a12cbbf2d93556e17d46d9fec4192b7048863d33c4c067323601b00000003b421f77a8200581cddae2af68ff6af9646283e32eb8dc815bba7d13216a745748d497a8a1a03425d048200581ce24bf093e19c1273405a7b86bc0f8d091fbf4298951892135642c8491b0000000139a619be8200581ce33f562ae3371c65d68bc02c1a9e0413ac6ec3fb2f65064d6cb42e1c1b0000000296df697b8200581ce47de36472a866f88b5c9e115041848a5efa6ce05a72be9a9b28a4411b00000001b4a6afcc8200581ce84cdca7675085f4fcb32d906b3ad6d383752caefd8fb059ad5329fa1b0000000651eea0038200581ce8ce8deab0fcac7f0b9ee858ba54d4fe2fc813fc834040c7aa733a681a04c1274f8200581ce9eb11829f1adbe9db107ae0466e57885e17d69ba59f06bf85996f631b00000003414f12f18200581cea025ee9adf4163f941ce9e5f20e79a113c00de8390c9e5f354f362d1ae9c00ec08200581cea9d83db8104bb696c471821e20bee6aaff18e813b4be446822d837d1a8ca7ec968200581ceafcae5bd7b5bfa44825dc372c709a99716d2c02cf29e7291ce4e7b31aa2f8dead8200581cebfcaec39f6feeb63f9b389abac90df1eacbee40504438a375d76d731a83d4a3b58200581ced1dbf3241ad9aac4ecd7239f00eaa6d9b00da4cbecf547f2b0c9ab51a076e59408200581ced92944b42a0cdc4be0fceee007e0be2f06d23cf75606afbe545ec951a544bf8eb8200581ceda87d0938cab2e4567a47411b95a27eb00aeb8218fb17bf6e5365a71b000000025731aca48200581cede458847c712968c6ea5218c265875b71397beed3f1ee31f54039c11b000000019b84f5d78200581cede783e3b86e790429cdb2c5daa5d1e30714f1d6b53e414f475b8d4f1abf2d496a8200581ceed2fea7bf1bdec24b06951c23e8fed7493262e223c0488efa360ac81b0000002c78502a118200581cf0b5051dccdfdb93921b2baff02e169010ef467e93cc69e1350e5dae1b00000002ad3d340a8200581cf0da723ef196b602ce56070f42281258ef11c38e203935359e3eaa2c1b00000002ce54e3ba8200581cf23582610378a83a3944b7e2023932e0d445a223461249fe3de319a11b00000001867cb8078200581cf3cc6cf632613d62331d546afff0257b15783d39b81b47bd7a9a6a961b00000001f6e8605d8200581cf6b12722038ed4ce1375c8e6ec6de7daeec1b9c27811493c75ec45651a0e375d988200581cf6bd5b9191660c0ca7cada2589aa5e84f48751339c99652fac4132851a590f39f88200581cf75a6f9c8d3c0163a506923234e66eb7404503394c0824ebdcc815d51a368702c78200581cf89dcde6687826def543b89cbae0925da0bc3ca9ecbb43a06c4a644b1b000000021c3d10c58200581cf8c118b504cd570a91429d997da109962d2b4cd9671bf83e44aaaec61b00000002804038da8200581cfd2ffd2c7692c6c733b5f30ada30c9d57f11f792b5a875163efaf60b1b0000000aaf6b2e028200581cfecffc4b11438299d925367829fd9c1745e36d98dd93d3bb0c20b1261b00000002b13f54d98200581cffa66135c2895c2b8ed91cd956883a3ca9a355709ff60938140499ab1a362abb358200581cffddc35c5c2085e54f527c610d8408cbbc7f26182b0aa8efc39f1c731b0000000121784c27ff82008200581c62b3fade57272bfba2b7abd7877dabcf79b332a4b6fe373084ccd53e82008200581c1d1692b16cd0fbe005e45abb77383d02bee4ed86ef1eb2886659498d82008200581c388ede6f04652931f309e1901e7cc8959e2809390ef4fb3f4f7a7dea82008200581caddeda62a36f8724910416536bef64a5dc2b59235cd3d04377d5603182008200581c4ed709c5054447bf326aebb31ba47f4ddb82acf81f00172c37116cfc82008200581c2e54bf801f57a14174679d6772011bbff179188bc9b68e9e073a547282008200581cac482f1aca71a41bf8a4f36cd8b2e4e926a2d7a3ed7704b40fedc2f082008200581c9a35241e655e6056dcec4e6821a7edb33e57c1e6dc82975664f1dd4c82008200581c938052389bf28701ed4d5dcbd75fca6ca8d3c82c2cdbd91baecc4d9082008200581ca4cb6c8e56ab4c71cd9a9e5b88d2705825e097fa58473585ef3eefd482008200581c2fe954dd15471788fc832ac41415a185e21d0cfc6f5d94ffc9756e6d82008200581c45796e1560faa06b777598d53faf54cd8fb77169cbd48eb5731bb92082008200581c4decd172f4102fffda2562fad82ce6926d386977d8e6d5c94907801b82008200581cf23582610378a83a3944b7e2023932e0d445a223461249fe3de319a182008200581c06cac706110395e66ac8913077e093539bbdcab1313433bf503b60c682008200581c695df5cfe3ea024049c4aef367ddd65bf89af62304a1327ab5fa38c782008200581ca66d3c24bab6bb7d0f65209792682888f072b350ce3fb567f406eeec82008200581cea9d83db8104bb696c471821e20bee6aaff18e813b4be446822d837d82008200581c6aca00bf64ef28394337887845b98e58147a505f55a6cb4b3f99ddba82008200581cb2ddc0f557f738ba1d55ab955f4fdc10e7a1d82fe6daa2c5f84a945582008200581c724ee90c9d307189ea2f15315672df7bf04520af50c96f975d0a1c4f82008200581ca5da28796493691f0c45cd1d3e204d476b46b98a49db90da4e54cdd682008200581c90b42898f9d24af72ca706b92f8962981c9029e19d76c0456994c3a982008200581c391825387fdff28dc24a4f2a7748d590ffbc4b4d8a8a6f0f836baf5882008200581c94690064ec0b4703ba56d0c7058c82b775b98e0a8abf55d816524acd82008200581cb4b52a7c6fd4ea957a30dce0294626a4fcc1473b07b03b088f9619e682008200581cd55c9a81d5be5317ceedffd0328848bd72d638e84ac8631589396eb682008200581ceafcae5bd7b5bfa44825dc372c709a99716d2c02cf29e7291ce4e7b382008200581c5b728b0bcf80bf766688ea376c07f09d3dc9a265553f22b25b75422882008200581cfd2ffd2c7692c6c733b5f30ada30c9d57f11f792b5a875163efaf60b82008200581c7851629ff085367e43c10a6c5165bf0b60e24c4a7081c2127f7bdab382008200581c7d985e411578d9f394ce42a89a6fd9d4c91f08569c8d76281eb018a782008200581ca860252947c21b9d16ff6f63ebf9e8fc1fdee23e4dcec4ac5acaf5d382008200581cc71bbf37b84106fe6208f3a54d1eead0063753353fde5a731e09952982008200581c3581b417908dea107b5abf821dcce6dd19f732af44ed6b6bb2d7496582008200581c2c459301ae68445b96991ca0eaef73d7869991f1c283af360e22088582008200581cbf7515f690ad911cb1cf7654aafca00de7e31e35ed64180e0eb4c8c882008200581ca4ca6b70c0be443386916c9c3dd3f4031b73017f6fba647dfa604d2682008200581c5d5a181ff3fc68cf7361cba7b7382efff80470497d6e50eca7778b9482008200581cac8434c64cfd18e6d806ba3c0f55120927277cc5e36f53054f3d9a3a82008200581ceda87d0938cab2e4567a47411b95a27eb00aeb8218fb17bf6e5365a782008200581cf89dcde6687826def543b89cbae0925da0bc3ca9ecbb43a06c4a644b82008200581cb26650089d0bc0405e3b595af3cb7853c43ab713eb98c059c0f1044c82008200581c80850d1d8ed856124a78cadabeef2ad271101d1c8aa711f2922934ac82008200581c67a93df9836b6a8b4a4424c4fa22f81310ae8393c14190c02e15a38482008200581cb3823a62b98e60221ec3039570f78c235d4a0a84800de56ecaecfdb782008200581c63181e6e8e5ee95f1c671cf38baee666721816efcb190c288e8fff9d82008200581ca15d19cecce1e73e09c534d971eb6d292f37ba70104f61ba2407935a82008200581ca8c60b21a26fb53932ab7436590c08ddcd679d56d4f3c2f590d012fc82008200581cede783e3b86e790429cdb2c5daa5d1e30714f1d6b53e414f475b8d4f82008200581c2be784dfd7253aebb3e8750ec0d9ef33054269dbacf8364d854f41ef82008200581cae4adfa24df9da6e6d0cd04154abd222904cc8915595c3c0157659d882008200581c6e209cdb8234d2c212bd037574f32006864f0362c008c05f032f873082008200581ce84cdca7675085f4fcb32d906b3ad6d383752caefd8fb059ad5329fa82008200581c39ff4977c8c3d37dba57d2044867650266f6e4c75c6a1665ac3765c682008200581cdd0f9a12cbbf2d93556e17d46d9fec4192b7048863d33c4c0673236082008200581cf3cc6cf632613d62331d546afff0257b15783d39b81b47bd7a9a6a9682008200581c19a6a21c3411bb8065859b2c91f74331f8d5a036b3b79184b6689ce482008200581cf8c118b504cd570a91429d997da109962d2b4cd9671bf83e44aaaec682008200581ce47de36472a866f88b5c9e115041848a5efa6ce05a72be9a9b28a44182008200581cf0da723ef196b602ce56070f42281258ef11c38e203935359e3eaa2c82008200581c89c30d3582f2c49ceee849c5dd50c2542c87cdada58038d2041a94de82008200581c058b6e56f91b7c34e367fa2fd65a64b7a679bd7bfa62e36e210d80d882008200581cce66658c36cbd0daf19982cf5a125b262da3bf2b12ce053cce5e284782008200581c22abaff6207509c0bccdba86dceaeb3ef450452a98831d913c7cb53b82008200581ce9eb11829f1adbe9db107ae0466e57885e17d69ba59f06bf85996f6382008200581c22a7719fe0a30b4977467de59a04d7b5e9d56caff57e665db0ac9f8182008200581c27978020d4d87c686e3a37fd4cba304eb5d6af4ed9e6ab4457d5afcd82008200581c902b97f1e29034be53b0437a36d12dac16d351301a82f6ce543f031982008200581c8fd507ce189686b9f911c82b39531b02549cb3a30a049dcb840e267482008200581cf6b12722038ed4ce1375c8e6ec6de7daeec1b9c27811493c75ec456582008200581ca88f757e9e177c39cfddcf015aa75d4fbb002e4daebb77e4ac1c099e82008200581c6a15a8cb2aba9468391e2f86cb425bff17fc37038c1b10f631dfff5282008200581ccca83cb796d6ebc3ead2039824058963d5a5100d626c174245fe96ba82008200581cb1156d7c21310ee196431264860cc8f22036ed182870ccff4881df1d82008200581c04b4af2f4536466c9d0010ea94c7319db6eff7a97e7f26cdb1e773d482008200581c4f8dda033362e9c8840206563925b9896263b103d3e45e9a0c28d45582008200581c6ec83fee45f558942b2222d0bc063e4425098bc5c0b478a2a2128f0582008200581c0bf0271768bb0cb210d400f5352a578121aba403c5ec00405a29488082008200581c33e85b999cdd061ec49ea20228112dd92a108e21e067ec48a167b82f82008200581cba83704986bff4e9a997b84fb5b5bea94c1bda4450188b14dc2a983282008200581c87690a80d7f59d6840fafa0689b80e6545eb1807af03baddc212c40782008200581cffa66135c2895c2b8ed91cd956883a3ca9a355709ff60938140499ab82008200581c7e190374a6dc467568c274e3ad12d7e975440ebe6ddba66e8c27eb5282008200581cb6fcb972aabe45a4d6e202c49924713d120d237d8d1a18bd60e0529082008200581c31b1f312087655265e40461acd2f4dda62cd69c0613570b2f91efc3282008200581c6a6f72f8b7bc856b7984da4b56aba5c03307c75b43d6e43854b4585982008200581cc41e4f71342c58b9b3b4c42c916ccbdf3b2a342d44c7f2c67c91a3d782008200581c233f410282657b90069dccdf0c03a78de31074869756f20bc417aebb82008200581ccf040215e6df1129342b99039740e984806addfb8b954980e3d5ba8982008200581c446e15940325aae70c01aa0c96d079bd68a0b2b1a6324879491210c982008200581cfecffc4b11438299d925367829fd9c1745e36d98dd93d3bb0c20b12682008200581cac6e6ced26048ea810d84b33cce61e817f2e7fc9a183c7ba2d45a60b82008200581cc7cdfe51df919c1a3919779383b9f1231c7857e1c325da3b1869643e82008200581cdb63546e634bec5828998b4a3dbb0930f664c4d1bcce586b9aa0b33782008200581c6b408dc175dd7193dfa9db64829da2b883841c625641e4da3712350582008200581cebfcaec39f6feeb63f9b389abac90df1eacbee40504438a375d76d7382008200581c6d3b82adb860d112d68829f71421732bf1edd403ea53b17f11a95c9082008200581c0cf972e71a248dc9a59bad9b2244bbc82f89060352e28b22ac8d465582008200581c090a63ca4e67569d49e7e9dc499ebeb847d8e60fb37d1963581368c682008200581cc150420070296f74a77983162f3e36c8067b94cf390bbb086759b8a882008200581caaec63d7c8a6b9a9f478e4bce149734818df63715a527ea37d102a1c82008200581c06cf8d573a5d9ff295eafcc48938212c9296871d13ff905db1a604f182008200581c4597ec4cc1c4d012dc8d9a8c4320773d88b739449467b4f445d34bf482008200581cc631165c7c41675a1cf3feed316829147ffaf05f0cd42cf60b2024a782008200581cab0c4b917f0b8c922c36621fa193dcce7895a8974c451e031811135182008200581c4d33eda5f702de4deed05ed99ee0a8bfc456742a4c8bfa4020f3454882008200581ceed2fea7bf1bdec24b06951c23e8fed7493262e223c0488efa360ac882008200581c438ea0cc2a75da6a3d60708a5dd879a7b1d63891a81dc42bdd2908db82008200581c24c7ee461f15e9cb6df13427b4685c01b5c5b5ef2954dc13e14aa74f82008200581c36eafaae91e48b329f24eb2ca7bb71c8ca5ea41c9582e8500d4b63e582008200581cb12ac10f7a4aa34ebaa3dd2229afbc610b42732dec8a4605c05071aa82008200581c9987bafb94c63269f6ad0e6e61dde7fe6c69965d1bf1e40624580f3982008200581c4d1307081d1362fb9e3debd029adac66d3ec947579905d266cccc56982008200581c33682102a3202c26cebf11d6e360b96291c2dead24560914bc15e1e782008200581c24074f532b2ae266b0ab5331b0d6a8383e31fa15145691b6d9f5799882008200581ced1dbf3241ad9aac4ecd7239f00eaa6d9b00da4cbecf547f2b0c9ab582008200581c910bfbbb6d6bb8cffb92c2ef6f2811cb7a51d4e538670baba7a533aa82008200581c641ca9753c1d2ce01b5d1bd8d17196969558b9ceeb186f0f729ddd4482008200581c791c0e0cc88c918c0c677d53303b3ed2ad7cbe8355800e66e33f43a382008200581c3f986f0b9fdc99a418c9a0f972f9c6b9454c5754a81bdccec40dc09d82008200581c3c417ea0dc93cb8b1f8c92b2e473434bb20c18979e800f4ec531eebb82008200581ca86abca03ba2b54b25f05ad0dfd02bdb194b4996607977fde211d24382008200581cac92d85538432b20d18d1196398cae90943a437fa22da1d9eb318b6782008200581c8565b09e73b318bb3f65babd29ed6377fcce728140a63d9f104f97d482008200581c6bedc05103b60a74da2ee66469af73e18eb8c0df2fdffe822150fb3882008200581cd8cdd04f5f91bb57e6cee3ea3a487b928a8f1003170fdd91421c879e82008200581c83e76c916dea1c2083371eb5c2aa92ed8a0836352054a009de5f012a82008200581c1dfc6bf6eddfa4120690126942972974ed3a0a931c1dac6fd051cf8e82008200581c80d0755799e40c6179ecc774e0f5af2d2db6a031abbd94c53814655a82008200581c50756852527d09065c47572e9b29fb5a63bc3f393b6326cb19e34c2182008200581cf75a6f9c8d3c0163a506923234e66eb7404503394c0824ebdcc815d582008200581cbebb1b0bd5dbdef3bf165424a930bd274a2c91e7c4457d3a4d88f42582008200581c73e586947a19208727f200a6dc74c9d1004ad8a338e8b1bb9831c27a82008200581c13ee747308779b78902d5f7b6e7608b9523885d9b7c500db0459b73e82008200581c6d9af9305f2537821371074abe1148d9ca45c04bdc85cf3428669e6d82008200581c64cc50e1ad676b598aaaa552dd187a4a79a6567027236a19c19222d482008200581cd9edc067383928ede002f78cdf700e8f5ad0e1682066440066622f8b82008200581ced92944b42a0cdc4be0fceee007e0be2f06d23cf75606afbe545ec9582008200581cede458847c712968c6ea5218c265875b71397beed3f1ee31f54039c182008200581c35ef3f2e454f77519961026ecf25d29616db1b600c6d123d0e3a0b5882008200581cbce4f7002c53c01c6962006f3e9b2fbc3ab0d70342b8a21dba5f68d382008200581c21227adb44903d2de668696200ec709ee4f92d0009130123ef19c3c382008200581c98aa41be6db0cb42219c53c133ee4152bd913ce67fcbcdaa4ee6f4ae82008200581cea025ee9adf4163f941ce9e5f20e79a113c00de8390c9e5f354f362d82008200581c959eb0c50fb02199da22ee8ae6235f1e7634df1699d44749fa36755582008200581cb0c81e93af4adf9af67e4df06bef2097ee137a9491d48021c68099fb82008200581c5d3a2e1a6aee1687bde20b2b374ee427a8159b6b7cd1ff481037ea3e82008200581c6e15b11603a7ecaadf948180cbe502ab91a6510436ab1f8735b86a6282008200581cddae2af68ff6af9646283e32eb8dc815bba7d13216a745748d497a8a82008200581ce8ce8deab0fcac7f0b9ee858ba54d4fe2fc813fc834040c7aa733a6882008200581cc4aef88847774c66d96850df7d63df294eed34fd7a550b409d99767382008200581c748ec9a7bcecd63fff45a381030fe276352683389e96a11ae04b500c82008200581c7db29a8776226ea72595092da750a5a67182387a3110b2b01a20594d82008200581c1f69478b9a753e969d5085e40439993fc4358e94db8cc174ddddfa9882008200581c25606e7ec45d235b74c5a0dcf2866929f955f1803a4d4da6b538e03982008200581c72affb2b16c3e5b64a6abb5701c4ae4b85b8188b41fa6c1647b77cec82008200581c9b98b35297ad2fe34001364456634ea9da1bdc62bc9e9f0bb429443c82008200581c4e83fbacea4d13d1c985dbea19286c849a5ea04cb68a611c5ca3b37582008200581c87e30c728cc6b8d0d679eb4daf5ad181169238d48f3895037e10bb0082008200581c1010834c8e3f8d7b6eb04a9181f0ff40e780429dd513662bf32c9c8082008200581c844ecacbc2e8d87cd270c038bbb15ee57d3ee7f7fc35a6a71408060882008200581ce24bf093e19c1273405a7b86bc0f8d091fbf4298951892135642c84982008200581c8b97f10f4388460cd798c25098978b320a9126e5aea114eaa019c7a782008200581cf6bd5b9191660c0ca7cada2589aa5e84f48751339c99652fac41328582008200581c19ebdabf80584f3df8640922ab156ead84eafd23b89dde7e19d6a7fa82008200581c9ff135f2045ceddb5bd8e1a7aec05f6f7feccbef499cf7489f2ec34482008200581c37557722eb102c5737b85f2332d5cd6baea6257ede6f0945389c03b182008200581cd30ba2531eff5bd174675cb1d9b3c4f2b3d66d947f0897007b41465b82008200581c2545642a9c1bfedf95bf30438849080aabc4219a227f209a84b72ee382008200581cbaf524f9631f2baa1b6171be65f6d65360d554fcfe4584c486f3bca582008200581c017a39dc9198c69f55e61e9edfcd4506d38c41703d7454cf4cb6a88282008200581c6c283f32874c3a72f50a1b7a1ea7e6c02ea32965ebeff0655f4067cc82008200581cc62d6b0badd636522d651c78e074f5dac8c6ce597f869dd78e674eb982008200581c763bd12ec9b014338676bf3d9831e818d8f7858577a5e4c203b740bc82008200581c00de27c8609df0a7f7ae090224ace1d68cc5d338a335f1c6bb583d7282008200581c92e1c6528e39e4e706f4ddfd3ab46fb5e1ba86cca09e003c87fb797082008200581cac899acee744ec0d790ac650a78c12bf7f0d8c224c7e41716863d65682008200581ce33f562ae3371c65d68bc02c1a9e0413ac6ec3fb2f65064d6cb42e1c82008200581c5fba4a7603fbb18fb1d58c0c00dc01eb4b919f2ce7d1afc92a19e39982008200581c17fac4618ea56ec6e12e9f6f818e8af5d98ea18c1681ba78aac44e1e82008200581c0477455a3a4a0b87c1be90c77c6b376841f8f4c52e42b47691baac2082008200581c7ebb7bee353d16731288bd7c85e507d80959fbafff403739d6663db182008200581c13ce7ba99b254954ddc361445ff6bb3673320bd3d5da3615d9277b1982008200581c9328af9a6f671ae8d16b485697e23b453252169e747684f00a04087182008200581c6ca5fab0747e89630b50b2da1fc95ec75490b78f124ec5d7256f2a3882008200581cf0b5051dccdfdb93921b2baff02e169010ef467e93cc69e1350e5dae82008200581ca04ecc89fdc3d58c97da62edff57367e98d13e1562819e869653358282008200581cffddc35c5c2085e54f527c610d8408cbbc7f26182b0aa8efc39f1c7382008200581c41e22a8c23ff59767ab1c6f4285350fb3c8b7bb3e5308d8cd981381782008200581c62566a741704214440ef14350ec5ab1919f0e2f6dce0636f3d616c1282008200581c1d6a5923d8c491a898cb9de9ec1f84d6d3b4332c9610e4924b4cf5b782008200581cb7b964c39e00e8789df147d0e792066bc64c77a5e509e82fa784801c82008200581c499fb62d49ef0c0183e176166b543feaceb7af7f9074a17e078f710682008200581c8e449375adfa692b1616675ecf07128d6d2a5f55e68331e74c44eb9082008200581ca85227c28dbbbaa49002f02b71caa6dec884a09b422b2ac832828a3b82008200581ca29839e1f99e66f815c839bab224577bd6914ea8263bcd0af493f1de82008200581c70951c5b5d889e59e8b9c38b0db440fed40630a3f00bb1918733392982008200581c7234a73a063838b34887f132c24afbda0f059e39831520bc8d4d5a8d82008200581c96f8728ecf0627a0cfbd17970950cba4055b2ca8068a82f0e264e7a6ffa1008882582061261a95b7613ee6bf2067dad77b70349729b0c50d57bc1cf30de0db4a1e73a8584004f7d6e3c5af19cf76ec46b6a876f688c5ea83f8bfd6cac4b26fb5944622584a1cd8a986bfef66a25dac652ec4d6004b2f250dce9d60e3a4efdb2e8520d2a4038258209180d818e69cd997e34663c418a648c076f2e19cd4194e486e159d8580bc6cda5840c19ed423f94f6100099305f47a44ab97d5ace1a1c654227f018229b71e923d6318a142bc9a68c89fd5206293bb612b1416ec48cc8535d1e967a9374ba44a990a82582089c29f8c4af27b7accbe589747820134ebbaa1caf3ce949270a3d0c7dcfd541b58401e84dc558773939db3abd63e9047e9c66c337eacbd3f341e6995f9bf2370ffa36dc445774b95e815adba96f4135aeefbbdffae1df29ef1fcab782b6cb6105105825820f14f712dc600d793052d4842d50cefa4e65884ea6cf83707079eb8ce302efc85584097027d20a4ea31963a7c04fd1bed3b6ec760cdacba3f6cd3710e3a8db6d3623bf541ed5596fbf7cb76c55335d28c7cfa165c0bf11e15f2fee6419c201b1494018258208b53207629f9a30e4b2015044f337c01735abe67243c19470c9dae8c7b732798584069770d513b8cd91cf751dd21a817481b9a25e052df1f1efc47e7c245c031f8977ffb371b64b9001d200140cd556950cff923086eed32ee4d41ad0c1acb987b0f8258205fddeedade2714d6db2f9e1104743d2d8d818ecddc306e176108db14caadd4415840bdf86ad2f72f238e58b1815594ca65c1ceac36373ef79fedfd971be0dc5f23233fff7b209b693ac09c3295f398667197695a1efc8fb31ea58acd1bf983794703825820cbc6b506e94fbefe442eecee376f3b3ebaf89415ef5cd2efb666e06ddae483935840740e0dad0843c68a286c267fcd08e7e9395f23f14b99f5adbd777c1fd646e7d11d0956210951ad6c9b18734751759dde18cf908d6ef1c4a463f761d0389c370f825820e8c03a03c0b2ddbea4195caf39f41e669f7d251ecf221fbb2f275c0a5d7e05d158407bbb8ffec33f55bd4065e33042a1a50f65b519c39d7f7d5b7878e9f72665f6870a98cbc85293d59d0616c02d1b6b1e7deefa3fc167de28e06047240755c49401f6"
                |> Transaction.deserialize
                |> Expect.equal
                    (Just
                        { body = txBody35d2728e
                        , witnessSet = txWitnessSet35d2728e
                        , isValid = True
                        , auxiliaryData = Nothing
                        }
                    )


txBody35d2728e : TransactionBody
txBody35d2728e =
    { newBody
        | inputs = [ { outputIndex = 0, transactionId = Bytes.fromHexUnchecked "2dcf5b56aa5d63bd346b1196b8dadc3bb32cafa3a7e080b04339d105e4637d17" } ]
        , outputs =
            [ { address = Address.Shelley { networkId = Mainnet, paymentCredential = Address.VKeyHash (Bytes.fromHexUnchecked "d80fe69ded1ff90f41e526d0332a2ff98ba8a0d85ceb8941b5178420"), stakeCredential = Nothing }
              , amount = Value.onlyLovelace (N.fromSafeInt 64608934235)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            ]
        , fee = N.fromSafeInt 900000
        , ttl = Just (N.fromSafeInt 10000000)
        , certificates =
            [ Transaction.MoveInstantaneousRewardsCert
                { source = Transaction.Reserves
                , target =
                    Transaction.StakeCredentials
                        [ ( Address.VKeyHash (Bytes.fromHexUnchecked "00de27c8609df0a7f7ae090224ace1d68cc5d338a335f1c6bb583d72"), bigNat [ 46223825, 32 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "017a39dc9198c69f55e61e9edfcd4506d38c41703d7454cf4cb6a882"), bigNat [ 13749931, 6 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "0477455a3a4a0b87c1be90c77c6b376841f8f4c52e42b47691baac20"), bigNat [ 53725743, 54 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "04b4af2f4536466c9d0010ea94c7319db6eff7a97e7f26cdb1e773d4"), bigNat [ 20229586, 86 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "058b6e56f91b7c34e367fa2fd65a64b7a679bd7bfa62e36e210d80d8"), bigNat [ 9696296, 160 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "06cac706110395e66ac8913077e093539bbdcab1313433bf503b60c6"), bigNat [ 58237716, 146 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "06cf8d573a5d9ff295eafcc48938212c9296871d13ff905db1a604f1"), bigNat [ 35452591, 17 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "090a63ca4e67569d49e7e9dc499ebeb847d8e60fb37d1963581368c6"), bigNat [ 25875210, 130 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "0bf0271768bb0cb210d400f5352a578121aba403c5ec00405a294880"), bigNat [ 31217804, 205 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "0cf972e71a248dc9a59bad9b2244bbc82f89060352e28b22ac8d4655"), bigNat [ 31323404, 2827 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "1010834c8e3f8d7b6eb04a9181f0ff40e780429dd513662bf32c9c80"), bigNat [ 19934888, 4708 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "13ce7ba99b254954ddc361445ff6bb3673320bd3d5da3615d9277b19"), bigNat [ 11623858, 197 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "13ee747308779b78902d5f7b6e7608b9523885d9b7c500db0459b73e"), bigNat [ 59150929, 351 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "17fac4618ea56ec6e12e9f6f818e8af5d98ea18c1681ba78aac44e1e"), bigNat [ 46733695, 261 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "19a6a21c3411bb8065859b2c91f74331f8d5a036b3b79184b6689ce4"), bigNat [ 9566255, 7104 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "19ebdabf80584f3df8640922ab156ead84eafd23b89dde7e19d6a7fa"), bigNat [ 43581813, 86 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "1d1692b16cd0fbe005e45abb77383d02bee4ed86ef1eb2886659498d"), bigNat [ 359603, 168 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "1d6a5923d8c491a898cb9de9ec1f84d6d3b4332c9610e4924b4cf5b7"), bigNat [ 46524391 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "1dfc6bf6eddfa4120690126942972974ed3a0a931c1dac6fd051cf8e"), bigNat [ 36347363, 105 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "1f69478b9a753e969d5085e40439993fc4358e94db8cc174ddddfa98"), bigNat [ 61440710, 27 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "21227adb44903d2de668696200ec709ee4f92d0009130123ef19c3c3"), bigNat [ 18738534, 131 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "22a7719fe0a30b4977467de59a04d7b5e9d56caff57e665db0ac9f81"), bigNat [ 18709182, 87 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "22abaff6207509c0bccdba86dceaeb3ef450452a98831d913c7cb53b"), bigNat [ 6166617, 404 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "233f410282657b90069dccdf0c03a78de31074869756f20bc417aebb"), bigNat [ 1769558, 37 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "24074f532b2ae266b0ab5331b0d6a8383e31fa15145691b6d9f57998"), bigNat [ 4147153, 32 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "24c7ee461f15e9cb6df13427b4685c01b5c5b5ef2954dc13e14aa74f"), bigNat [ 3797056, 53 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "2545642a9c1bfedf95bf30438849080aabc4219a227f209a84b72ee3"), bigNat [ 31251980, 57 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "25606e7ec45d235b74c5a0dcf2866929f955f1803a4d4da6b538e039"), bigNat [ 18892733, 244 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "27978020d4d87c686e3a37fd4cba304eb5d6af4ed9e6ab4457d5afcd"), bigNat [ 47997500, 44 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "2be784dfd7253aebb3e8750ec0d9ef33054269dbacf8364d854f41ef"), bigNat [ 26240269, 11 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "2c459301ae68445b96991ca0eaef73d7869991f1c283af360e220885"), bigNat [ 15477496, 1 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "2e54bf801f57a14174679d6772011bbff179188bc9b68e9e073a5472"), bigNat [ 17083327, 1684 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "2fe954dd15471788fc832ac41415a185e21d0cfc6f5d94ffc9756e6d"), bigNat [ 36901307, 29 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "31b1f312087655265e40461acd2f4dda62cd69c0613570b2f91efc32"), bigNat [ 51157766, 105 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "33682102a3202c26cebf11d6e360b96291c2dead24560914bc15e1e7"), bigNat [ 12196537, 245 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "33e85b999cdd061ec49ea20228112dd92a108e21e067ec48a167b82f"), bigNat [ 41124065, 168 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "3581b417908dea107b5abf821dcce6dd19f732af44ed6b6bb2d74965"), bigNat [ 62601902, 9 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "35ef3f2e454f77519961026ecf25d29616db1b600c6d123d0e3a0b58"), bigNat [ 31311335, 86 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "36eafaae91e48b329f24eb2ca7bb71c8ca5ea41c9582e8500d4b63e5"), bigNat [ 51861956, 11 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "37557722eb102c5737b85f2332d5cd6baea6257ede6f0945389c03b1"), bigNat [ 60437571, 87 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "388ede6f04652931f309e1901e7cc8959e2809390ef4fb3f4f7a7dea"), bigNat [ 3187943, 200 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "391825387fdff28dc24a4f2a7748d590ffbc4b4d8a8a6f0f836baf58"), bigNat [ 62444056, 16 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "39ff4977c8c3d37dba57d2044867650266f6e4c75c6a1665ac3765c6"), bigNat [ 50113956, 2864 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "3c417ea0dc93cb8b1f8c92b2e473434bb20c18979e800f4ec531eebb"), bigNat [ 34115850, 7 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "3f986f0b9fdc99a418c9a0f972f9c6b9454c5754a81bdccec40dc09d"), bigNat [ 39609412, 224 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "41e22a8c23ff59767ab1c6f4285350fb3c8b7bb3e5308d8cd9813817"), bigNat [ 31674256, 154 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "438ea0cc2a75da6a3d60708a5dd879a7b1d63891a81dc42bdd2908db"), bigNat [ 57986537, 120 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "446e15940325aae70c01aa0c96d079bd68a0b2b1a6324879491210c9"), bigNat [ 34785686, 21 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "45796e1560faa06b777598d53faf54cd8fb77169cbd48eb5731bb920"), bigNat [ 17783112, 174 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "4597ec4cc1c4d012dc8d9a8c4320773d88b739449467b4f445d34bf4"), bigNat [ 12598984, 160 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "499fb62d49ef0c0183e176166b543feaceb7af7f9074a17e078f7106"), bigNat [ 36579780, 15 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "4d1307081d1362fb9e3debd029adac66d3ec947579905d266cccc569"), bigNat [ 59473501, 9 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "4d33eda5f702de4deed05ed99ee0a8bfc456742a4c8bfa4020f34548"), bigNat [ 2825179, 1 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "4decd172f4102fffda2562fad82ce6926d386977d8e6d5c94907801b"), bigNat [ 2665564, 86 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "4e83fbacea4d13d1c985dbea19286c849a5ea04cb68a611c5ca3b375"), bigNat [ 52522677, 17 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "4ed709c5054447bf326aebb31ba47f4ddb82acf81f00172c37116cfc"), bigNat [ 61779130, 12 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "4f8dda033362e9c8840206563925b9896263b103d3e45e9a0c28d455"), bigNat [ 1185002, 1 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "50756852527d09065c47572e9b29fb5a63bc3f393b6326cb19e34c21"), bigNat [ 53193454, 124 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "5b728b0bcf80bf766688ea376c07f09d3dc9a265553f22b25b754228"), bigNat [ 45072987, 619 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "5d3a2e1a6aee1687bde20b2b374ee427a8159b6b7cd1ff481037ea3e"), bigNat [ 11324400, 128 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "5d5a181ff3fc68cf7361cba7b7382efff80470497d6e50eca7778b94"), bigNat [ 10544067, 455 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "5fba4a7603fbb18fb1d58c0c00dc01eb4b919f2ce7d1afc92a19e399"), bigNat [ 18363179, 9 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "62566a741704214440ef14350ec5ab1919f0e2f6dce0636f3d616c12"), bigNat [ 46743987, 199 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "62b3fade57272bfba2b7abd7877dabcf79b332a4b6fe373084ccd53e"), bigNat [ 32007133, 6414 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "63181e6e8e5ee95f1c671cf38baee666721816efcb190c288e8fff9d"), bigNat [ 34987049, 295 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "641ca9753c1d2ce01b5d1bd8d17196969558b9ceeb186f0f729ddd44"), bigNat [ 14436861, 8 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "64cc50e1ad676b598aaaa552dd187a4a79a6567027236a19c19222d4"), bigNat [ 60372467, 28 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "67a93df9836b6a8b4a4424c4fa22f81310ae8393c14190c02e15a384"), bigNat [ 37464490, 2 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "695df5cfe3ea024049c4aef367ddd65bf89af62304a1327ab5fa38c7"), bigNat [ 40289816, 182 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "6a15a8cb2aba9468391e2f86cb425bff17fc37038c1b10f631dfff52"), bigNat [ 10927545, 252 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "6a6f72f8b7bc856b7984da4b56aba5c03307c75b43d6e43854b45859"), bigNat [ 31469844, 485 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "6aca00bf64ef28394337887845b98e58147a505f55a6cb4b3f99ddba"), bigNat [ 44374112, 1 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "6b408dc175dd7193dfa9db64829da2b883841c625641e4da37123505"), bigNat [ 52519856, 173 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "6bedc05103b60a74da2ee66469af73e18eb8c0df2fdffe822150fb38"), bigNat [ 33714717, 186 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "6c283f32874c3a72f50a1b7a1ea7e6c02ea32965ebeff0655f4067cc"), bigNat [ 42846175, 171 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "6ca5fab0747e89630b50b2da1fc95ec75490b78f124ec5d7256f2a38"), bigNat [ 24808698, 237 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "6d3b82adb860d112d68829f71421732bf1edd403ea53b17f11a95c90"), bigNat [ 18262243, 10 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "6d9af9305f2537821371074abe1148d9ca45c04bdc85cf3428669e6d"), bigNat [ 57307875, 2 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "6e15b11603a7ecaadf948180cbe502ab91a6510436ab1f8735b86a62"), bigNat [ 8255642, 37 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "6e209cdb8234d2c212bd037574f32006864f0362c008c05f032f8730"), bigNat [ 59893299, 1096 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "6ec83fee45f558942b2222d0bc063e4425098bc5c0b478a2a2128f05"), bigNat [ 61814016, 160 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "70951c5b5d889e59e8b9c38b0db440fed40630a3f00bb19187333929"), bigNat [ 55432280, 61 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "7234a73a063838b34887f132c24afbda0f059e39831520bc8d4d5a8d"), bigNat [ 23920301, 25 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "724ee90c9d307189ea2f15315672df7bf04520af50c96f975d0a1c4f"), bigNat [ 23094555, 171 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "72affb2b16c3e5b64a6abb5701c4ae4b85b8188b41fa6c1647b77cec"), bigNat [ 66525520, 3301 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "73e586947a19208727f200a6dc74c9d1004ad8a338e8b1bb9831c27a"), bigNat [ 178649, 79 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "748ec9a7bcecd63fff45a381030fe276352683389e96a11ae04b500c"), bigNat [ 37507823, 129 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "763bd12ec9b014338676bf3d9831e818d8f7858577a5e4c203b740bc"), bigNat [ 42639435, 67 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "7851629ff085367e43c10a6c5165bf0b60e24c4a7081c2127f7bdab3"), bigNat [ 60272821, 9 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "791c0e0cc88c918c0c677d53303b3ed2ad7cbe8355800e66e33f43a3"), bigNat [ 13872143 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "7d985e411578d9f394ce42a89a6fd9d4c91f08569c8d76281eb018a7"), bigNat [ 28705782, 86 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "7db29a8776226ea72595092da750a5a67182387a3110b2b01a20594d"), bigNat [ 64770721, 477 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "7e190374a6dc467568c274e3ad12d7e975440ebe6ddba66e8c27eb52"), bigNat [ 2081507, 103 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "7ebb7bee353d16731288bd7c85e507d80959fbafff403739d6663db1"), bigNat [ 30415434, 46 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "80850d1d8ed856124a78cadabeef2ad271101d1c8aa711f2922934ac"), bigNat [ 62238569, 3 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "80d0755799e40c6179ecc774e0f5af2d2db6a031abbd94c53814655a"), bigNat [ 63300186, 7 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "83e76c916dea1c2083371eb5c2aa92ed8a0836352054a009de5f012a"), bigNat [ 27046312, 297 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "844ecacbc2e8d87cd270c038bbb15ee57d3ee7f7fc35a6a714080608"), bigNat [ 26730472, 18 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "8565b09e73b318bb3f65babd29ed6377fcce728140a63d9f104f97d4"), bigNat [ 53028949, 12 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "87690a80d7f59d6840fafa0689b80e6545eb1807af03baddc212c407"), bigNat [ 56840298 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "87e30c728cc6b8d0d679eb4daf5ad181169238d48f3895037e10bb00"), bigNat [ 60220020, 48 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "89c30d3582f2c49ceee849c5dd50c2542c87cdada58038d2041a94de"), bigNat [ 9556750, 21 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "8b97f10f4388460cd798c25098978b320a9126e5aea114eaa019c7a7"), bigNat [ 2445294 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "8e449375adfa692b1616675ecf07128d6d2a5f55e68331e74c44eb90"), bigNat [ 57537026, 39 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "8fd507ce189686b9f911c82b39531b02549cb3a30a049dcb840e2674"), bigNat [ 28842783, 70 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "902b97f1e29034be53b0437a36d12dac16d351301a82f6ce543f0319"), bigNat [ 62677932, 24 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "90b42898f9d24af72ca706b92f8962981c9029e19d76c0456994c3a9"), bigNat [ 6519612, 122 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "910bfbbb6d6bb8cffb92c2ef6f2811cb7a51d4e538670baba7a533aa"), bigNat [ 53896387, 13 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "92e1c6528e39e4e706f4ddfd3ab46fb5e1ba86cca09e003c87fb7970"), bigNat [ 29139636, 43 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "9328af9a6f671ae8d16b485697e23b453252169e747684f00a040871"), bigNat [ 30523730, 19 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "938052389bf28701ed4d5dcbd75fca6ca8d3c82c2cdbd91baecc4d90"), bigNat [ 59191199, 89 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "94690064ec0b4703ba56d0c7058c82b775b98e0a8abf55d816524acd"), bigNat [ 36673013, 480 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "959eb0c50fb02199da22ee8ae6235f1e7634df1699d44749fa367555"), bigNat [ 37725973, 60 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "96f8728ecf0627a0cfbd17970950cba4055b2ca8068a82f0e264e7a6"), bigNat [ 15970137, 86 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "98aa41be6db0cb42219c53c133ee4152bd913ce67fcbcdaa4ee6f4ae"), bigNat [ 14714583, 8 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "9987bafb94c63269f6ad0e6e61dde7fe6c69965d1bf1e40624580f39"), bigNat [ 25561067, 780 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "9a35241e655e6056dcec4e6821a7edb33e57c1e6dc82975664f1dd4c"), bigNat [ 4244764, 195 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "9b98b35297ad2fe34001364456634ea9da1bdc62bc9e9f0bb429443c"), bigNat [ 55493902, 1 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "9ff135f2045ceddb5bd8e1a7aec05f6f7feccbef499cf7489f2ec344"), bigNat [ 61175404, 61 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "a04ecc89fdc3d58c97da62edff57367e98d13e1562819e8696533582"), bigNat [ 56384254, 387 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "a15d19cecce1e73e09c534d971eb6d292f37ba70104f61ba2407935a"), bigNat [ 48873275, 319 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "a29839e1f99e66f815c839bab224577bd6914ea8263bcd0af493f1de"), bigNat [ 24814267, 79 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "a4ca6b70c0be443386916c9c3dd3f4031b73017f6fba647dfa604d26"), bigNat [ 34911310, 3396 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "a4cb6c8e56ab4c71cd9a9e5b88d2705825e097fa58473585ef3eefd4"), bigNat [ 63585533, 91 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "a5da28796493691f0c45cd1d3e204d476b46b98a49db90da4e54cdd6"), bigNat [ 45675966, 824 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "a66d3c24bab6bb7d0f65209792682888f072b350ce3fb567f406eeec"), bigNat [ 42715566, 10 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "a85227c28dbbbaa49002f02b71caa6dec884a09b422b2ac832828a3b"), bigNat [ 5508202, 713 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "a860252947c21b9d16ff6f63ebf9e8fc1fdee23e4dcec4ac5acaf5d3"), bigNat [ 18711453 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "a86abca03ba2b54b25f05ad0dfd02bdb194b4996607977fde211d243"), bigNat [ 41599732, 201 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "a88f757e9e177c39cfddcf015aa75d4fbb002e4daebb77e4ac1c099e"), bigNat [ 4900721, 10 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "a8c60b21a26fb53932ab7436590c08ddcd679d56d4f3c2f590d012fc"), bigNat [ 36495318, 14 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "aaec63d7c8a6b9a9f478e4bce149734818df63715a527ea37d102a1c"), bigNat [ 46376472, 47 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "ab0c4b917f0b8c922c36621fa193dcce7895a8974c451e0318111351"), bigNat [ 35333141 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "ac482f1aca71a41bf8a4f36cd8b2e4e926a2d7a3ed7704b40fedc2f0"), bigNat [ 50206191, 3367 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "ac6e6ced26048ea810d84b33cce61e817f2e7fc9a183c7ba2d45a60b"), bigNat [ 61038743, 61 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "ac8434c64cfd18e6d806ba3c0f55120927277cc5e36f53054f3d9a3a"), bigNat [ 1549228, 1 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "ac899acee744ec0d790ac650a78c12bf7f0d8c224c7e41716863d656"), bigNat [ 63721426, 263 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "ac92d85538432b20d18d1196398cae90943a437fa22da1d9eb318b67"), bigNat [ 18152599, 59 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "addeda62a36f8724910416536bef64a5dc2b59235cd3d04377d56031"), bigNat [ 45341260, 9 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "ae4adfa24df9da6e6d0cd04154abd222904cc8915595c3c0157659d8"), bigNat [ 46063441, 88 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "b0c81e93af4adf9af67e4df06bef2097ee137a9491d48021c68099fb"), bigNat [ 42487954, 11 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "b1156d7c21310ee196431264860cc8f22036ed182870ccff4881df1d"), bigNat [ 50764962, 294 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "b12ac10f7a4aa34ebaa3dd2229afbc610b42732dec8a4605c05071aa"), bigNat [ 32350951, 650 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "b26650089d0bc0405e3b595af3cb7853c43ab713eb98c059c0f1044c"), bigNat [ 14660904, 128 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "b2ddc0f557f738ba1d55ab955f4fdc10e7a1d82fe6daa2c5f84a9455"), bigNat [ 5445038, 1336 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "b3823a62b98e60221ec3039570f78c235d4a0a84800de56ecaecfdb7"), bigNat [ 1993055, 27 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "b4b52a7c6fd4ea957a30dce0294626a4fcc1473b07b03b088f9619e6"), bigNat [ 64728786, 53 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "b6fcb972aabe45a4d6e202c49924713d120d237d8d1a18bd60e05290"), bigNat [ 39999146, 37 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "b7b964c39e00e8789df147d0e792066bc64c77a5e509e82fa784801c"), bigNat [ 35055108, 106 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "ba83704986bff4e9a997b84fb5b5bea94c1bda4450188b14dc2a9832"), bigNat [ 56731892, 6 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "baf524f9631f2baa1b6171be65f6d65360d554fcfe4584c486f3bca5"), bigNat [ 313120, 35 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "bce4f7002c53c01c6962006f3e9b2fbc3ab0d70342b8a21dba5f68d3"), bigNat [ 163351, 174 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "bebb1b0bd5dbdef3bf165424a930bd274a2c91e7c4457d3a4d88f425"), bigNat [ 16743942 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "bf7515f690ad911cb1cf7654aafca00de7e31e35ed64180e0eb4c8c8"), bigNat [ 50396547, 177 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "c150420070296f74a77983162f3e36c8067b94cf390bbb086759b8a8"), bigNat [ 13030247, 1844 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "c41e4f71342c58b9b3b4c42c916ccbdf3b2a342d44c7f2c67c91a3d7"), bigNat [ 16483257, 338 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "c4aef88847774c66d96850df7d63df294eed34fd7a550b409d997673"), bigNat [ 21561456, 1361 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "c62d6b0badd636522d651c78e074f5dac8c6ce597f869dd78e674eb9"), bigNat [ 51982804, 83 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "c631165c7c41675a1cf3feed316829147ffaf05f0cd42cf60b2024a7"), bigNat [ 53484221, 16 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "c71bbf37b84106fe6208f3a54d1eead0063753353fde5a731e099529"), bigNat [ 61641855, 47 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "c7cdfe51df919c1a3919779383b9f1231c7857e1c325da3b1869643e"), bigNat [ 25761164, 24 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "cca83cb796d6ebc3ead2039824058963d5a5100d626c174245fe96ba"), bigNat [ 64546706, 3 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "ce66658c36cbd0daf19982cf5a125b262da3bf2b12ce053cce5e2847"), bigNat [ 49653520, 20 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "cf040215e6df1129342b99039740e984806addfb8b954980e3d5ba89"), bigNat [ 61915902, 4581 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "d30ba2531eff5bd174675cb1d9b3c4f2b3d66d947f0897007b41465b"), bigNat [ 63823739, 124 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "d55c9a81d5be5317ceedffd0328848bd72d638e84ac8631589396eb6"), bigNat [ 7653484, 11 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "d8cdd04f5f91bb57e6cee3ea3a487b928a8f1003170fdd91421c879e"), bigNat [ 3991150, 1 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "d9edc067383928ede002f78cdf700e8f5ad0e1682066440066622f8b"), bigNat [ 12576741, 10 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "db63546e634bec5828998b4a3dbb0930f664c4d1bcce586b9aa0b337"), bigNat [ 6742755, 13 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "dd0f9a12cbbf2d93556e17d46d9fec4192b7048863d33c4c06732360"), bigNat [ 2226042, 237 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "ddae2af68ff6af9646283e32eb8dc815bba7d13216a745748d497a8a"), bigNat [ 54680836 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "e24bf093e19c1273405a7b86bc0f8d091fbf4298951892135642c849"), bigNat [ 27662782, 78 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "e33f562ae3371c65d68bc02c1a9e0413ac6ec3fb2f65064d6cb42e1c"), bigNat [ 48195963, 165 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "e47de36472a866f88b5c9e115041848a5efa6ce05a72be9a9b28a441"), bigNat [ 10923980, 109 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "e84cdca7675085f4fcb32d906b3ad6d383752caefd8fb059ad5329fa"), bigNat [ 32415747, 404 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "e8ce8deab0fcac7f0b9ee858ba54d4fe2fc813fc834040c7aa733a68"), bigNat [ 12658511, 1 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "e9eb11829f1adbe9db107ae0466e57885e17d69ba59f06bf85996f63"), bigNat [ 21959409, 208 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "ea025ee9adf4163f941ce9e5f20e79a113c00de8390c9e5f354f362d"), bigNat [ 29363904, 58 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "ea9d83db8104bb696c471821e20bee6aaff18e813b4be446822d837d"), bigNat [ 11005078, 35 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "eafcae5bd7b5bfa44825dc372c709a99716d2c02cf29e7291ce4e7b3"), bigNat [ 49864365, 40 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "ebfcaec39f6feeb63f9b389abac90df1eacbee40504438a375d76d73"), bigNat [ 64267189, 32 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "ed1dbf3241ad9aac4ecd7239f00eaa6d9b00da4cbecf547f2b0c9ab5"), bigNat [ 57563456, 1 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "ed92944b42a0cdc4be0fceee007e0be2f06d23cf75606afbe545ec95"), bigNat [ 4978923, 21 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "eda87d0938cab2e4567a47411b95a27eb00aeb8218fb17bf6e5365a7"), bigNat [ 53587108, 149 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "ede458847c712968c6ea5218c265875b71397beed3f1ee31f54039c1"), bigNat [ 59045335, 102 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "ede783e3b86e790429cdb2c5daa5d1e30714f1d6b53e414f475b8d4f"), bigNat [ 53299562, 47 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "eed2fea7bf1bdec24b06951c23e8fed7493262e223c0488efa360ac8"), bigNat [ 5253649, 2846 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "f0b5051dccdfdb93921b2baff02e169010ef467e93cc69e1350e5dae"), bigNat [ 20788234, 171 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "f0da723ef196b602ce56070f42281258ef11c38e203935359e3eaa2c"), bigNat [ 39117754, 179 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "f23582610378a83a3944b7e2023932e0d445a223461249fe3de319a1"), bigNat [ 41728007, 97 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "f3cc6cf632613d62331d546afff0257b15783d39b81b47bd7a9a6a96"), bigNat [ 48783453, 125 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "f6b12722038ed4ce1375c8e6ec6de7daeec1b9c27811493c75ec4565"), bigNat [ 37182872, 3 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "f6bd5b9191660c0ca7cada2589aa5e84f48751339c99652fac413285"), bigNat [ 17775096, 22 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "f75a6f9c8d3c0163a506923234e66eb7404503394c0824ebdcc815d5"), bigNat [ 42402503, 13 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "f89dcde6687826def543b89cbae0925da0bc3ca9ecbb43a06c4a644b"), bigNat [ 4001989, 135 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "f8c118b504cd570a91429d997da109962d2b4cd9671bf83e44aaaec6"), bigNat [ 4208858, 160 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "fd2ffd2c7692c6c733b5f30ada30c9d57f11f792b5a875163efaf60b"), bigNat [ 57355778, 683 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "fecffc4b11438299d925367829fd9c1745e36d98dd93d3bb0c20b126"), bigNat [ 20927705, 172 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "ffa66135c2895c2b8ed91cd956883a3ca9a355709ff60938140499ab"), bigNat [ 36354869, 13 ] )
                        , ( Address.VKeyHash (Bytes.fromHexUnchecked "ffddc35c5c2085e54f527c610d8408cbbc7f26182b0aa8efc39f1c73"), bigNat [ 24661031, 72 ] )
                        ]
                }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "62b3fade57272bfba2b7abd7877dabcf79b332a4b6fe373084ccd53e") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "1d1692b16cd0fbe005e45abb77383d02bee4ed86ef1eb2886659498d") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "388ede6f04652931f309e1901e7cc8959e2809390ef4fb3f4f7a7dea") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "addeda62a36f8724910416536bef64a5dc2b59235cd3d04377d56031") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "4ed709c5054447bf326aebb31ba47f4ddb82acf81f00172c37116cfc") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "2e54bf801f57a14174679d6772011bbff179188bc9b68e9e073a5472") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "ac482f1aca71a41bf8a4f36cd8b2e4e926a2d7a3ed7704b40fedc2f0") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "9a35241e655e6056dcec4e6821a7edb33e57c1e6dc82975664f1dd4c") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "938052389bf28701ed4d5dcbd75fca6ca8d3c82c2cdbd91baecc4d90") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "a4cb6c8e56ab4c71cd9a9e5b88d2705825e097fa58473585ef3eefd4") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "2fe954dd15471788fc832ac41415a185e21d0cfc6f5d94ffc9756e6d") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "45796e1560faa06b777598d53faf54cd8fb77169cbd48eb5731bb920") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "4decd172f4102fffda2562fad82ce6926d386977d8e6d5c94907801b") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "f23582610378a83a3944b7e2023932e0d445a223461249fe3de319a1") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "06cac706110395e66ac8913077e093539bbdcab1313433bf503b60c6") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "695df5cfe3ea024049c4aef367ddd65bf89af62304a1327ab5fa38c7") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "a66d3c24bab6bb7d0f65209792682888f072b350ce3fb567f406eeec") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "ea9d83db8104bb696c471821e20bee6aaff18e813b4be446822d837d") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "6aca00bf64ef28394337887845b98e58147a505f55a6cb4b3f99ddba") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "b2ddc0f557f738ba1d55ab955f4fdc10e7a1d82fe6daa2c5f84a9455") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "724ee90c9d307189ea2f15315672df7bf04520af50c96f975d0a1c4f") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "a5da28796493691f0c45cd1d3e204d476b46b98a49db90da4e54cdd6") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "90b42898f9d24af72ca706b92f8962981c9029e19d76c0456994c3a9") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "391825387fdff28dc24a4f2a7748d590ffbc4b4d8a8a6f0f836baf58") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "94690064ec0b4703ba56d0c7058c82b775b98e0a8abf55d816524acd") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "b4b52a7c6fd4ea957a30dce0294626a4fcc1473b07b03b088f9619e6") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "d55c9a81d5be5317ceedffd0328848bd72d638e84ac8631589396eb6") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "eafcae5bd7b5bfa44825dc372c709a99716d2c02cf29e7291ce4e7b3") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "5b728b0bcf80bf766688ea376c07f09d3dc9a265553f22b25b754228") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "fd2ffd2c7692c6c733b5f30ada30c9d57f11f792b5a875163efaf60b") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "7851629ff085367e43c10a6c5165bf0b60e24c4a7081c2127f7bdab3") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "7d985e411578d9f394ce42a89a6fd9d4c91f08569c8d76281eb018a7") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "a860252947c21b9d16ff6f63ebf9e8fc1fdee23e4dcec4ac5acaf5d3") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "c71bbf37b84106fe6208f3a54d1eead0063753353fde5a731e099529") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "3581b417908dea107b5abf821dcce6dd19f732af44ed6b6bb2d74965") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "2c459301ae68445b96991ca0eaef73d7869991f1c283af360e220885") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "bf7515f690ad911cb1cf7654aafca00de7e31e35ed64180e0eb4c8c8") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "a4ca6b70c0be443386916c9c3dd3f4031b73017f6fba647dfa604d26") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "5d5a181ff3fc68cf7361cba7b7382efff80470497d6e50eca7778b94") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "ac8434c64cfd18e6d806ba3c0f55120927277cc5e36f53054f3d9a3a") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "eda87d0938cab2e4567a47411b95a27eb00aeb8218fb17bf6e5365a7") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "f89dcde6687826def543b89cbae0925da0bc3ca9ecbb43a06c4a644b") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "b26650089d0bc0405e3b595af3cb7853c43ab713eb98c059c0f1044c") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "80850d1d8ed856124a78cadabeef2ad271101d1c8aa711f2922934ac") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "67a93df9836b6a8b4a4424c4fa22f81310ae8393c14190c02e15a384") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "b3823a62b98e60221ec3039570f78c235d4a0a84800de56ecaecfdb7") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "63181e6e8e5ee95f1c671cf38baee666721816efcb190c288e8fff9d") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "a15d19cecce1e73e09c534d971eb6d292f37ba70104f61ba2407935a") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "a8c60b21a26fb53932ab7436590c08ddcd679d56d4f3c2f590d012fc") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "ede783e3b86e790429cdb2c5daa5d1e30714f1d6b53e414f475b8d4f") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "2be784dfd7253aebb3e8750ec0d9ef33054269dbacf8364d854f41ef") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "ae4adfa24df9da6e6d0cd04154abd222904cc8915595c3c0157659d8") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "6e209cdb8234d2c212bd037574f32006864f0362c008c05f032f8730") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "e84cdca7675085f4fcb32d906b3ad6d383752caefd8fb059ad5329fa") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "39ff4977c8c3d37dba57d2044867650266f6e4c75c6a1665ac3765c6") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "dd0f9a12cbbf2d93556e17d46d9fec4192b7048863d33c4c06732360") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "f3cc6cf632613d62331d546afff0257b15783d39b81b47bd7a9a6a96") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "19a6a21c3411bb8065859b2c91f74331f8d5a036b3b79184b6689ce4") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "f8c118b504cd570a91429d997da109962d2b4cd9671bf83e44aaaec6") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "e47de36472a866f88b5c9e115041848a5efa6ce05a72be9a9b28a441") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "f0da723ef196b602ce56070f42281258ef11c38e203935359e3eaa2c") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "89c30d3582f2c49ceee849c5dd50c2542c87cdada58038d2041a94de") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "058b6e56f91b7c34e367fa2fd65a64b7a679bd7bfa62e36e210d80d8") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "ce66658c36cbd0daf19982cf5a125b262da3bf2b12ce053cce5e2847") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "22abaff6207509c0bccdba86dceaeb3ef450452a98831d913c7cb53b") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "e9eb11829f1adbe9db107ae0466e57885e17d69ba59f06bf85996f63") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "22a7719fe0a30b4977467de59a04d7b5e9d56caff57e665db0ac9f81") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "27978020d4d87c686e3a37fd4cba304eb5d6af4ed9e6ab4457d5afcd") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "902b97f1e29034be53b0437a36d12dac16d351301a82f6ce543f0319") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "8fd507ce189686b9f911c82b39531b02549cb3a30a049dcb840e2674") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "f6b12722038ed4ce1375c8e6ec6de7daeec1b9c27811493c75ec4565") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "a88f757e9e177c39cfddcf015aa75d4fbb002e4daebb77e4ac1c099e") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "6a15a8cb2aba9468391e2f86cb425bff17fc37038c1b10f631dfff52") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "cca83cb796d6ebc3ead2039824058963d5a5100d626c174245fe96ba") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "b1156d7c21310ee196431264860cc8f22036ed182870ccff4881df1d") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "04b4af2f4536466c9d0010ea94c7319db6eff7a97e7f26cdb1e773d4") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "4f8dda033362e9c8840206563925b9896263b103d3e45e9a0c28d455") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "6ec83fee45f558942b2222d0bc063e4425098bc5c0b478a2a2128f05") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "0bf0271768bb0cb210d400f5352a578121aba403c5ec00405a294880") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "33e85b999cdd061ec49ea20228112dd92a108e21e067ec48a167b82f") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "ba83704986bff4e9a997b84fb5b5bea94c1bda4450188b14dc2a9832") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "87690a80d7f59d6840fafa0689b80e6545eb1807af03baddc212c407") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "ffa66135c2895c2b8ed91cd956883a3ca9a355709ff60938140499ab") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "7e190374a6dc467568c274e3ad12d7e975440ebe6ddba66e8c27eb52") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "b6fcb972aabe45a4d6e202c49924713d120d237d8d1a18bd60e05290") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "31b1f312087655265e40461acd2f4dda62cd69c0613570b2f91efc32") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "6a6f72f8b7bc856b7984da4b56aba5c03307c75b43d6e43854b45859") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "c41e4f71342c58b9b3b4c42c916ccbdf3b2a342d44c7f2c67c91a3d7") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "233f410282657b90069dccdf0c03a78de31074869756f20bc417aebb") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "cf040215e6df1129342b99039740e984806addfb8b954980e3d5ba89") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "446e15940325aae70c01aa0c96d079bd68a0b2b1a6324879491210c9") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "fecffc4b11438299d925367829fd9c1745e36d98dd93d3bb0c20b126") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "ac6e6ced26048ea810d84b33cce61e817f2e7fc9a183c7ba2d45a60b") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "c7cdfe51df919c1a3919779383b9f1231c7857e1c325da3b1869643e") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "db63546e634bec5828998b4a3dbb0930f664c4d1bcce586b9aa0b337") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "6b408dc175dd7193dfa9db64829da2b883841c625641e4da37123505") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "ebfcaec39f6feeb63f9b389abac90df1eacbee40504438a375d76d73") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "6d3b82adb860d112d68829f71421732bf1edd403ea53b17f11a95c90") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "0cf972e71a248dc9a59bad9b2244bbc82f89060352e28b22ac8d4655") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "090a63ca4e67569d49e7e9dc499ebeb847d8e60fb37d1963581368c6") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "c150420070296f74a77983162f3e36c8067b94cf390bbb086759b8a8") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "aaec63d7c8a6b9a9f478e4bce149734818df63715a527ea37d102a1c") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "06cf8d573a5d9ff295eafcc48938212c9296871d13ff905db1a604f1") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "4597ec4cc1c4d012dc8d9a8c4320773d88b739449467b4f445d34bf4") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "c631165c7c41675a1cf3feed316829147ffaf05f0cd42cf60b2024a7") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "ab0c4b917f0b8c922c36621fa193dcce7895a8974c451e0318111351") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "4d33eda5f702de4deed05ed99ee0a8bfc456742a4c8bfa4020f34548") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "eed2fea7bf1bdec24b06951c23e8fed7493262e223c0488efa360ac8") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "438ea0cc2a75da6a3d60708a5dd879a7b1d63891a81dc42bdd2908db") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "24c7ee461f15e9cb6df13427b4685c01b5c5b5ef2954dc13e14aa74f") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "36eafaae91e48b329f24eb2ca7bb71c8ca5ea41c9582e8500d4b63e5") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "b12ac10f7a4aa34ebaa3dd2229afbc610b42732dec8a4605c05071aa") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "9987bafb94c63269f6ad0e6e61dde7fe6c69965d1bf1e40624580f39") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "4d1307081d1362fb9e3debd029adac66d3ec947579905d266cccc569") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "33682102a3202c26cebf11d6e360b96291c2dead24560914bc15e1e7") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "24074f532b2ae266b0ab5331b0d6a8383e31fa15145691b6d9f57998") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "ed1dbf3241ad9aac4ecd7239f00eaa6d9b00da4cbecf547f2b0c9ab5") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "910bfbbb6d6bb8cffb92c2ef6f2811cb7a51d4e538670baba7a533aa") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "641ca9753c1d2ce01b5d1bd8d17196969558b9ceeb186f0f729ddd44") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "791c0e0cc88c918c0c677d53303b3ed2ad7cbe8355800e66e33f43a3") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "3f986f0b9fdc99a418c9a0f972f9c6b9454c5754a81bdccec40dc09d") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "3c417ea0dc93cb8b1f8c92b2e473434bb20c18979e800f4ec531eebb") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "a86abca03ba2b54b25f05ad0dfd02bdb194b4996607977fde211d243") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "ac92d85538432b20d18d1196398cae90943a437fa22da1d9eb318b67") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "8565b09e73b318bb3f65babd29ed6377fcce728140a63d9f104f97d4") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "6bedc05103b60a74da2ee66469af73e18eb8c0df2fdffe822150fb38") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "d8cdd04f5f91bb57e6cee3ea3a487b928a8f1003170fdd91421c879e") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "83e76c916dea1c2083371eb5c2aa92ed8a0836352054a009de5f012a") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "1dfc6bf6eddfa4120690126942972974ed3a0a931c1dac6fd051cf8e") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "80d0755799e40c6179ecc774e0f5af2d2db6a031abbd94c53814655a") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "50756852527d09065c47572e9b29fb5a63bc3f393b6326cb19e34c21") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "f75a6f9c8d3c0163a506923234e66eb7404503394c0824ebdcc815d5") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "bebb1b0bd5dbdef3bf165424a930bd274a2c91e7c4457d3a4d88f425") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "73e586947a19208727f200a6dc74c9d1004ad8a338e8b1bb9831c27a") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "13ee747308779b78902d5f7b6e7608b9523885d9b7c500db0459b73e") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "6d9af9305f2537821371074abe1148d9ca45c04bdc85cf3428669e6d") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "64cc50e1ad676b598aaaa552dd187a4a79a6567027236a19c19222d4") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "d9edc067383928ede002f78cdf700e8f5ad0e1682066440066622f8b") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "ed92944b42a0cdc4be0fceee007e0be2f06d23cf75606afbe545ec95") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "ede458847c712968c6ea5218c265875b71397beed3f1ee31f54039c1") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "35ef3f2e454f77519961026ecf25d29616db1b600c6d123d0e3a0b58") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "bce4f7002c53c01c6962006f3e9b2fbc3ab0d70342b8a21dba5f68d3") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "21227adb44903d2de668696200ec709ee4f92d0009130123ef19c3c3") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "98aa41be6db0cb42219c53c133ee4152bd913ce67fcbcdaa4ee6f4ae") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "ea025ee9adf4163f941ce9e5f20e79a113c00de8390c9e5f354f362d") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "959eb0c50fb02199da22ee8ae6235f1e7634df1699d44749fa367555") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "b0c81e93af4adf9af67e4df06bef2097ee137a9491d48021c68099fb") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "5d3a2e1a6aee1687bde20b2b374ee427a8159b6b7cd1ff481037ea3e") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "6e15b11603a7ecaadf948180cbe502ab91a6510436ab1f8735b86a62") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "ddae2af68ff6af9646283e32eb8dc815bba7d13216a745748d497a8a") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "e8ce8deab0fcac7f0b9ee858ba54d4fe2fc813fc834040c7aa733a68") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "c4aef88847774c66d96850df7d63df294eed34fd7a550b409d997673") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "748ec9a7bcecd63fff45a381030fe276352683389e96a11ae04b500c") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "7db29a8776226ea72595092da750a5a67182387a3110b2b01a20594d") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "1f69478b9a753e969d5085e40439993fc4358e94db8cc174ddddfa98") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "25606e7ec45d235b74c5a0dcf2866929f955f1803a4d4da6b538e039") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "72affb2b16c3e5b64a6abb5701c4ae4b85b8188b41fa6c1647b77cec") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "9b98b35297ad2fe34001364456634ea9da1bdc62bc9e9f0bb429443c") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "4e83fbacea4d13d1c985dbea19286c849a5ea04cb68a611c5ca3b375") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "87e30c728cc6b8d0d679eb4daf5ad181169238d48f3895037e10bb00") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "1010834c8e3f8d7b6eb04a9181f0ff40e780429dd513662bf32c9c80") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "844ecacbc2e8d87cd270c038bbb15ee57d3ee7f7fc35a6a714080608") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "e24bf093e19c1273405a7b86bc0f8d091fbf4298951892135642c849") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "8b97f10f4388460cd798c25098978b320a9126e5aea114eaa019c7a7") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "f6bd5b9191660c0ca7cada2589aa5e84f48751339c99652fac413285") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "19ebdabf80584f3df8640922ab156ead84eafd23b89dde7e19d6a7fa") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "9ff135f2045ceddb5bd8e1a7aec05f6f7feccbef499cf7489f2ec344") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "37557722eb102c5737b85f2332d5cd6baea6257ede6f0945389c03b1") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "d30ba2531eff5bd174675cb1d9b3c4f2b3d66d947f0897007b41465b") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "2545642a9c1bfedf95bf30438849080aabc4219a227f209a84b72ee3") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "baf524f9631f2baa1b6171be65f6d65360d554fcfe4584c486f3bca5") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "017a39dc9198c69f55e61e9edfcd4506d38c41703d7454cf4cb6a882") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "6c283f32874c3a72f50a1b7a1ea7e6c02ea32965ebeff0655f4067cc") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "c62d6b0badd636522d651c78e074f5dac8c6ce597f869dd78e674eb9") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "763bd12ec9b014338676bf3d9831e818d8f7858577a5e4c203b740bc") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "00de27c8609df0a7f7ae090224ace1d68cc5d338a335f1c6bb583d72") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "92e1c6528e39e4e706f4ddfd3ab46fb5e1ba86cca09e003c87fb7970") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "ac899acee744ec0d790ac650a78c12bf7f0d8c224c7e41716863d656") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "e33f562ae3371c65d68bc02c1a9e0413ac6ec3fb2f65064d6cb42e1c") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "5fba4a7603fbb18fb1d58c0c00dc01eb4b919f2ce7d1afc92a19e399") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "17fac4618ea56ec6e12e9f6f818e8af5d98ea18c1681ba78aac44e1e") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "0477455a3a4a0b87c1be90c77c6b376841f8f4c52e42b47691baac20") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "7ebb7bee353d16731288bd7c85e507d80959fbafff403739d6663db1") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "13ce7ba99b254954ddc361445ff6bb3673320bd3d5da3615d9277b19") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "9328af9a6f671ae8d16b485697e23b453252169e747684f00a040871") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "6ca5fab0747e89630b50b2da1fc95ec75490b78f124ec5d7256f2a38") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "f0b5051dccdfdb93921b2baff02e169010ef467e93cc69e1350e5dae") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "a04ecc89fdc3d58c97da62edff57367e98d13e1562819e8696533582") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "ffddc35c5c2085e54f527c610d8408cbbc7f26182b0aa8efc39f1c73") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "41e22a8c23ff59767ab1c6f4285350fb3c8b7bb3e5308d8cd9813817") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "62566a741704214440ef14350ec5ab1919f0e2f6dce0636f3d616c12") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "1d6a5923d8c491a898cb9de9ec1f84d6d3b4332c9610e4924b4cf5b7") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "b7b964c39e00e8789df147d0e792066bc64c77a5e509e82fa784801c") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "499fb62d49ef0c0183e176166b543feaceb7af7f9074a17e078f7106") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "8e449375adfa692b1616675ecf07128d6d2a5f55e68331e74c44eb90") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "a85227c28dbbbaa49002f02b71caa6dec884a09b422b2ac832828a3b") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "a29839e1f99e66f815c839bab224577bd6914ea8263bcd0af493f1de") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "70951c5b5d889e59e8b9c38b0db440fed40630a3f00bb19187333929") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "7234a73a063838b34887f132c24afbda0f059e39831520bc8d4d5a8d") }
            , Transaction.StakeRegistrationCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "96f8728ecf0627a0cfbd17970950cba4055b2ca8068a82f0e264e7a6") }
            ]
    }


txWitnessSet35d2728e : WitnessSet
txWitnessSet35d2728e =
    { newWitnessSet
        | vkeywitness =
            Just
                [ { signature = Bytes.fromHexUnchecked "04f7d6e3c5af19cf76ec46b6a876f688c5ea83f8bfd6cac4b26fb5944622584a1cd8a986bfef66a25dac652ec4d6004b2f250dce9d60e3a4efdb2e8520d2a403"
                  , vkey = Bytes.fromHexUnchecked "61261a95b7613ee6bf2067dad77b70349729b0c50d57bc1cf30de0db4a1e73a8"
                  }
                , { signature = Bytes.fromHexUnchecked "c19ed423f94f6100099305f47a44ab97d5ace1a1c654227f018229b71e923d6318a142bc9a68c89fd5206293bb612b1416ec48cc8535d1e967a9374ba44a990a"
                  , vkey = Bytes.fromHexUnchecked "9180d818e69cd997e34663c418a648c076f2e19cd4194e486e159d8580bc6cda"
                  }
                , { signature = Bytes.fromHexUnchecked "1e84dc558773939db3abd63e9047e9c66c337eacbd3f341e6995f9bf2370ffa36dc445774b95e815adba96f4135aeefbbdffae1df29ef1fcab782b6cb6105105"
                  , vkey = Bytes.fromHexUnchecked "89c29f8c4af27b7accbe589747820134ebbaa1caf3ce949270a3d0c7dcfd541b"
                  }
                , { signature = Bytes.fromHexUnchecked "97027d20a4ea31963a7c04fd1bed3b6ec760cdacba3f6cd3710e3a8db6d3623bf541ed5596fbf7cb76c55335d28c7cfa165c0bf11e15f2fee6419c201b149401"
                  , vkey = Bytes.fromHexUnchecked "f14f712dc600d793052d4842d50cefa4e65884ea6cf83707079eb8ce302efc85"
                  }
                , { signature = Bytes.fromHexUnchecked "69770d513b8cd91cf751dd21a817481b9a25e052df1f1efc47e7c245c031f8977ffb371b64b9001d200140cd556950cff923086eed32ee4d41ad0c1acb987b0f"
                  , vkey = Bytes.fromHexUnchecked "8b53207629f9a30e4b2015044f337c01735abe67243c19470c9dae8c7b732798"
                  }
                , { signature = Bytes.fromHexUnchecked "bdf86ad2f72f238e58b1815594ca65c1ceac36373ef79fedfd971be0dc5f23233fff7b209b693ac09c3295f398667197695a1efc8fb31ea58acd1bf983794703"
                  , vkey = Bytes.fromHexUnchecked "5fddeedade2714d6db2f9e1104743d2d8d818ecddc306e176108db14caadd441"
                  }
                , { signature = Bytes.fromHexUnchecked "740e0dad0843c68a286c267fcd08e7e9395f23f14b99f5adbd777c1fd646e7d11d0956210951ad6c9b18734751759dde18cf908d6ef1c4a463f761d0389c370f"
                  , vkey = Bytes.fromHexUnchecked "cbc6b506e94fbefe442eecee376f3b3ebaf89415ef5cd2efb666e06ddae48393"
                  }
                , { signature = Bytes.fromHexUnchecked "7bbb8ffec33f55bd4065e33042a1a50f65b519c39d7f7d5b7878e9f72665f6870a98cbc85293d59d0616c02d1b6b1e7deefa3fc167de28e06047240755c49401"
                  , vkey = Bytes.fromHexUnchecked "e8c03a03c0b2ddbea4195caf39f41e669f7d251ecf221fbb2f275c0a5d7e05d1"
                  }
                ]
    }


{-| Next Shelley failure.

Tx id: a2d8a92709fecadaa0b641ee296d2e0b512b4e81f8eadab4fed84769dcb549de
Block height: 4497888
Previous block intersection:

  - slot: 4640780
  - id: f8fa3f085cebc2094b4e3df6d46ee184af6ab44935626b21511aab9983750b1b

-}
decodea2d8a927 : Test
decodea2d8a927 =
    test "Tx id a2d8a92709fecadaa0b641ee296d2e0b512b4e81f8eadab4fed84769dcb549de" <|
        \_ ->
            Bytes.fromHexUnchecked "83a40081825820a6afff5e962033731b67a256b9205fdaadc57faa06793dbde553dd26a2cd17320001828258204186880a8bb19ec8742db9076795c5107f7ffc65a889e7b0980ffeaca20c0c0c1a000f424082581d6186880a8bb19ec8742da9076795c5107f7ffc65a889e7b0980ffeaca21a001398fd021a00034a63031a7fffffffa10081825820d08529ec8ca7640d0e79848d0d4c790743b88f6455d32f00437b28aebf943ffe58400c836a9aa1d5b32b7e282add2458e1a71e8828e34e6e2ad3ad50454e7bee4a3c83222d5b16d466be02d3e969684c1e09005b2c16a8ca26aabcbd344f56e8cb09f6"
                |> Transaction.deserialize
                |> Expect.equal
                    (Just
                        { body = txBodya2d8a927
                        , witnessSet = txWitnessSeta2d8a927
                        , isValid = True
                        , auxiliaryData = Nothing
                        }
                    )


txBodya2d8a927 : TransactionBody
txBodya2d8a927 =
    { newBody
        | fee = N.fromSafeInt 215651
        , inputs = [ { outputIndex = 0, transactionId = Bytes.fromHexUnchecked "a6afff5e962033731b67a256b9205fdaadc57faa06793dbde553dd26a2cd1732" } ]
        , outputs =
            [ { address = Address.Shelley { networkId = Mainnet, paymentCredential = Address.VKeyHash (Bytes.fromHexUnchecked "86880a8bb19ec8742db9076795c5107f7ffc65a889e7b0980ffeaca2"), stakeCredential = Just (Address.PointerCredential { certificateIndex = 12, slotNumber = 12, transactionIndex = 12 }) }
              , amount = Value.onlyLovelace (N.fromSafeInt 1000000)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            , { address = Address.Shelley { networkId = Mainnet, paymentCredential = Address.VKeyHash (Bytes.fromHexUnchecked "86880a8bb19ec8742da9076795c5107f7ffc65a889e7b0980ffeaca2"), stakeCredential = Nothing }
              , amount = Value.onlyLovelace (N.fromSafeInt 1284349)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            ]
        , ttl = Just (bigNat [ 67108863, 31 ])
    }


txWitnessSeta2d8a927 : WitnessSet
txWitnessSeta2d8a927 =
    { newWitnessSet
        | vkeywitness =
            Just
                [ { signature = Bytes.fromHexUnchecked "0c836a9aa1d5b32b7e282add2458e1a71e8828e34e6e2ad3ad50454e7bee4a3c83222d5b16d466be02d3e969684c1e09005b2c16a8ca26aabcbd344f56e8cb09"
                  , vkey = Bytes.fromHexUnchecked "d08529ec8ca7640d0e79848d0d4c790743b88f6455d32f00437b28aebf943ffe"
                  }
                ]
    }


{-| Next Shelley failure.

Tx id: 2383af0582da2b18039fab49ef4bb246f7d23d4304e36eb48f9387ff80adc769
Block height: 4512070
Previous block intersection:

  - slot: 4924900
  - id: 3b860fb2977c8201000da94e64e8c57acde24cc1b5e2742f7b9158a5e07769d2

-}
decode2383af05 : Test
decode2383af05 =
    test "Tx id 2383af0582da2b18039fab49ef4bb246f7d23d4304e36eb48f9387ff80adc769" <|
        \_ ->
            Bytes.fromHexUnchecked "83a5008182582072cb2dde1d5cea967255d6dd141aaf76801840033760f452763436bd1afc383601018182584c82d818584283581cce81d6b8d9f957ff8da898172fd08beea98e575b61e8ce01d0182372a101581e581c2b0b011ba3683d182bc2472ac7a7d5939b443746208b10c34367d9d6001a53e87e3b1b000000036ae06484021a000f4240031a004b420405a1581de1558f3ee09b26d88fac2eddc772a9eda94cce6dbadbe9fee439bd60011b000000036178a5aea10082825820e9b47b5d20a737cfc29b57642ef18723eff8b5a4bb6a8efb7ea0caa9a84f594d5840fe644cc6fe77d19bf75d766d2894246a5ad9c5482be671be5a81efb6ad694215e15312895bf8bb36150d9a15c80457017c2895792ad244da1d136ed50d41f60382582078d04608e7cf0d3dfc0b65a26ce571f47c17ea8e86f5b74c76a5794dfb7490c258401738eb2f7370490269cd5e513a2f7dbaad8c7694e45a3379127391c060ec938541e4233668e5479e17c7e706bcf45423301237aaee04c36aaf12aea897c3cb0ff6"
                |> Transaction.deserialize
                |> Expect.equal
                    (Just
                        { body = txBody2383af05
                        , witnessSet = txWitnessSet2383af05
                        , isValid = True
                        , auxiliaryData = Nothing
                        }
                    )


txBody2383af05 : TransactionBody
txBody2383af05 =
    { newBody
        | fee = N.fromSafeInt 1000000
        , inputs = [ { outputIndex = 1, transactionId = Bytes.fromHexUnchecked "72cb2dde1d5cea967255d6dd141aaf76801840033760f452763436bd1afc3836" } ]
        , outputs =
            [ { address = Address.Byron (Bytes.fromHexUnchecked "82d818584283581cce81d6b8d9f957ff8da898172fd08beea98e575b61e8ce01d0182372a101581e581c2b0b011ba3683d182bc2472ac7a7d5939b443746208b10c34367d9d6001a53e87e3b")
              , amount = Value.onlyLovelace (bigNat [ 48260228, 218 ])
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            ]
        , ttl = Just (N.fromSafeInt 4932100)
        , withdrawals = [ ( { networkId = Mainnet, stakeCredential = Address.VKeyHash (Bytes.fromHexUnchecked "558f3ee09b26d88fac2eddc772a9eda94cce6dbadbe9fee439bd6001") }, bigNat [ 24683950, 216 ] ) ]
    }


txWitnessSet2383af05 : WitnessSet
txWitnessSet2383af05 =
    { newWitnessSet
        | vkeywitness =
            Just
                [ { signature = Bytes.fromHexUnchecked "fe644cc6fe77d19bf75d766d2894246a5ad9c5482be671be5a81efb6ad694215e15312895bf8bb36150d9a15c80457017c2895792ad244da1d136ed50d41f603"
                  , vkey = Bytes.fromHexUnchecked "e9b47b5d20a737cfc29b57642ef18723eff8b5a4bb6a8efb7ea0caa9a84f594d"
                  }
                , { signature = Bytes.fromHexUnchecked "1738eb2f7370490269cd5e513a2f7dbaad8c7694e45a3379127391c060ec938541e4233668e5479e17c7e706bcf45423301237aaee04c36aaf12aea897c3cb0f"
                  , vkey = Bytes.fromHexUnchecked "78d04608e7cf0d3dfc0b65a26ce571f47c17ea8e86f5b74c76a5794dfb7490c2"
                  }
                ]
    }


{-| Next Shelley failure.

Tx id: 1bcd8fa799bd7d47f6af8d54e9ebda4756597991ca44b33dcdae6f534f020257
Block height: 4519399
Previous block intersection:

  - slot: 5071600
  - id: e209bcf37895193c121de458fe2ca88d0ab4afab956a7d3eaa82eb640b937e95

-}
decode1bcd8fa7 : Test
decode1bcd8fa7 =
    test "Tx id 1bcd8fa799bd7d47f6af8d54e9ebda4756597991ca44b33dcdae6f534f020257" <|
        \_ ->
            Bytes.fromHexUnchecked "83a4031a055d4a80018282582b82d818582183581c5f0b7754ae7707405bc7dcd03fce70fa7295ebd69d069ff786d78445a0001aea3675061a000f424082582b82d818582183581cac8b848e1ef9ad778bb8d8da127daeed273827727fa0e084194bd2efa0001a0d3c27671a0049b3a40081825820ca6267b5f2b336da224e6b5efac292f0eaf45b40f7f7b931e4b5ee21e68455d800021a0002979ca1028184582002ec61ad3946d5282d054fddd635789325a46b884bb8daebb424b0081ce9d9945840ef3e425c97d8f733f9d33302495e208056c877c6a9d89a35439fe5d6bda3b6958171958416754bcf18e5ddbb68533c16715fe180e06ce270dc37bfb556347b0c5820000000000000000000000000000000000000000000000000000000000000000041a0f6"
                |> Transaction.deserialize
                |> Expect.equal
                    (Just
                        { body = txBody1bcd8fa7
                        , witnessSet = txWitnessSet1bcd8fa7
                        , isValid = True
                        , auxiliaryData = Nothing
                        }
                    )


txBody1bcd8fa7 : TransactionBody
txBody1bcd8fa7 =
    { newBody
        | fee = N.fromSafeInt 169884
        , inputs = [ { outputIndex = 0, transactionId = Bytes.fromHexUnchecked "ca6267b5f2b336da224e6b5efac292f0eaf45b40f7f7b931e4b5ee21e68455d8" } ]
        , outputs =
            [ { address = Address.Byron (Bytes.fromHexUnchecked "82d818582183581c5f0b7754ae7707405bc7dcd03fce70fa7295ebd69d069ff786d78445a0001aea367506")
              , amount = Value.onlyLovelace (N.fromSafeInt 1000000)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            , { address = Address.Byron (Bytes.fromHexUnchecked "82d818582183581cac8b848e1ef9ad778bb8d8da127daeed273827727fa0e084194bd2efa0001a0d3c2767")
              , amount = Value.onlyLovelace (N.fromSafeInt 4830116)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            ]
        , ttl = Just (bigNat [ 22891136, 1 ])
    }


txWitnessSet1bcd8fa7 : WitnessSet
txWitnessSet1bcd8fa7 =
    { newWitnessSet
        | bootstrapWitness =
            Just
                [ { attributes = Bytes.fromHexUnchecked "a0"
                  , chainCode = Bytes.fromHexUnchecked "0000000000000000000000000000000000000000000000000000000000000000"
                  , publicKey = Bytes.fromHexUnchecked "02ec61ad3946d5282d054fddd635789325a46b884bb8daebb424b0081ce9d994"
                  , signature = Bytes.fromHexUnchecked "ef3e425c97d8f733f9d33302495e208056c877c6a9d89a35439fe5d6bda3b6958171958416754bcf18e5ddbb68533c16715fe180e06ce270dc37bfb556347b0c"
                  }
                ]
    }


{-| Next Shelley failure.

Tx id: c220e20cc480df9ce7cd871df491d7390c6a004b9252cf20f45fc3c968535b4a
Block height: 4558697
Previous block intersection:

  - slot: 5860466
  - id: 382a42c0b62d733f35abac4e003b30cd148f62f75e0af235f8be7fd0812018d4

-}
decodec220e20c : Test
decodec220e20c =
    test "Tx id c220e20cc480df9ce7cd871df491d7390c6a004b9252cf20f45fc3c968535b4a" <|
        \_ ->
            Bytes.fromHexUnchecked "83a500818258205b06f6ea129a404d5bc610880be35376625a8f7f11773bf79db1889eb3bb87eb00018182581d61c96001f4a4e10567ac18be3c47663a00a858f51c56779e94993d30ef1a0095e957021a0002ad29031a005991b0075820c2d2b42fbacf30eeddab1447f525297eec0ab134f8cddd2025a075c69d57e4bca100818258204251d746864839409bc2bb6dfbb680c503c3a2613dba0ac55c6791eaebd9ad84584057e649e46b1711bfd45cb2ae0e4ecb8c863e5c261545f0ec96fe6ee3fc8dd5b36106fd13d21e643c34e04c58b18759afaca58f990060b4342dd7369bd11b1d06a101a368766f7465725f69646f3132336162633030306362613332316662616c6c6f74a36669737375653163796573666973737565336e416c7068612043656e746175726966697373756532626e6f67766f74655f696469616263313233303030"
                |> Transaction.deserialize
                |> Expect.equal
                    (Just
                        { body = txBodyc220e20c
                        , witnessSet = txWitnessSetc220e20c
                        , isValid = True
                        , auxiliaryData = Just txAuxiliaryDatac220e20c
                        }
                    )


txAuxiliaryDatac220e20c : AuxiliaryData
txAuxiliaryDatac220e20c =
    { labels =
        [ ( N.fromSafeInt 1
          , Metadatum.Map
                [ ( Metadatum.String "voter_id", Metadatum.String "123abc000cba321" )
                , ( Metadatum.String "ballot", Metadatum.Map [ ( Metadatum.String "issue1", Metadatum.String "yes" ), ( Metadatum.String "issue3", Metadatum.String "Alpha Centauri" ), ( Metadatum.String "issue2", Metadatum.String "no" ) ] )
                , ( Metadatum.String "vote_id", Metadatum.String "abc123000" )
                ]
          )
        ]
    , nativeScripts = []
    , plutusV1Scripts = []
    , plutusV2Scripts = []
    , plutusV3Scripts = []
    }


txBodyc220e20c : TransactionBody
txBodyc220e20c =
    { newBody
        | auxiliaryDataHash = Just (Bytes.fromHexUnchecked "c2d2b42fbacf30eeddab1447f525297eec0ab134f8cddd2025a075c69d57e4bc")
        , fee = N.fromSafeInt 175401
        , inputs = [ { outputIndex = 0, transactionId = Bytes.fromHexUnchecked "5b06f6ea129a404d5bc610880be35376625a8f7f11773bf79db1889eb3bb87eb" } ]
        , outputs =
            [ { address = Address.Shelley { networkId = Mainnet, paymentCredential = Address.VKeyHash (Bytes.fromHexUnchecked "c96001f4a4e10567ac18be3c47663a00a858f51c56779e94993d30ef"), stakeCredential = Nothing }
              , amount = Value.onlyLovelace (N.fromSafeInt 9824599)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            ]
        , ttl = Just (N.fromSafeInt 5870000)
    }


txWitnessSetc220e20c : WitnessSet
txWitnessSetc220e20c =
    { newWitnessSet
        | vkeywitness =
            Just
                [ { signature = Bytes.fromHexUnchecked "57e649e46b1711bfd45cb2ae0e4ecb8c863e5c261545f0ec96fe6ee3fc8dd5b36106fd13d21e643c34e04c58b18759afaca58f990060b4342dd7369bd11b1d06"
                  , vkey = Bytes.fromHexUnchecked "4251d746864839409bc2bb6dfbb680c503c3a2613dba0ac55c6791eaebd9ad84"
                  }
                ]
    }


{-| First Allegra failure.

Tx id: 254685a838cde38aaf2ba64b41c676024d5de9378535a7022df3db651ccc4b5d
Block height: 5086528
Previous block intersection:

  - slot: 16588889
  - id: f77e011ef1ad383dced90e32e18c28c0fdc6e363066ab5adf191044fdbcf668d

-}
decode254685a8 : Test
decode254685a8 =
    test "Tx id 254685a838cde38aaf2ba64b41c676024d5de9378535a7022df3db651ccc4b5d" <|
        \_ ->
            Bytes.fromHexUnchecked "83a50081825820d241c8e10ff0d9ac04cfed2a6d6d6f80d0250bc2a47489df7b436f0d9f769b4d010182825839013ffb2a4330a1742c6243aecdb3619768f10efd50676cbbe90df6ee6f001b294a5d8925e7fc4ac1ee89235925c722c1eadbfa74f90c1f4b0d1a000f4240825839019a9cb3efe697ef46401222bc952ce1c7b63cee34c48951ce5e22f888001b294a5d8925e7fc4ac1ee89235925c722c1eadbfa74f90c1f4b0d1a035cf832021a0002b149031a00fd3c9807582027c2bbc74ce52de561afc65f4db18eaff9cf05938e570c2370bca19bf8b33c68a1008182582071b6f4525dea5a5c295cb78c8674e93202995f75157b38fc149338b5a8b35f195840b6fff317173f5aa8eff05ae47df56b89360fe547a3a900e1db849f52e83bd0743b18da254684256d29c016ed2e3792149bdbf6c3375b7b9ff4cf12507e662e0582a219ef64a2015820ad5c72ef06f056a8c3f49029e26bd93bf02aa5fde2c316d32057e1e16eb38c77025820b69b1be933e9463b1a1553c2829b6f093e7e4c5c6b6afb1ce5bd333262ef804c19ef65a10158408d6c17e1b41ecf192fe70bac45d452c33c6d4598c2f60f50fe4f9de35984ffdc9fda74ba8feaafe96c0cd2918f321e86ccb318ab46d7731192e5caf684c2930080"
                |> Transaction.deserialize
                |> Expect.equal
                    (Just
                        { body = txBody254685a8
                        , witnessSet = txWitnessSet254685a8
                        , isValid = True
                        , auxiliaryData = Just txAuxiliaryData254685a8
                        }
                    )


txAuxiliaryData254685a8 : AuxiliaryData
txAuxiliaryData254685a8 =
    { labels =
        [ ( N.fromSafeInt 61284
          , Metadatum.Map
                [ ( Metadatum.Int (Integer.fromNatural (N.fromSafeInt 1)), Metadatum.Bytes (Bytes.fromHexUnchecked "ad5c72ef06f056a8c3f49029e26bd93bf02aa5fde2c316d32057e1e16eb38c77") )
                , ( Metadatum.Int (Integer.fromNatural (N.fromSafeInt 2)), Metadatum.Bytes (Bytes.fromHexUnchecked "b69b1be933e9463b1a1553c2829b6f093e7e4c5c6b6afb1ce5bd333262ef804c") )
                ]
          )
        , ( N.fromSafeInt 61285
          , Metadatum.Map
                [ ( Metadatum.Int (Integer.fromNatural (N.fromSafeInt 1)), Metadatum.Bytes (Bytes.fromHexUnchecked "8d6c17e1b41ecf192fe70bac45d452c33c6d4598c2f60f50fe4f9de35984ffdc9fda74ba8feaafe96c0cd2918f321e86ccb318ab46d7731192e5caf684c29300") )
                ]
          )
        ]
    , nativeScripts = []
    , plutusV1Scripts = []
    , plutusV2Scripts = []
    , plutusV3Scripts = []
    }


txBody254685a8 : TransactionBody
txBody254685a8 =
    { newBody
        | auxiliaryDataHash = Just (Bytes.fromHexUnchecked "27c2bbc74ce52de561afc65f4db18eaff9cf05938e570c2370bca19bf8b33c68")
        , fee = N.fromSafeInt 176457
        , inputs = [ { outputIndex = 1, transactionId = Bytes.fromHexUnchecked "d241c8e10ff0d9ac04cfed2a6d6d6f80d0250bc2a47489df7b436f0d9f769b4d" } ]
        , outputs =
            [ { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromHexUnchecked "3ffb2a4330a1742c6243aecdb3619768f10efd50676cbbe90df6ee6f")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromHexUnchecked "001b294a5d8925e7fc4ac1ee89235925c722c1eadbfa74f90c1f4b0d")))
                        }
              , amount = Value.onlyLovelace (N.fromSafeInt 1000000)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            , { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromHexUnchecked "9a9cb3efe697ef46401222bc952ce1c7b63cee34c48951ce5e22f888")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromHexUnchecked "001b294a5d8925e7fc4ac1ee89235925c722c1eadbfa74f90c1f4b0d")))
                        }
              , amount = Value.onlyLovelace (N.fromSafeInt 56424498)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            ]
        , ttl = Just (N.fromSafeInt 16596120)
    }


txWitnessSet254685a8 : WitnessSet
txWitnessSet254685a8 =
    { newWitnessSet
        | vkeywitness =
            Just
                [ { signature = Bytes.fromHexUnchecked "b6fff317173f5aa8eff05ae47df56b89360fe547a3a900e1db849f52e83bd0743b18da254684256d29c016ed2e3792149bdbf6c3375b7b9ff4cf12507e662e05"
                  , vkey = Bytes.fromHexUnchecked "71b6f4525dea5a5c295cb78c8674e93202995f75157b38fc149338b5a8b35f19"
                  }
                ]
    }


{-| Next Allegra failure.

Tx id: 4a3f86762383f1d228542d383ae7ac89cf75cf7ff84dec8148558ea92b0b92d0
Block height: 5135340
Previous block intersection:

  - slot: 17584867
  - id: ca25b60d411b7be9d0ce53064043d39681848bc93aa777a2ea60a7910330e93e

-}
decode4a3f8676 : Test
decode4a3f8676 =
    test "Tx id 4a3f86762383f1d228542d383ae7ac89cf75cf7ff84dec8148558ea92b0b92d0" <|
        \_ ->
            Bytes.fromHexUnchecked "83a40081825820e7db1f809fcc21d3dd108ced6218bf0f0cbb6a0f679f848ff1790b68d3a35872000181825839010c57a4aa08aaa7c42b45e4e9490151e2665dbb7d374e795ad5be5e4960562a0d213c675c2b84ee0e34eb377d4abbe82a4c256a0708baac251a0016e360021a0007a120031a010c59f8a200838258205df1be8b0071123c982a94c19c0c06485dbe9271e4381e8cf4fc2ed554ac133f5840c271a9d652ae95e6a8a5117c5026369235182da7e4fe040b02465089e5e2caf05063cf8b61601e6f6074c03f700bacaadcd62483c48d66a5f8d418a7c27c4b01825820403171966fadb1ce9b26852cb74018a04bc031a4aee92be39702b18efd75e058584039395858906ec9ab7540e79022b25a1f3bdfece09f9e2f36254eb5abe625b72dbd8179ece4c9fc1d537afce95b67d8095d29e1f3c50de4ecc30fd67e1ba440048258206311da054c5dfa3ac53c9fc3be859bd322f0712f7e093596fa5f6de031d95acd58403d7deba60f80a03f5bf172c1699f07ecef557d9509551cbe8d2b9000e903c3e3f60bb127c9c0b4cd5df84c01791b98e10fe19209088d0b085c74702b25914a0d01818201838200581ca96da581c39549aeda81f539ac3940ac0cb53657e774ca7e68f15ed98200581cccfcb3fed004562be1354c837a4a4b9f4b1c2b6705229efeedd12d4d8200581c74fcd61aecebe36aa6b6cd4314027282fa4b41c3ce8af17d9b77d0d1f6"
                |> Transaction.deserialize
                |> Expect.equal
                    (Just
                        { body = txBody4a3f8676
                        , witnessSet = txWitnessSet4a3f8676
                        , isValid = True
                        , auxiliaryData = Nothing
                        }
                    )


txBody4a3f8676 : TransactionBody
txBody4a3f8676 =
    { newBody
        | fee = N.fromSafeInt 500000
        , inputs = [ { outputIndex = 0, transactionId = Bytes.fromHexUnchecked "e7db1f809fcc21d3dd108ced6218bf0f0cbb6a0f679f848ff1790b68d3a35872" } ]
        , outputs =
            [ { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromHexUnchecked "0c57a4aa08aaa7c42b45e4e9490151e2665dbb7d374e795ad5be5e49")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromHexUnchecked "60562a0d213c675c2b84ee0e34eb377d4abbe82a4c256a0708baac25")))
                        }
              , amount = Value.onlyLovelace (N.fromSafeInt 1500000)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            ]
        , ttl = Just (N.fromSafeInt 17586680)
    }


txWitnessSet4a3f8676 : WitnessSet
txWitnessSet4a3f8676 =
    { newWitnessSet
        | nativeScripts =
            Just
                [ ScriptAll
                    [ ScriptPubkey (Bytes.fromHexUnchecked "a96da581c39549aeda81f539ac3940ac0cb53657e774ca7e68f15ed9")
                    , ScriptPubkey (Bytes.fromHexUnchecked "ccfcb3fed004562be1354c837a4a4b9f4b1c2b6705229efeedd12d4d")
                    , ScriptPubkey (Bytes.fromHexUnchecked "74fcd61aecebe36aa6b6cd4314027282fa4b41c3ce8af17d9b77d0d1")
                    ]
                ]
        , vkeywitness =
            Just
                [ { signature = Bytes.fromHexUnchecked "c271a9d652ae95e6a8a5117c5026369235182da7e4fe040b02465089e5e2caf05063cf8b61601e6f6074c03f700bacaadcd62483c48d66a5f8d418a7c27c4b01"
                  , vkey = Bytes.fromHexUnchecked "5df1be8b0071123c982a94c19c0c06485dbe9271e4381e8cf4fc2ed554ac133f"
                  }
                , { signature = Bytes.fromHexUnchecked "39395858906ec9ab7540e79022b25a1f3bdfece09f9e2f36254eb5abe625b72dbd8179ece4c9fc1d537afce95b67d8095d29e1f3c50de4ecc30fd67e1ba44004"
                  , vkey = Bytes.fromHexUnchecked "403171966fadb1ce9b26852cb74018a04bc031a4aee92be39702b18efd75e058"
                  }
                , { signature = Bytes.fromHexUnchecked "3d7deba60f80a03f5bf172c1699f07ecef557d9509551cbe8d2b9000e903c3e3f60bb127c9c0b4cd5df84c01791b98e10fe19209088d0b085c74702b25914a0d"
                  , vkey = Bytes.fromHexUnchecked "6311da054c5dfa3ac53c9fc3be859bd322f0712f7e093596fa5f6de031d95acd"
                  }
                ]
    }


{-| First Mary failure.

Tx id: e252be4c7e40d35919f741c9649ff207c3e49d53bb819e5c1cb458055fd363ed
Block height: 5406748
Previous block intersection:

  - slot: 23068800
  - id: a650a3f398ba4a9427ec8c293e9f7156d81fd2f7ca849014d8d2c1156c359b3a

-}
decodee252be4c : Test
decodee252be4c =
    test "Tx id e252be4c7e40d35919f741c9649ff207c3e49d53bb819e5c1cb458055fd363ed" <|
        \_ ->
            Bytes.fromHexUnchecked "83a50081825820d9a8ae2194e2e25e8079a04a4694e2679464a4f51512863a0008a35a85762ff00001818258390187be75696aea59b41b36415d0b7c8b7a0c21fe3dd5a63939acfbc3a0285f7ef46037e7fd61f04d19b4cabbc7ff7c2a7f8279a11f50422a95821a003d0900a1581c00000002df633853f6a47465c9496721d2d5b1291b8398016c0e87aea1476e7574636f696e01021a000f4240031a017290bf09a1581c00000002df633853f6a47465c9496721d2d5b1291b8398016c0e87aea1476e7574636f696e01a20082825820f89a1eb82e913f8622f7891cd7c41bae2cd0f1279f290ca4825b9c58e9f407ff584061e4e5821b852ecffaadc664bbfdd905b54520c11e76f04e684b6702a3204797a85b4d3f42cb66669a66623972d102da6f2a78f55a0a5862b0c090a1675a880782582008c2ca6654c9e43b41b0b1560ee6a7bb4997629c2646575982934a51ecd71900584034cb5fb5b8f9f9fd339611a2d82ca35468863cd205684534e104841dca0632f8b3a350e91c6696576252128f338f8636925dd66e398a9e2df25614a89c2c5d0d018182018282051a017290bf8200581ce97316c52c85eab276fd40feacf78bc5eff74e225e744567140070c3f6"
                |> Transaction.deserialize
                |> Expect.equal
                    (Just
                        { body = txBodye252be4c
                        , witnessSet = txWitnessSete252be4c
                        , isValid = True
                        , auxiliaryData = Nothing
                        }
                    )


txBodye252be4c : TransactionBody
txBodye252be4c =
    { newBody
        | auxiliaryDataHash = Nothing
        , fee = N.fromSafeInt 1000000
        , inputs = [ { outputIndex = 0, transactionId = Bytes.fromHexUnchecked "d9a8ae2194e2e25e8079a04a4694e2679464a4f51512863a0008a35a85762ff0" } ]
        , outputs =
            [ { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromHexUnchecked "87be75696aea59b41b36415d0b7c8b7a0c21fe3dd5a63939acfbc3a0")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromHexUnchecked "285f7ef46037e7fd61f04d19b4cabbc7ff7c2a7f8279a11f50422a95")))
                        }
              , amount =
                    { assets = bytesMap (Dict.fromList [ ( "00000002df633853f6a47465c9496721d2d5b1291b8398016c0e87ae", bytesMap (Dict.fromList [ ( "6e7574636f696e", N.fromSafeInt 1 ) ]) ) ])
                    , lovelace = N.fromSafeInt 4000000
                    }
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            ]
        , mint = bytesMap (Dict.fromList [ ( "00000002df633853f6a47465c9496721d2d5b1291b8398016c0e87ae", bytesMap (Dict.fromList [ ( "6e7574636f696e", Integer.fromNatural (N.fromSafeInt 1) ) ]) ) ])
        , ttl = Just (N.fromSafeInt 24285375)
    }


txWitnessSete252be4c : WitnessSet
txWitnessSete252be4c =
    { newWitnessSet
        | bootstrapWitness = Nothing
        , nativeScripts = Just [ ScriptAll [ InvalidHereafter (N.fromSafeInt 24285375), ScriptPubkey (Bytes.fromHexUnchecked "e97316c52c85eab276fd40feacf78bc5eff74e225e744567140070c3") ] ]
        , vkeywitness =
            Just
                [ { signature = Bytes.fromHexUnchecked "61e4e5821b852ecffaadc664bbfdd905b54520c11e76f04e684b6702a3204797a85b4d3f42cb66669a66623972d102da6f2a78f55a0a5862b0c090a1675a8807"
                  , vkey = Bytes.fromHexUnchecked "f89a1eb82e913f8622f7891cd7c41bae2cd0f1279f290ca4825b9c58e9f407ff"
                  }
                , { signature = Bytes.fromHexUnchecked "34cb5fb5b8f9f9fd339611a2d82ca35468863cd205684534e104841dca0632f8b3a350e91c6696576252128f338f8636925dd66e398a9e2df25614a89c2c5d0d"
                  , vkey = Bytes.fromHexUnchecked "08c2ca6654c9e43b41b0b1560ee6a7bb4997629c2646575982934a51ecd71900"
                  }
                ]
    }


{-| Next Mary failure.

Tx id: fc863a441b55acceebb7d25c81ff7259e4fc9b92fbdf6d594118fb8f1110a78c
Block height: 5582677
Previous block intersection:

  - slot: 26681683
  - id: b86f5d4306d75c93a17a50141ea27cdf13a4ba70d2a1a28354c9c95e50f54fd3

-}
decodefc863a44 : Test
decodefc863a44 =
    test "Tx id fc863a441b55acceebb7d25c81ff7259e4fc9b92fbdf6d594118fb8f1110a78c" <|
        \_ ->
            Bytes.fromHexUnchecked "83a50081825820bf30608a974d09c56dd62ca10199ec11746ea2d90dbd83649d4f37c629b1ba840001818258390117d237fb8f952c995cd28f73c555adc2307322d819b7f565196ce754348144bff68f23c1386b85dea0f8425ca574b1a11e188ffaba67537c1a0048f96f021a000351d1031a019732f30682a7581c162f94554ac8c225383a2248c245659eda870eaa82d0ef25fc7dcd82a10d8100581c2075a095b3c844a29c24317a94a643ab8e22d54a3a3a72a420260af6a10d8100581c268cfc0b89e910ead22e0ade91493d8212f53f3e2164b2e4bef0819ba10d8100581c60baee25cbc90047e83fd01e1e57dc0b06d3d0cb150d0ab40bbfead1a10d8100581cad5463153dc3d24b9ff133e46136028bdc1edbb897f5a7cf1b37950ca10d8100581cb9547b8a57656539a8d9bc42c008e38d9c8bd9c8adbb1e73ad529497a10d8100581cf7b341c14cd58fca4195a9b278cce1ef402dc0e06deb77e543cd1757a10d8100190103a1008882582061261a95b7613ee6bf2067dad77b70349729b0c50d57bc1cf30de0db4a1e73a858407d72721e7504e12d50204f7d9e9d9fe60d9c6a4fd18ad629604729df4f7f3867199b62885623fab68a02863e7877955ca4a56c867157a559722b7b350b668a0b8258209180d818e69cd997e34663c418a648c076f2e19cd4194e486e159d8580bc6cda5840af668e57c98f0c3d9b47c66eb9271213c39b4ea1b4d543b0892f03985edcef4216d1f98f7b731eedc260a2154124b5cab015bfeaf694d58966d124ad2ff60f0382582089c29f8c4af27b7accbe589747820134ebbaa1caf3ce949270a3d0c7dcfd541b58401ad69342385ba6c3bef937a79456d7280c0d539128072db15db120b1579c46ba95d18c1fa073d7dbffb4d975b1e02ebb7372936940cff0a96fce950616d2f504825820f14f712dc600d793052d4842d50cefa4e65884ea6cf83707079eb8ce302efc855840638f7410929e7eab565b1451effdfbeea2a8839f7cfcc4c4483c4931d489547a2e94b73e4b15f8494de7f42ea31e573c459a9a7e5269af17b0978e70567de80e8258208b53207629f9a30e4b2015044f337c01735abe67243c19470c9dae8c7b73279858400c4ed03254c33a19256b7a3859079a9b75215cad83871a9b74eb51d8bcab52911c37ea5c43bdd212d006d1e6670220ff1d03714addf94f490e482edacbb08f068258205fddeedade2714d6db2f9e1104743d2d8d818ecddc306e176108db14caadd4415840bf48f5dd577b5cb920bfe60e13c8b1b889366c23e2f2e28d51814ed23def3a0ff4a1964f806829d40180d83b5230728409c1f18ddb5a61c44e614b823bd43f01825820cbc6b506e94fbefe442eecee376f3b3ebaf89415ef5cd2efb666e06ddae48393584089bff8f81a20b22f2c3f8a2288b15f1798b51f3363e0437a46c0a2e4e283b7c1018eba0b2b192d6d522ac8df2f2e95b4c8941b387cda89857ab0ae77db14780c825820e8c03a03c0b2ddbea4195caf39f41e669f7d251ecf221fbb2f275c0a5d7e05d158402643ac53dd4da4f6e80fb192b2bf7d1dd9a333bbacea8f07531ba450dd8fb93e481589d370a6ef33a97e03b2f5816e4b2c6a8abf606a859108ba6f416e530d07f6"
                |> Transaction.deserialize
                |> Expect.equal
                    (Just
                        { body = txBodyfc863a44
                        , witnessSet = txWitnessSetfc863a44
                        , isValid = True
                        , auxiliaryData = Nothing
                        }
                    )


txBodyfc863a44 : TransactionBody
txBodyfc863a44 =
    { newBody
        | fee = N.fromSafeInt 217553
        , inputs = [ { outputIndex = 0, transactionId = Bytes.fromHexUnchecked "bf30608a974d09c56dd62ca10199ec11746ea2d90dbd83649d4f37c629b1ba84" } ]
        , outputs =
            [ { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromHexUnchecked "17d237fb8f952c995cd28f73c555adc2307322d819b7f565196ce754")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromHexUnchecked "348144bff68f23c1386b85dea0f8425ca574b1a11e188ffaba67537c")))
                        }
              , amount = Value.onlyLovelace (N.fromSafeInt 4782447)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            ]
        , ttl = Just (N.fromSafeInt 26686195)
        , update =
            Just
                { epoch = N.fromSafeInt 259
                , proposedProtocolParameterUpdates =
                    bytesMap
                        (Dict.fromList
                            [ ( "162f94554ac8c225383a2248c245659eda870eaa82d0ef25fc7dcd82", { noParamUpdate | extraEntropy = Just Just0 } )
                            , ( "2075a095b3c844a29c24317a94a643ab8e22d54a3a3a72a420260af6", { noParamUpdate | extraEntropy = Just Just0 } )
                            , ( "268cfc0b89e910ead22e0ade91493d8212f53f3e2164b2e4bef0819b", { noParamUpdate | extraEntropy = Just Just0 } )
                            , ( "60baee25cbc90047e83fd01e1e57dc0b06d3d0cb150d0ab40bbfead1", { noParamUpdate | extraEntropy = Just Just0 } )
                            , ( "ad5463153dc3d24b9ff133e46136028bdc1edbb897f5a7cf1b37950c", { noParamUpdate | extraEntropy = Just Just0 } )
                            , ( "b9547b8a57656539a8d9bc42c008e38d9c8bd9c8adbb1e73ad529497", { noParamUpdate | extraEntropy = Just Just0 } )
                            , ( "f7b341c14cd58fca4195a9b278cce1ef402dc0e06deb77e543cd1757", { noParamUpdate | extraEntropy = Just Just0 } )
                            ]
                        )
                }
    }


txWitnessSetfc863a44 : WitnessSet
txWitnessSetfc863a44 =
    { newWitnessSet
        | vkeywitness =
            Just
                [ { signature = Bytes.fromHexUnchecked "7d72721e7504e12d50204f7d9e9d9fe60d9c6a4fd18ad629604729df4f7f3867199b62885623fab68a02863e7877955ca4a56c867157a559722b7b350b668a0b"
                  , vkey = Bytes.fromHexUnchecked "61261a95b7613ee6bf2067dad77b70349729b0c50d57bc1cf30de0db4a1e73a8"
                  }
                , { signature = Bytes.fromHexUnchecked "af668e57c98f0c3d9b47c66eb9271213c39b4ea1b4d543b0892f03985edcef4216d1f98f7b731eedc260a2154124b5cab015bfeaf694d58966d124ad2ff60f03"
                  , vkey = Bytes.fromHexUnchecked "9180d818e69cd997e34663c418a648c076f2e19cd4194e486e159d8580bc6cda"
                  }
                , { signature = Bytes.fromHexUnchecked "1ad69342385ba6c3bef937a79456d7280c0d539128072db15db120b1579c46ba95d18c1fa073d7dbffb4d975b1e02ebb7372936940cff0a96fce950616d2f504"
                  , vkey = Bytes.fromHexUnchecked "89c29f8c4af27b7accbe589747820134ebbaa1caf3ce949270a3d0c7dcfd541b"
                  }
                , { signature = Bytes.fromHexUnchecked "638f7410929e7eab565b1451effdfbeea2a8839f7cfcc4c4483c4931d489547a2e94b73e4b15f8494de7f42ea31e573c459a9a7e5269af17b0978e70567de80e"
                  , vkey = Bytes.fromHexUnchecked "f14f712dc600d793052d4842d50cefa4e65884ea6cf83707079eb8ce302efc85"
                  }
                , { signature = Bytes.fromHexUnchecked "0c4ed03254c33a19256b7a3859079a9b75215cad83871a9b74eb51d8bcab52911c37ea5c43bdd212d006d1e6670220ff1d03714addf94f490e482edacbb08f06"
                  , vkey = Bytes.fromHexUnchecked "8b53207629f9a30e4b2015044f337c01735abe67243c19470c9dae8c7b732798"
                  }
                , { signature = Bytes.fromHexUnchecked "bf48f5dd577b5cb920bfe60e13c8b1b889366c23e2f2e28d51814ed23def3a0ff4a1964f806829d40180d83b5230728409c1f18ddb5a61c44e614b823bd43f01"
                  , vkey = Bytes.fromHexUnchecked "5fddeedade2714d6db2f9e1104743d2d8d818ecddc306e176108db14caadd441"
                  }
                , { signature = Bytes.fromHexUnchecked "89bff8f81a20b22f2c3f8a2288b15f1798b51f3363e0437a46c0a2e4e283b7c1018eba0b2b192d6d522ac8df2f2e95b4c8941b387cda89857ab0ae77db14780c"
                  , vkey = Bytes.fromHexUnchecked "cbc6b506e94fbefe442eecee376f3b3ebaf89415ef5cd2efb666e06ddae48393"
                  }
                , { signature = Bytes.fromHexUnchecked "2643ac53dd4da4f6e80fb192b2bf7d1dd9a333bbacea8f07531ba450dd8fb93e481589d370a6ef33a97e03b2f5816e4b2c6a8abf606a859108ba6f416e530d07"
                  , vkey = Bytes.fromHexUnchecked "e8c03a03c0b2ddbea4195caf39f41e669f7d251ecf221fbb2f275c0a5d7e05d1"
                  }
                ]
    }



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



-- decodeAnyAndFailTest : Bytes a -> Expectation
-- decodeAnyAndFailTest bytes =
--     Cbor.Decode.decode Cbor.Decode.any (Bytes.toBytes bytes)
--         |> Expect.equal Nothing
