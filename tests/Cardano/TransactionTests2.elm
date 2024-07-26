module Cardano.TransactionTests2 exposing (suite)

import Bytes.Comparable as Bytes
import Bytes.Map exposing (BytesMap)
import Cardano.Address as Address exposing (Credential(..), NetworkId(..))
import Cardano.Data as Data exposing (Data(..))
import Cardano.Redeemer exposing (RedeemerTag(..))
import Cardano.Script exposing (NativeScript(..))
import Cardano.Transaction as Transaction exposing (Nonce(..), TransactionBody, WitnessSet, noParamUpdate)
import Cardano.Transaction.AuxiliaryData exposing (AuxiliaryData)
import Cardano.Transaction.AuxiliaryData.Metadatum as Metadatum
import Cardano.Transaction.Builder as Tx
import Cardano.Utxo as Utxo exposing (DatumOption(..))
import Cardano.Value as Value
import Cbor.Decode as D
import Dict exposing (Dict)
import Expect
import Integer
import Natural as N exposing (Natural)
import Test exposing (Test, describe, test)
import Tests exposing (expectBytes)


suite : Test
suite =
    describe "Cardano.Transaction (follow up)"
        [ describe "deserialize (follow up)"
            -- Alonzo transactions
            [ decode8a8f8dfe
            , decodebf095309
            , decode5fb50416

            -- Babbage transactions
            , decode9c91bdbb
            ]
        , decodeInputs
        , decodeOutputs
        ]


{-| First Alonzo failure.

Tx id: 8a8f8dfe180112e5a5080078a14528cf8f2e42cc2aeecb5a5c9912efe870991c
Block height: 6236063
Previous block intersection:

  - slot: 39917142
  - id: 1464f8f784e4c9237a08d2068cc37a09598ee69a51748bcf70ab5cf39aa2e106

-}
decode8a8f8dfe : Test
decode8a8f8dfe =
    test "Tx id 8a8f8dfe180112e5a5080078a14528cf8f2e42cc2aeecb5a5c9912efe870991c" <|
        \_ ->
            Bytes.fromStringUnchecked "84a6008482582041b2bf9badec879f09dc9154c582da12f0c5a7970cd3f922ed34baf83e19e2e10082582041b2bf9badec879f09dc9154c582da12f0c5a7970cd3f922ed34baf83e19e2e10182582041b2bf9badec879f09dc9154c582da12f0c5a7970cd3f922ed34baf83e19e2e10282582041b2bf9badec879f09dc9154c582da12f0c5a7970cd3f922ed34baf83e19e2e1030d8182582041b2bf9badec879f09dc9154c582da12f0c5a7970cd3f922ed34baf83e19e2e103018482581d6108f30ea42a1e844e389e7e0483e69376dc8babd01ad91ab536c8b8891a02a0d28883581d71e265b741cd9ac2c4d695402f7bb4cb49cbe0d9e33eec9a26dbfa9e59821a004c4b40a1581cfb353334ac071f79f6d938e0972640a6b6650815124e9d63fbc0b5e8a1444641524d01582060cc87c3d3c639bb292b00670f83a08cb6b4abd20e018b137377b34ccf2ccf2982581d6108f30ea42a1e844e389e7e0483e69376dc8babd01ad91ab536c8b889821a02c2730da1581cfb353334ac071f79f6d938e0972640a6b6650815124e9d63fbc0b5e8a144434f524e0182581d6108f30ea42a1e844e389e7e0483e69376dc8babd01ad91ab536c8b8891a001e8480021a00031c050e8007582036663d429bded43331a968fcaa3a0aba03d6d83474176b8c85a019b0b408ff8da100818258202a61b85e796ce56b7cb960696f27caf8867e3a209fb5243e935ac3bf99aa0c0c58406ee6dfafc7fdcd553bf0c11cc93d165c77a93a265af250eafb1dca2b044ae0b62d6eb4969e7946c438be5f73b0d9a25ad83d074c8d9cd6f4c80ace7b7c62ab0df5d90103a100a11902a2a1636d736783783157656c636f6d6520746f207468652050494759204f7261636c6520616e6420746f2074686520416c6f6e7a6f206572612160781c68747470733a2f2f6f7261636c652e70696779746f6b656e2e636f6d"
                |> Transaction.deserialize
                |> Expect.equal
                    (Just
                        { body = body8a8f8dfe
                        , witnessSet = witnessSet8a8f8dfe
                        , isValid = True
                        , auxiliaryData = Just auxiliaryData8a8f8dfe
                        }
                    )


auxiliaryData8a8f8dfe : AuxiliaryData
auxiliaryData8a8f8dfe =
    { labels = [ ( N.fromSafeInt 674, Metadatum.Map [ ( Metadatum.String "msg", Metadatum.List [ Metadatum.String "Welcome to the PIGY Oracle and to the Alonzo era!", Metadatum.String "", Metadatum.String "https://oracle.pigytoken.com" ] ) ] ) ]
    , nativeScripts = []
    , plutusV1Scripts = []
    , plutusV2Scripts = []
    }


body8a8f8dfe : TransactionBody
body8a8f8dfe =
    { newTxBody
        | auxiliaryDataHash = Just (Bytes.fromStringUnchecked "36663d429bded43331a968fcaa3a0aba03d6d83474176b8c85a019b0b408ff8d")
        , fee = Just (N.fromSafeInt 203781)
        , inputs =
            [ { outputIndex = 0, transactionId = Bytes.fromStringUnchecked "41b2bf9badec879f09dc9154c582da12f0c5a7970cd3f922ed34baf83e19e2e1" }
            , { outputIndex = 1, transactionId = Bytes.fromStringUnchecked "41b2bf9badec879f09dc9154c582da12f0c5a7970cd3f922ed34baf83e19e2e1" }
            , { outputIndex = 2, transactionId = Bytes.fromStringUnchecked "41b2bf9badec879f09dc9154c582da12f0c5a7970cd3f922ed34baf83e19e2e1" }
            , { outputIndex = 3, transactionId = Bytes.fromStringUnchecked "41b2bf9badec879f09dc9154c582da12f0c5a7970cd3f922ed34baf83e19e2e1" }
            ]
        , collateral = [ { outputIndex = 3, transactionId = Bytes.fromStringUnchecked "41b2bf9badec879f09dc9154c582da12f0c5a7970cd3f922ed34baf83e19e2e1" } ]
        , outputs =
            [ { address = Address.Shelley { networkId = Mainnet, paymentCredential = Address.VKeyHash (Bytes.fromStringUnchecked "08f30ea42a1e844e389e7e0483e69376dc8babd01ad91ab536c8b889"), stakeCredential = Nothing }
              , amount = Value.onlyLovelace (N.fromSafeInt 44094088)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            , { address = Address.Shelley { networkId = Mainnet, paymentCredential = ScriptHash (Bytes.fromStringUnchecked "e265b741cd9ac2c4d695402f7bb4cb49cbe0d9e33eec9a26dbfa9e59"), stakeCredential = Nothing }
              , amount = { assets = bytesMap (Dict.fromList [ ( "fb353334ac071f79f6d938e0972640a6b6650815124e9d63fbc0b5e8", bytesMap (Dict.fromList [ ( "4641524d", N.fromSafeInt 1 ) ]) ) ]), lovelace = N.fromSafeInt 5000000 }
              , datumOption = Just (DatumHash (Bytes.fromStringUnchecked "60cc87c3d3c639bb292b00670f83a08cb6b4abd20e018b137377b34ccf2ccf29"))
              , referenceScript = Nothing
              }
            , { address = Address.Shelley { networkId = Mainnet, paymentCredential = Address.VKeyHash (Bytes.fromStringUnchecked "08f30ea42a1e844e389e7e0483e69376dc8babd01ad91ab536c8b889"), stakeCredential = Nothing }
              , amount = { assets = bytesMap (Dict.fromList [ ( "fb353334ac071f79f6d938e0972640a6b6650815124e9d63fbc0b5e8", bytesMap (Dict.fromList [ ( "434f524e", N.fromSafeInt 1 ) ]) ) ]), lovelace = N.fromSafeInt 46297869 }
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            , { address = Address.Shelley { networkId = Mainnet, paymentCredential = Address.VKeyHash (Bytes.fromStringUnchecked "08f30ea42a1e844e389e7e0483e69376dc8babd01ad91ab536c8b889"), stakeCredential = Nothing }
              , amount = Value.onlyLovelace (N.fromSafeInt 2000000)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            ]
    }


witnessSet8a8f8dfe : WitnessSet
witnessSet8a8f8dfe =
    { newTxWitnessSet
        | vkeywitness =
            Just
                [ { signature = Bytes.fromStringUnchecked "6ee6dfafc7fdcd553bf0c11cc93d165c77a93a265af250eafb1dca2b044ae0b62d6eb4969e7946c438be5f73b0d9a25ad83d074c8d9cd6f4c80ace7b7c62ab0d"
                  , vkey = Bytes.fromStringUnchecked "2a61b85e796ce56b7cb960696f27caf8867e3a209fb5243e935ac3bf99aa0c0c"
                  }
                ]
    }


{-| Next Alonzo failure.

Tx id: bf095309ba20174d1a5c30ea03580cbf8bfe7dd75da1203d9ed51bfd151bb327
Block height: 6555637
Previous block intersection:

  - slot: 46461491
  - id: c35b68abe2bf17cd393c2e3024f751c5343732c9ae83f1c5b3d56a713c7d986d

-}
decodebf095309 : Test
decodebf095309 =
    test "Tx id bf095309ba20174d1a5c30ea03580cbf8bfe7dd75da1203d9ed51bfd151bb327" <|
        \_ ->
            Bytes.fromStringUnchecked "84a6008182582003b02cff29a5f2dfc827e00345eaab8b29a3d740e9878aa6e5dd2b52da0763c5000d80018182581d61d80fe69ded1ff90f41e526d0332a2ff98ba8a0d85ceb8941b51784201a05633249021a00033a9d0682a7581c162f94554ac8c225383a2248c245659eda870eaa82d0ef25fc7dcd82a2021a0001200014821a00aba9501b00000002540be400581c2075a095b3c844a29c24317a94a643ab8e22d54a3a3a72a420260af6a2021a0001200014821a00aba9501b00000002540be400581c268cfc0b89e910ead22e0ade91493d8212f53f3e2164b2e4bef0819ba2021a0001200014821a00aba9501b00000002540be400581c60baee25cbc90047e83fd01e1e57dc0b06d3d0cb150d0ab40bbfead1a2021a0001200014821a00aba9501b00000002540be400581cad5463153dc3d24b9ff133e46136028bdc1edbb897f5a7cf1b37950ca2021a0001200014821a00aba9501b00000002540be400581cb9547b8a57656539a8d9bc42c008e38d9c8bd9c8adbb1e73ad529497a2021a0001200014821a00aba9501b00000002540be400581cf7b341c14cd58fca4195a9b278cce1ef402dc0e06deb77e543cd1757a2021a0001200014821a00aba9501b00000002540be4001901310e80a1008882582061261a95b7613ee6bf2067dad77b70349729b0c50d57bc1cf30de0db4a1e73a85840c65d631ecb286668eeef3537c279fb0c5c5d54bb7ab71a6d0c795f48f6093e664f9e923fd590e3373dd9e054eb622724cb107673a83ad201f503622cdcdae6038258209180d818e69cd997e34663c418a648c076f2e19cd4194e486e159d8580bc6cda584030f64d310d2cc64178fd86681013ba0960d76f5c404d434833238ed2f73a7336fb271027239f0ad0c99436852c023ee95b68d99f02b9956db5776abc8379320a82582089c29f8c4af27b7accbe589747820134ebbaa1caf3ce949270a3d0c7dcfd541b5840fe9cafe18e8fe6c31c2c88012aede78b220857d854a6f488f49aa993aef14d891548c3fd1c2c6c8edfb749c999be26f716991447ae2c0461fa6d7a8d573b0a02825820f14f712dc600d793052d4842d50cefa4e65884ea6cf83707079eb8ce302efc85584001b76c22a07514dfd86182e350e94789c04ff868fc0351a7c74fb58ca6642a658ea1e56c373fa2de85fd1a47bad40f25a62f2caee709d40368ffc14a223171018258208b53207629f9a30e4b2015044f337c01735abe67243c19470c9dae8c7b7327985840fa2b5fc33a08f863c47a67bc5f6fc649b83f0b650ec7ed9329b11db77c562262bd15d5fc97ee71efd4b2df98db9ea133a05aa7f04955d4b21a0c086acc0479018258205fddeedade2714d6db2f9e1104743d2d8d818ecddc306e176108db14caadd4415840ee63870c4c1f27866073b05b5dd418f21712667739d289997c01fbbf28ff0cb3987629588cb13a5459844066d526ce0cde89dc26af5b8108654b0514f7578f0a825820cbc6b506e94fbefe442eecee376f3b3ebaf89415ef5cd2efb666e06ddae483935840a2da7ce96693480cd49e256819b4d7a565f3a973864fdc8e4225e8a3338605aab18d6c1e7099129671fb479a8ee38aa67dde383f012741fcec02afbc82a0f903825820e8c03a03c0b2ddbea4195caf39f41e669f7d251ecf221fbb2f275c0a5d7e05d158402086186c201f064e685ca7fd50f4b313fc1f86b0e0a68cc8db2e4548cb42e2e47ffbdf07c80352484a06332f4e9a180ff8846d3cadd92d0b6717a57482127a08f5f6"
                |> Transaction.deserialize
                |> Expect.equal
                    (Just
                        { body = bodybf095309
                        , witnessSet = witnessSetbf095309
                        , isValid = True
                        , auxiliaryData = Nothing
                        }
                    )


bodybf095309 : TransactionBody
bodybf095309 =
    { newTxBody
        | fee = Just (N.fromSafeInt 211613)
        , inputs = [ { outputIndex = 0, transactionId = Bytes.fromStringUnchecked "03b02cff29a5f2dfc827e00345eaab8b29a3d740e9878aa6e5dd2b52da0763c5" } ]
        , outputs =
            [ { address = Address.Shelley { networkId = Mainnet, paymentCredential = Address.VKeyHash (Bytes.fromStringUnchecked "d80fe69ded1ff90f41e526d0332a2ff98ba8a0d85ceb8941b5178420"), stakeCredential = Nothing }
              , amount = Value.onlyLovelace (bigNat [ 23278153, 1 ])
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            ]
        , update =
            Just
                { epoch = N.fromSafeInt 305
                , proposedProtocolParameterUpdates =
                    bytesMap
                        (Dict.fromList
                            [ ( "162f94554ac8c225383a2248c245659eda870eaa82d0ef25fc7dcd82"
                              , { noParamUpdate
                                    | maxBlockBodySize = Just 73728
                                    , maxTxExUnits = Just { mem = 11250000, steps = 10000000000 }
                                }
                              )
                            , ( "2075a095b3c844a29c24317a94a643ab8e22d54a3a3a72a420260af6"
                              , { noParamUpdate
                                    | maxBlockBodySize = Just 73728
                                    , maxTxExUnits = Just { mem = 11250000, steps = 10000000000 }
                                }
                              )
                            , ( "268cfc0b89e910ead22e0ade91493d8212f53f3e2164b2e4bef0819b"
                              , { noParamUpdate
                                    | maxBlockBodySize = Just 73728
                                    , maxTxExUnits = Just { mem = 11250000, steps = 10000000000 }
                                }
                              )
                            , ( "60baee25cbc90047e83fd01e1e57dc0b06d3d0cb150d0ab40bbfead1"
                              , { noParamUpdate
                                    | maxBlockBodySize = Just 73728
                                    , maxTxExUnits = Just { mem = 11250000, steps = 10000000000 }
                                }
                              )
                            , ( "ad5463153dc3d24b9ff133e46136028bdc1edbb897f5a7cf1b37950c"
                              , { noParamUpdate
                                    | maxBlockBodySize = Just 73728
                                    , maxTxExUnits = Just { mem = 11250000, steps = 10000000000 }
                                }
                              )
                            , ( "b9547b8a57656539a8d9bc42c008e38d9c8bd9c8adbb1e73ad529497"
                              , { noParamUpdate
                                    | maxBlockBodySize = Just 73728
                                    , maxTxExUnits = Just { mem = 11250000, steps = 10000000000 }
                                }
                              )
                            , ( "f7b341c14cd58fca4195a9b278cce1ef402dc0e06deb77e543cd1757"
                              , { noParamUpdate
                                    | maxBlockBodySize = Just 73728
                                    , maxTxExUnits = Just { mem = 11250000, steps = 10000000000 }
                                }
                              )
                            ]
                        )
                }
    }


witnessSetbf095309 : WitnessSet
witnessSetbf095309 =
    { newTxWitnessSet
        | vkeywitness =
            Just
                [ { signature = Bytes.fromStringUnchecked "c65d631ecb286668eeef3537c279fb0c5c5d54bb7ab71a6d0c795f48f6093e664f9e923fd590e3373dd9e054eb622724cb107673a83ad201f503622cdcdae603", vkey = Bytes.fromStringUnchecked "61261a95b7613ee6bf2067dad77b70349729b0c50d57bc1cf30de0db4a1e73a8" }
                , { signature = Bytes.fromStringUnchecked "30f64d310d2cc64178fd86681013ba0960d76f5c404d434833238ed2f73a7336fb271027239f0ad0c99436852c023ee95b68d99f02b9956db5776abc8379320a", vkey = Bytes.fromStringUnchecked "9180d818e69cd997e34663c418a648c076f2e19cd4194e486e159d8580bc6cda" }
                , { signature = Bytes.fromStringUnchecked "fe9cafe18e8fe6c31c2c88012aede78b220857d854a6f488f49aa993aef14d891548c3fd1c2c6c8edfb749c999be26f716991447ae2c0461fa6d7a8d573b0a02", vkey = Bytes.fromStringUnchecked "89c29f8c4af27b7accbe589747820134ebbaa1caf3ce949270a3d0c7dcfd541b" }
                , { signature = Bytes.fromStringUnchecked "01b76c22a07514dfd86182e350e94789c04ff868fc0351a7c74fb58ca6642a658ea1e56c373fa2de85fd1a47bad40f25a62f2caee709d40368ffc14a22317101", vkey = Bytes.fromStringUnchecked "f14f712dc600d793052d4842d50cefa4e65884ea6cf83707079eb8ce302efc85" }
                , { signature = Bytes.fromStringUnchecked "fa2b5fc33a08f863c47a67bc5f6fc649b83f0b650ec7ed9329b11db77c562262bd15d5fc97ee71efd4b2df98db9ea133a05aa7f04955d4b21a0c086acc047901", vkey = Bytes.fromStringUnchecked "8b53207629f9a30e4b2015044f337c01735abe67243c19470c9dae8c7b732798" }
                , { signature = Bytes.fromStringUnchecked "ee63870c4c1f27866073b05b5dd418f21712667739d289997c01fbbf28ff0cb3987629588cb13a5459844066d526ce0cde89dc26af5b8108654b0514f7578f0a", vkey = Bytes.fromStringUnchecked "5fddeedade2714d6db2f9e1104743d2d8d818ecddc306e176108db14caadd441" }
                , { signature = Bytes.fromStringUnchecked "a2da7ce96693480cd49e256819b4d7a565f3a973864fdc8e4225e8a3338605aab18d6c1e7099129671fb479a8ee38aa67dde383f012741fcec02afbc82a0f903", vkey = Bytes.fromStringUnchecked "cbc6b506e94fbefe442eecee376f3b3ebaf89415ef5cd2efb666e06ddae48393" }
                , { signature = Bytes.fromStringUnchecked "2086186c201f064e685ca7fd50f4b313fc1f86b0e0a68cc8db2e4548cb42e2e47ffbdf07c80352484a06332f4e9a180ff8846d3cadd92d0b6717a57482127a08", vkey = Bytes.fromStringUnchecked "e8c03a03c0b2ddbea4195caf39f41e669f7d251ecf221fbb2f275c0a5d7e05d1" }
                ]
    }


{-| Next Alonzo failure.

Tx id: 5fb50416bbdb0b81ec30d68ecf969fb7a3ee03b07f25f01c327ae4ee9d6371d6
Block height: 6868167
Previous block intersection:

  - slot: 52997130
  - id: 783e577f59d6894635140c5241e7591989895cd2c6be71c828f5ab1d9cd3c161

-}
decode5fb50416 : Test
decode5fb50416 =
    test "Tx id 5fb50416bbdb0b81ec30d68ecf969fb7a3ee03b07f25f01c327ae4ee9d6371d6" <|
        \_ ->
            Bytes.fromStringUnchecked "84a6009f82582002d96ea8d1306db626903c429da7ad93c9b3ae96b4599c7f5b08902ab015dbff008258200e6a00baf157961c7bf14ffaa7fe7bc0699566f17ea43be6611a1b90f7c98dab0082582026b5703faf4113644cef865c2862c2292e3d7ec9329653598a81a49d3309ba9a008258202f61d933eedea3cebad0f8c3cf44695785087e7394d97dcea24b16445296afd000825820378b184e5c838655450a0395459386918171f890acfb1d06256c6540879ad5a3008258203e4fb9dc745b8618de5b23d9006b0b2d9a0a11a2e432988e8a34375f834f2fa10082582052d5415953ce9865a18dc8efcfb003907424a2002108d3b66343fc3115bbf7a90082582059734e1b95b5c991805612f9c20a208ff214f14dbe5f42d808e4e3dbeac707e200825820786a6ae640a413d7076a296e4a9508a24ca9851aae0a68097eebb873ae8c8831008258207dc09eebaeda72ef2e0459c87cb3bac1f374bda9d85cb17c2dadc66df7e439c2008258208fd8687d2da0fbbcd71f0d018eff88f67c8eb062f878450b2c3dc45192fd215c008258209df80549512d3f7f449a62a5cd29487d7871e2f67c474a788e85e0f8c76eacb4008258209eff8340742ac5448e80dddec5540b7cce3c38241e1f3eda7898e197900e4fd700825820b05f55878af5cd123fb733d0f25c106c6da037bfa7001d6681f4db25385b69e700825820b380d843a45b8a4a08203d5412286d3a94d92ab3bcf40dfaec466b00a5902cec00825820bd20e5522790e0efee6a5c819aacf51aea99adf5eb32b60142c93a3ef5a2da0c00825820be32b22489772483acfbdd7105bf899f48f1c4a3c44ae5a52b453adeb724b1ea00825820bf52851e1517a8ca096f0771962755e7c428519182fda70a497f89b4a325d07c00825820c03a711361fcd752b57ceb8339f3f50d2795a12cfae35ca9d831d550f2daf07500825820c5e57f8a142e442bdc6d10dfd20d3e50a71200481dfc8551da3bf1c0706289c300825820d8f9bef425e68b4b065842e9b218e84d484ff08a39b439c4e7d999c563712e3d00825820d9f80e7f14aae77f2eef509d479a5eb60401a2b9e2086879adadc15114f212d500825820ddef7290638c29d85eb634c9e7e621ef51f60ce60dcdbef847b68726de369cb000825820e00f908ba000140d39f2ee35786878118d1b3ddbee84d95b0529a49dfc5e899400825820e956b84b0added596e405cdd0ca52a2872b7b1913bd7c8a5625484b1caff451700825820f4c075f154d0308e41d76234b97a91c979003a93af52f92550d250c15bff635800825820f6717240cc320424a8faeed76c9976537703bbf66cce71a0e5be615d289d400e00825820f969e68448c0ba0b489c64be63a87b8dcfcac5bbcdc61242dc5ed6ab037e651300ff0d80019f825839011b80d26918fdf67926d0e2b3efe2ed925fff9bbb02422a8f57313f03a0051400c3c3e87ba84c3479ad4b96fa18dc3e9868a2bb857d7f5b831a0ded1040825839018b5e37fd8abf9363a54b84c66d23a8e74693037678ad465dd8ebebd09a5ca04fcaa6a06eb9f201d1ee47a9eb9c00df4754ea8abb4d40045e1a0cbc7f8082583901d9bdde6d639a50951deef8a30e08178be0ae1e72d0471c97973ee4e22544031100d245884c0370fc901e7f09fc8b19f251059f0f8b7f174d1a0c509e50825839016630c7dace155a5d6c2ea14fd59085e0b7a17c67ec320f23b63410726f53a523c78edcb5c8fdc857ed1d5c2a9de570976d5573ef77de1f851a0c509e508258390138bba676633b25db054b4705d576ee1cf81dbd1c15666acc4f58edf7a11222873bbe7b0250f482b5a6a361181b398b7bba985b7aa8046bda1a0d8bdd70825839010f34fe3191fc19946b657ddad79c6cba4fbf6f3c5be68eaa76ac19378690d68fd06954f5b6f69f28585394616b24cb046797f6931b878fcd1a0dcc1ac082583901b58c09f5a93b478b77210e81fba0dc3171ee75275e1a2bc72f71f766691a163b8b1fef714fd8604edc1c3e9532134883668d6044cd588aba1a015e0960825839018c663f342c7e37de4e178b6a72435a2ca3c8defe8e59893a5fccb30d6575c9183f86d85907cffa74605891c36b0d83c5c1d163a23f69c0971a0ded1040825839018c663f342c7e37de4e178b6a72435a2ca3c8defe8e59893a5fccb30d6575c9183f86d85907cffa74605891c36b0d83c5c1d163a23f69c0971a004c4b408258390127943d4923eecbf8e2e44fde676ebb1395431ef71d24368ae4bbe1d7b3097e4731e2263ab175820b5b7ce79166c1afb1e230018ca37e38091a0ded10408258390107823056b202bda49afeeba405bdb8dcae3b2a2d5c6db453dba2a1dd7ae4910d818d27a16a3d4b450bfcade07bf1d9d0992cd1148858174f1a0bebc20082583901881c18e58926f8a0a4e0443e4fd3a3f77634b0aded1888d81cdb4ed1436426dc4f2125e50ef00ac160160c460a2f75fa2c614fb759c8f4181a06e60d608258390199fca72b5ce96bd5d5cecbc85e979abfd7bc929b26cda6fafaa8a4e47358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0dca434682583901b6563dbdc671edff4dedd847410c3ce35aeb0528014e7098c8b388bc7358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0c9df95482583901903eed3376b63941b3195efbb8eaee3554928fcbc6250ad26bd159ef7358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0c339bee8258390103dbaffd44bb56eddc84aba9b4d3647bf9fb8118c3d47ee327c205f67358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0c339bee82583901f5269e8d5266b46e6ed1e446771e050ab61defc26720ee7926bf6fe87358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0d6a6dda82583901f1256b96127b2b5aa90ea7ee4038c4cc3b8c1c2e60c42a152bcbd4db7358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0da9c43f825839015963b6413f0929be98ee0f2c864956a9591f8d897dc42d90fd1c395d7358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0168614882583901a47054d66a9a63feb7b0acf64fb1dd91528e48363340975e5e4a90fc7358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0dca434682583901ee067106d922da2a6c6df9d356ab0a04fc62053270d0ebd9d88c04d07358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a005a7b2a8258390140c647979432c0d13b3586f0559156157149361b42b492322299c7cd7358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0dca4346825839015be1cb60a8535828dfdcf9d532e6052918d65c3ec11aa3a03dfebd3e7358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0bd02a2d82583901b1e340be62c7a6b054cb0f8ffc15702babbb0d46b5e679b297d609437358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a06dc8335ff021a000623b1031a0328c7080e80a1009f825820d82b68e8776af0104787d575fd5ee34fc2617d8578d569189df463b7994850f85840cb880741a25f15d6b991d12bcccc77b15c449aedc840479d3f654317f6004547825db37a16b371a5a2cc3660d1498e0501937e55ed87142e051941cf4140200b825820776dab08ce4554672163ff9ca04d96ebb4d2b3663d900447f34711157305fbf3584099b2913cfe343fecc8e6ab5a7431b3ef53f30698788c00a9f9f0f4d67adad02433fdc200a69250c523ae90375a59fbd5cce35f2bc46d1598cbd4cc7c5a94c50c825820cebbc1e8b4b624e459365a765600c8ccba296a278354b8b6e0e9c983309f717158405aa1c3f3ef4f76b9c5d15b5b066dd15c3617241e63524d834610b1af5fdf65e2df72dea61ea9b0ea5ce7d542ef00f6b87f66f8c26549c08e13a613a3c7c87706825820362dc2c18266f7d1a15bb8f3770381b5cc9a5524a51030383fec9fd97014dba55840f58455930a3a65a6ae51e251fc33ee726b8606710a088225db496c43a3bdaf5618af058b1e2b1432b6123ea0f2fabedc3dd440cdf470c3eadbdbc8b89af5d40282582044b417b72eab77e853f21cbb8a9388c7fd6760b1d266e95a86bc5f166dca5c5158403481a1e0fc77f3977e628f79c184f1a0ea745cb2761b8ad7d31d2c2b6e28d8d8b65255b3f0cecdf991688ba79524156dbc3fe33c4c9152ce41717bc759956d02825820bd154a180286999f08c4ca6da13dfd4506e0e0865067ff85ce36445ea29992605840cc7019199cd5264c2210755c2aafb7ef28c7dfddf0bdb265f915c9670dc04b63bd4cfa8b3911f4dad09bd600f83aef7957504e6b90d3f41c90c56d3cdd38000c8258209c41eafe7c334e0420c7ec2ad29fb135bc84bc555ba0add035cd53b844c7ece15840502acb3489cbe2a7c6564d87f4d90151531803494675b713904ee648f244e861fb12fde2be0ef0e1a2b0f6c2a66ad2e377089c0683c9f3b9bc46d0d0dc215c068258204fbcc7a463c6b7d9bcd7cb98a521a9ec499665e1879f4a7dbba47f5087886dd158407b8dbf1ab690150b84a486dcd8a5f59fbd7fc753078b16b45c38e8b7073d2ab0a241738eb8e9b621b83f7f313ac8088104168092ee6985160f3f83ce25e0cf068258204a6ac7bd375049286c2d85c3dcbef160cda8cc7c0336e008ef2104af3e1db5fc5840948d3f05f899d323b37a9e06207b0aa066f562d834e981dfe02eed5002b4b42eab9e5cce3fc5779ab4d9964a5c2aeb7c641a739fb885526388ee1dcbd5ae1d08825820d7f065f74dda041b5138becae296b3052950583458927d5bc497a01ba17d46da58403d547d376ce8e5e9a5ae9df522ea7049a0a529dd11564a41e0c7e63ebdbbec006d5afc1e0e5e1dea7e0f7f283429f2e3ec6d73150def929fd04de7cd2b12690882582074be23e35fe1a0d0b78e118d85ba6360c5865715eb12e9ea42ceca8fc27fa85f58401d5b559d3c95fa2c9fde08acb5829d2c607eb66a6f2cc4bde90d66750d70e5f6a7ce2b49240d354a520308af2838750352719d961088d5fce8db71eb593677008258209aa034218b8f7f45227e9f8897262cfe570a028ca9dfd1453c913b909afc080258404d0475f5c869ef826a53416182fca178dae42d1b266559397046b580ced712790c7722f4e1720846519a80bfcbaf7ec2f911dd9f2dab86229924d88fd5a29e08825820d5a8a386c18e6eabc5e9a2344208179c20400d030b4513d58f04481984f99cfe5840cccd7de81ed364bd9f74fa8390ef5f4ae8aeba57793a530a3d6087804fdc369c42cd9a8badd1a9d8396284f650bca51bde418746e55596250e9dcf155c84cf0682582004ceb23e99ffd81dfac1393de760c597901d3c09d754be2465c24aa80b22ceaf5840320cf0f065482dfa35c4ae151316df8716a52952f8126c9a2768a635a70171a1f328b5b2a1283583f019d0504645fef3e38d6505ca18f7e830b3d125320e8902825820f574ee10684cc2cca113ab9694a1b4c9af7a347f527d20bbc67b24006ad8a8915840c5bfd0408ff3a6734abd551223c548bc68a329ae44356a4d0c79af6217cb2f1b6e09cb4e799a44465b038cfa871b794e695d6e290599588b4e96d636ac6d0e0f8258203b9efb6584ca0acb7e9f1dd13061dacef808cf1416e18575f2e930afa0f25e6a58402549851005bbee6454f7324c26c77d92e2e233f0905cab50d798a247380bd7401591ccd3927fba92795963eeed705048fd44f8787bb681d1dceee8007e8cdf038258209b0795de2b2619b165fd16563fc1e2f4637bdb511efc229e6d56c1e5f8ea37835840f15b7285c92c3627a1ad1f6ed54e426972817a4929475c8a1e682aeedb2fccc954e4c314a6a9f72cf31bc231c28fa1ea572076949740c15ecfd48c3e8f2cc200825820e4d27651476d05cdd43888a7d795ae2b74eeb0c3d6fb7131bd71ac0047c4de545840446665c2a75db6daeb5a4c8c8b586fa5b358c7aee35f85367c73aebe3efe96af221e3520746e6e810fdf24eef08fb71ff359f3c94644f4d64726e3398188500d825820dbbb79767f6862c0513ea51a87b7d9406711c8735c3ddb93869f376557f12d9658404e199ed793566708460c8e253f4c9e090de11a58192bec30046d3e5e2da3fba585a3a79a257fd40e0d47ff05229a6117f3cd2597fab122706835cb870f91c30c825820920aabe8c54c9f61702f46ee3561832ca2be869cff05cb11af924f1236d275085840099572aeabc6017dafa56d9a9d32781c5106f6d9fa52845485b90dcc3ceb18dae4a52935c19cd3bd90b0b6045a9a076d082a688116507557de8cc9d806e1300f8258205d292057e9e434e77f07acdd65d3013f8f143968ac9211548c1cfa6455857bdb584083a3df06fb905b8955296c1fd232766a7155d8ab3eefb8d107a1c63eb99231ceb4bda73c56513927cba3f779fffc8ee7f2d868a310cd91bb4837ee343d62cf0c8258205a36edd50ea03ecfd1689ca556d05dd95533807ce2f2207f534e84e78d24208b58401a7073596fe77ad17ac1b0de832e73665e3760370b6d1c092a7aea008a77caaedafc29b787caf56958227f5f5a165e0baf9d251f548aa256b953740a58401e0a825820caa4170429db540c9c4fa052d27d4ca4c2bc9be7550fd0debab8928e956fbfd1584087df1772964096b3b69c6e2eb553eba7f66c3f9e13025423daec031a14403e43e100a5bb6db7c5c35e97dbed31e2d3caa135414ebeca4b2819660380e10ed809825820434119e168ea0db1722acb1c011272a23f905479b2f6755acab1ffa383ca43a9584015bf2460505a2936f0ea4d0cde4c0f6caa230974570d0a95b79fb6adbe6627b64285a7f6c201348d3a33e779fa5b37dbe2150df6a2a1f5fd6299e1f02c4ae00e825820938761f68254b4517d100be84f5756931fcdbfbcc15660983138573acde03df658407dfb31c0992ab10085a2bb7a9fd47ba01622ff1308b35b0fece6674e9b0ec07702625e152fbabc25a0e67d4a5c41ed197bccd6b07d3d21357ae10659d150e90c8258208e4c18e38cf5a2bd96f7d76a9141f18552f449887053031db13f0bfc8220d6315840e774f1bcd390867a6e26a372a857d5465012faccc7fa99afa9d28b4b60ac6a52ee678ac12ce9fa04744848402d6407b230e9a09cc600133f9c77aa861c8ea40d825820a4b55d39a912fdeee6742d3afd93c3bc87a780088bd8e29a85aa1e558639e924584079f6f36ea41e4b6e7eac48f9be7abe4cc7888f4b6e9d1ef81bac6c3b85d66451ef5ed0ed4bb9fdd5480aa67ab1a828917d2a9a2864a1870085c1510d2d0ea505825820f1ad14ce051c1e6ad0789343c86f6ebfc0bcfd44082b5d7fc92d32f2820615935840af048de173488fdd083f11a309c021b17ef97b545105e3edea2a874744b50e7e5bed4216259f5a32d7152e228a5fd2f34920a276d490bd816d87f8787d2a150bfff5f6"
                |> Transaction.deserialize
                |> Expect.notEqual Nothing


decodeInputs : Test
decodeInputs =
    test "Decode inputs" <|
        \_ ->
            Bytes.fromStringUnchecked "981c82582002d96ea8d1306db626903c429da7ad93c9b3ae96b4599c7f5b08902ab015dbff008258200e6a00baf157961c7bf14ffaa7fe7bc0699566f17ea43be6611a1b90f7c98dab0082582026b5703faf4113644cef865c2862c2292e3d7ec9329653598a81a49d3309ba9a008258202f61d933eedea3cebad0f8c3cf44695785087e7394d97dcea24b16445296afd000825820378b184e5c838655450a0395459386918171f890acfb1d06256c6540879ad5a3008258203e4fb9dc745b8618de5b23d9006b0b2d9a0a11a2e432988e8a34375f834f2fa10082582052d5415953ce9865a18dc8efcfb003907424a2002108d3b66343fc3115bbf7a90082582059734e1b95b5c991805612f9c20a208ff214f14dbe5f42d808e4e3dbeac707e200825820786a6ae640a413d7076a296e4a9508a24ca9851aae0a68097eebb873ae8c8831008258207dc09eebaeda72ef2e0459c87cb3bac1f374bda9d85cb17c2dadc66df7e439c2008258208fd8687d2da0fbbcd71f0d018eff88f67c8eb062f878450b2c3dc45192fd215c008258209df80549512d3f7f449a62a5cd29487d7871e2f67c474a788e85e0f8c76eacb4008258209eff8340742ac5448e80dddec5540b7cce3c38241e1f3eda7898e197900e4fd700825820b05f55878af5cd123fb733d0f25c106c6da037bfa7001d6681f4db25385b69e700825820b380d843a45b8a4a08203d5412286d3a94d92ab3bcf40dfaec466b00a5902cec00825820bd20e5522790e0efee6a5c819aacf51aea99adf5eb32b60142c93a3ef5a2da0c00825820be32b22489772483acfbdd7105bf899f48f1c4a3c44ae5a52b453adeb724b1ea00825820bf52851e1517a8ca096f0771962755e7c428519182fda70a497f89b4a325d07c00825820c03a711361fcd752b57ceb8339f3f50d2795a12cfae35ca9d831d550f2daf07500825820c5e57f8a142e442bdc6d10dfd20d3e50a71200481dfc8551da3bf1c0706289c300825820d8f9bef425e68b4b065842e9b218e84d484ff08a39b439c4e7d999c563712e3d00825820d9f80e7f14aae77f2eef509d479a5eb60401a2b9e2086879adadc15114f212d500825820ddef7290638c29d85eb634c9e7e621ef51f60ce60dcdbef847b68726de369cb000825820e00f908ba000140d39f2ee35786878118d1b3ddbee84d95b0529a49dfc5e899400825820e956b84b0added596e405cdd0ca52a2872b7b1913bd7c8a5625484b1caff451700825820f4c075f154d0308e41d76234b97a91c979003a93af52f92550d250c15bff635800825820f6717240cc320424a8faeed76c9976537703bbf66cce71a0e5be615d289d400e00825820f969e68448c0ba0b489c64be63a87b8dcfcac5bbcdc61242dc5ed6ab037e651300"
                |> Bytes.toBytes
                |> D.decode (D.list Utxo.decodeOutputReference)
                |> Expect.notEqual Nothing


decodeOutputs : Test
decodeOutputs =
    test "Decode outputs" <|
        \_ ->
            Bytes.fromStringUnchecked "9818825839011b80d26918fdf67926d0e2b3efe2ed925fff9bbb02422a8f57313f03a0051400c3c3e87ba84c3479ad4b96fa18dc3e9868a2bb857d7f5b831a0ded1040825839018b5e37fd8abf9363a54b84c66d23a8e74693037678ad465dd8ebebd09a5ca04fcaa6a06eb9f201d1ee47a9eb9c00df4754ea8abb4d40045e1a0cbc7f8082583901d9bdde6d639a50951deef8a30e08178be0ae1e72d0471c97973ee4e22544031100d245884c0370fc901e7f09fc8b19f251059f0f8b7f174d1a0c509e50825839016630c7dace155a5d6c2ea14fd59085e0b7a17c67ec320f23b63410726f53a523c78edcb5c8fdc857ed1d5c2a9de570976d5573ef77de1f851a0c509e508258390138bba676633b25db054b4705d576ee1cf81dbd1c15666acc4f58edf7a11222873bbe7b0250f482b5a6a361181b398b7bba985b7aa8046bda1a0d8bdd70825839010f34fe3191fc19946b657ddad79c6cba4fbf6f3c5be68eaa76ac19378690d68fd06954f5b6f69f28585394616b24cb046797f6931b878fcd1a0dcc1ac082583901b58c09f5a93b478b77210e81fba0dc3171ee75275e1a2bc72f71f766691a163b8b1fef714fd8604edc1c3e9532134883668d6044cd588aba1a015e0960825839018c663f342c7e37de4e178b6a72435a2ca3c8defe8e59893a5fccb30d6575c9183f86d85907cffa74605891c36b0d83c5c1d163a23f69c0971a0ded1040825839018c663f342c7e37de4e178b6a72435a2ca3c8defe8e59893a5fccb30d6575c9183f86d85907cffa74605891c36b0d83c5c1d163a23f69c0971a004c4b408258390127943d4923eecbf8e2e44fde676ebb1395431ef71d24368ae4bbe1d7b3097e4731e2263ab175820b5b7ce79166c1afb1e230018ca37e38091a0ded10408258390107823056b202bda49afeeba405bdb8dcae3b2a2d5c6db453dba2a1dd7ae4910d818d27a16a3d4b450bfcade07bf1d9d0992cd1148858174f1a0bebc20082583901881c18e58926f8a0a4e0443e4fd3a3f77634b0aded1888d81cdb4ed1436426dc4f2125e50ef00ac160160c460a2f75fa2c614fb759c8f4181a06e60d608258390199fca72b5ce96bd5d5cecbc85e979abfd7bc929b26cda6fafaa8a4e47358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0dca434682583901b6563dbdc671edff4dedd847410c3ce35aeb0528014e7098c8b388bc7358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0c9df95482583901903eed3376b63941b3195efbb8eaee3554928fcbc6250ad26bd159ef7358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0c339bee8258390103dbaffd44bb56eddc84aba9b4d3647bf9fb8118c3d47ee327c205f67358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0c339bee82583901f5269e8d5266b46e6ed1e446771e050ab61defc26720ee7926bf6fe87358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0d6a6dda82583901f1256b96127b2b5aa90ea7ee4038c4cc3b8c1c2e60c42a152bcbd4db7358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0da9c43f825839015963b6413f0929be98ee0f2c864956a9591f8d897dc42d90fd1c395d7358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0168614882583901a47054d66a9a63feb7b0acf64fb1dd91528e48363340975e5e4a90fc7358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0dca434682583901ee067106d922da2a6c6df9d356ab0a04fc62053270d0ebd9d88c04d07358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a005a7b2a8258390140c647979432c0d13b3586f0559156157149361b42b492322299c7cd7358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0dca4346825839015be1cb60a8535828dfdcf9d532e6052918d65c3ec11aa3a03dfebd3e7358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0bd02a2d82583901b1e340be62c7a6b054cb0f8ffc15702babbb0d46b5e679b297d609437358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a06dc8335"
                |> Bytes.toBytes
                |> D.decode (D.list Utxo.decodeOutput)
                |> Expect.notEqual Nothing


{-| First Babbage failure.

Tx id: 9c91bdbbface40d0f6f9099a5888635bfe9ba72873e8d21e3bbc541fb53159b4
Block height: 7791724
Previous block intersection:

  - slot: 72317291
  - id: 5ee70ef716d4a3bb8bdb1164ef49d9b499a600bc7309f4f8df13ba01ac400e67

-}
decode9c91bdbb : Test
decode9c91bdbb =
    test "Tx id 9c91bdbbface40d0f6f9099a5888635bfe9ba72873e8d21e3bbc541fb53159b4" <|
        \_ ->
            Bytes.fromStringUnchecked "84a70082825820f6e81bb9da6b4d635f3d774c7b7a58813a47b899bb52fef34caa4250fa8aa26100825820f6e81bb9da6b4d635f3d774c7b7a58813a47b899bb52fef34caa4250fa8aa261010d81825820f6e81bb9da6b4d635f3d774c7b7a58813a47b899bb52fef34caa4250fa8aa261000182a200581d610a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07011a02e94d2ba300581d716a9391d6aa51af28dd876ebb5565b69d1e83e5ac7861506bd29b56b001821a002dc6c0a1581c8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338da1434d3442030282005820605635647b7fa1f331978bdf5aeed389bdb3369274482ead60c6b86c389cf020021a00037c9d0e81581c0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e070b582074dc5b362d2bf0c2e1d9c3d1e9e4e5f21539a2095323880b8d0ca86d9d3b7af707582091ebd602815a977fff9028bbebbe7bfb7f8ae703c65a684e5c85f380f5249e24a20081825820669ed15b1bc5e97ec45af8951e9cbcbd33a3b5878943704d054a1a3ec46be2825840b18cd30d60e5e8635a3fbaf118b1caef5f8d781e385be23a1d271a0a3680fbcdc39fe9c26614ad4b84547023c19fa5b4ad5a4fd7d65471cb6b0e7de9a8bb8d0d0481d8799fd8799f40ffd8799fa2d8799fd8799fd87a80d8799fd8799f581c0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07ffd87a80ffffd8799f4040ffff1a002dc6c0d8799fd8799fd87a80d8799fd8799f581c0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07ffd87a80ffffd8799f581c8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d434d3442ffff03a0a000ffd87c9f9fd8799fd87b9fd87d9fd9050180d87a9f1b000001836721a9f8ffffffd87a9fd8799fd87a80d8799fd8799f581c0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07ffd87a80ffffd87a9fd8799fd87a80d8799fd8799f581c9205b71f3561fd659782172c34517bc859e3ea6312bace7f7143e570ffd8799fd8799fd8799f581c009dda942712dd276cad4aef90709b214d6b5e331631b2cdfea745cbffffffffffffd8799f581c8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d434d3442ffd87a9f01ffd87a9fd8799fd87a80d8799fd8799f581c0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07ffd87a80ffffd87a9fd8799fd87a80d8799fd8799f581ce19d61fddd5d2484c87370b2a97c44b2d4f012f7859c2d665ae54d29ffd8799fd8799fd8799f581cec6cd8fff4cfddf7bef15a985c0457cd1b5c0b71ef850efabcb26391ffffffffffffd8799f581c8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d434d3442ffd87a9f01ffd87a9fd8799fd87a80d8799fd8799f581c0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07ffd87a80ffffd87a9fd8799fd87a80d8799fd8799f581c2bbbdfab8c28a5703f472f6b28c61cdd066a99790d62cc839d2917daffd8799fd8799fd8799f581c0cff2d99c693c4a7c787995a69262d796d8597a9b4b24e57d81cbe9fffffffffffffd8799f581c8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d434d3442ffd87a9f01ffd87980ffffffffff1b00000183b4610df8d87980fffff5d90103a100a11902a2a1636d736781782557656c636f6d652c204d61726c6f77652c20746f2074686520426162626167652045726121"
                |> Transaction.deserialize
                |> Expect.equal
                    (Just
                        { body = body9c91bdbb
                        , witnessSet = witnessSet9c91bdbb
                        , isValid = True
                        , auxiliaryData = Just auxiliaryData9c91bdbb
                        }
                    )


auxiliaryData9c91bdbb : AuxiliaryData
auxiliaryData9c91bdbb =
    { labels = [ ( N.fromSafeInt 674, Metadatum.Map [ ( Metadatum.String "msg", Metadatum.List [ Metadatum.String "Welcome, Marlowe, to the Babbage Era!" ] ) ] ) ]
    , nativeScripts = []
    , plutusV1Scripts = []
    , plutusV2Scripts = []
    }


body9c91bdbb : TransactionBody
body9c91bdbb =
    { newTxBody
        | auxiliaryDataHash = Just (Bytes.fromStringUnchecked "91ebd602815a977fff9028bbebbe7bfb7f8ae703c65a684e5c85f380f5249e24")
        , collateral = [ { outputIndex = 0, transactionId = Bytes.fromStringUnchecked "f6e81bb9da6b4d635f3d774c7b7a58813a47b899bb52fef34caa4250fa8aa261" } ]
        , fee = Just (N.fromSafeInt 228509)
        , inputs =
            [ { outputIndex = 0, transactionId = Bytes.fromStringUnchecked "f6e81bb9da6b4d635f3d774c7b7a58813a47b899bb52fef34caa4250fa8aa261" }
            , { outputIndex = 1, transactionId = Bytes.fromStringUnchecked "f6e81bb9da6b4d635f3d774c7b7a58813a47b899bb52fef34caa4250fa8aa261" }
            ]
        , outputs =
            [ { address = Address.Shelley { networkId = Mainnet, paymentCredential = Address.VKeyHash (Bytes.fromStringUnchecked "0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07"), stakeCredential = Nothing }
              , amount = Value.onlyLovelace (N.fromSafeInt 48844075)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            , { address = Address.Shelley { networkId = Mainnet, paymentCredential = ScriptHash (Bytes.fromStringUnchecked "6a9391d6aa51af28dd876ebb5565b69d1e83e5ac7861506bd29b56b0"), stakeCredential = Nothing }
              , amount = { assets = bytesMap (Dict.fromList [ ( "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d", bytesMap (Dict.fromList [ ( "4d3442", N.fromSafeInt 3 ) ]) ) ]), lovelace = N.fromSafeInt 3000000 }
              , datumOption = Just (DatumHash (Bytes.fromStringUnchecked "605635647b7fa1f331978bdf5aeed389bdb3369274482ead60c6b86c389cf020"))
              , referenceScript = Nothing
              }
            ]
        , requiredSigners = [ Bytes.fromStringUnchecked "0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07" ]
        , scriptDataHash = Just (Bytes.fromStringUnchecked "74dc5b362d2bf0c2e1d9c3d1e9e4e5f21539a2095323880b8d0ca86d9d3b7af7")
    }


witnessSet9c91bdbb : WitnessSet
witnessSet9c91bdbb =
    { newTxWitnessSet
        | plutusData =
            Just
                [ Data.Constr (N.fromSafeInt 0)
                    [ Data.Constr (N.fromSafeInt 0) [ Data.Bytes (Bytes.fromStringUnchecked "") ]
                    , Data.Constr (N.fromSafeInt 0)
                        [ Data.Map
                            [ ( Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 1) [], Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 0) [ Data.Bytes (Bytes.fromStringUnchecked "0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07") ], Data.Constr (N.fromSafeInt 1) [] ] ], Data.Constr (N.fromSafeInt 0) [ Data.Bytes (Bytes.fromStringUnchecked ""), Data.Bytes (Bytes.fromStringUnchecked "") ] ], Data.Int (Integer.fromNatural (N.fromSafeInt 3000000)) )
                            , ( Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 1) [], Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 0) [ Data.Bytes (Bytes.fromStringUnchecked "0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07") ], Data.Constr (N.fromSafeInt 1) [] ] ], Data.Constr (N.fromSafeInt 0) [ Data.Bytes (Bytes.fromStringUnchecked "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d"), Data.Bytes (Bytes.fromStringUnchecked "4d3442") ] ], Data.Int (Integer.fromNatural (N.fromSafeInt 3)) )
                            ]
                        , Data.Map []
                        , Data.Map []
                        , Data.Int (Integer.fromSafeInt 0)
                        ]
                    , Data.Constr (N.fromSafeInt 3)
                        [ Data.List
                            [ Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 2) [ Data.Constr (N.fromSafeInt 4) [ Data.Constr (N.fromSafeInt 8) [], Data.Constr (N.fromSafeInt 1) [ Data.Int (Integer.fromNatural (bigNat [ 52537848, 24793 ])) ] ] ], Data.Constr (N.fromSafeInt 1) [ Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 1) [], Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 0) [ Data.Bytes (Bytes.fromStringUnchecked "0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07") ], Data.Constr (N.fromSafeInt 1) [] ] ], Data.Constr (N.fromSafeInt 1) [ Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 1) [], Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 0) [ Data.Bytes (Bytes.fromStringUnchecked "9205b71f3561fd659782172c34517bc859e3ea6312bace7f7143e570") ], Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 0) [ Data.Bytes (Bytes.fromStringUnchecked "009dda942712dd276cad4aef90709b214d6b5e331631b2cdfea745cb") ] ] ] ] ] ], Data.Constr (N.fromSafeInt 0) [ Data.Bytes (Bytes.fromStringUnchecked "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d"), Data.Bytes (Bytes.fromStringUnchecked "4d3442") ], Data.Constr (N.fromSafeInt 1) [ Data.Int (Integer.fromNatural (N.fromSafeInt 1)) ], Data.Constr (N.fromSafeInt 1) [ Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 1) [], Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 0) [ Data.Bytes (Bytes.fromStringUnchecked "0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07") ], Data.Constr (N.fromSafeInt 1) [] ] ], Data.Constr (N.fromSafeInt 1) [ Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 1) [], Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 0) [ Data.Bytes (Bytes.fromStringUnchecked "e19d61fddd5d2484c87370b2a97c44b2d4f012f7859c2d665ae54d29") ], Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 0) [ Data.Bytes (Bytes.fromStringUnchecked "ec6cd8fff4cfddf7bef15a985c0457cd1b5c0b71ef850efabcb26391") ] ] ] ] ] ], Data.Constr (N.fromSafeInt 0) [ Data.Bytes (Bytes.fromStringUnchecked "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d"), Data.Bytes (Bytes.fromStringUnchecked "4d3442") ], Data.Constr (N.fromSafeInt 1) [ Data.Int (Integer.fromNatural (N.fromSafeInt 1)) ], Data.Constr (N.fromSafeInt 1) [ Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 1) [], Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 0) [ Data.Bytes (Bytes.fromStringUnchecked "0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07") ], Data.Constr (N.fromSafeInt 1) [] ] ], Data.Constr (N.fromSafeInt 1) [ Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 1) [], Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 0) [ Data.Bytes (Bytes.fromStringUnchecked "2bbbdfab8c28a5703f472f6b28c61cdd066a99790d62cc839d2917da") ], Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 0) [ Data.Bytes (Bytes.fromStringUnchecked "0cff2d99c693c4a7c787995a69262d796d8597a9b4b24e57d81cbe9f") ] ] ] ] ] ], Data.Constr (N.fromSafeInt 0) [ Data.Bytes (Bytes.fromStringUnchecked "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d"), Data.Bytes (Bytes.fromStringUnchecked "4d3442") ], Data.Constr (N.fromSafeInt 1) [ Data.Int (Integer.fromNatural (N.fromSafeInt 1)) ], Data.Constr (N.fromSafeInt 0) [] ] ] ] ]
                            ]
                        , Data.Int (Integer.fromNatural (bigNat [ 6360568, 24813 ]))
                        , Data.Constr (N.fromSafeInt 0) []
                        ]
                    ]
                ]
        , vkeywitness =
            Just
                [ { signature = Bytes.fromStringUnchecked "b18cd30d60e5e8635a3fbaf118b1caef5f8d781e385be23a1d271a0a3680fbcdc39fe9c26614ad4b84547023c19fa5b4ad5a4fd7d65471cb6b0e7de9a8bb8d0d"
                  , vkey = Bytes.fromStringUnchecked "669ed15b1bc5e97ec45af8951e9cbcbd33a3b5878943704d054a1a3ec46be282"
                  }
                ]
    }



-- Helpers


newTxBody : TransactionBody
newTxBody =
    Tx.newBody


newTxWitnessSet : WitnessSet
newTxWitnessSet =
    Tx.newWitnessSet


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



-- decodeAnyAndFailTest : Bytes a -> Expectation
-- decodeAnyAndFailTest bytes =
--     Cbor.Decode.decode Cbor.Decode.any (Bytes.toBytes bytes)
--         |> Expect.equal Nothing
