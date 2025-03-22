module Cardano.TransactionTests2 exposing (suite)

import Bytes.Comparable as Bytes
import Bytes.Map exposing (BytesMap)
import Cardano.Address as Address exposing (Credential(..), NetworkId(..))
import Cardano.AuxiliaryData exposing (AuxiliaryData)
import Cardano.Data as Data
import Cardano.Gov exposing (Action(..), Drep(..), Vote(..), Voter(..), noParamUpdate)
import Cardano.Metadatum as Metadatum
import Cardano.Transaction as Transaction exposing (Certificate(..), TransactionBody, WitnessSet, newBody, newWitnessSet)
import Cardano.Utxo as Utxo exposing (DatumOption(..))
import Cardano.Value as Value
import Cbor.Decode as D
import Dict exposing (Dict)
import Expect
import Integer
import Natural as N exposing (Natural)
import Test exposing (Test, describe, test)


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
            , decodefd83f4f9

            -- Conway transactions
            , decodeef45fe8e
            , decode4385b3d8
            , decode1e5dd53b
            , decode15f82a36
            , decode7925c2e6
            , decode2264f554
            ]
        , decodeInputs
        , decodeOutputs
        , decodeOutputfd83f4f9
        , decode437389ef
        , decode59fd3532
        , decodeOutputa4005839
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
            Bytes.fromHexUnchecked "84a6008482582041b2bf9badec879f09dc9154c582da12f0c5a7970cd3f922ed34baf83e19e2e10082582041b2bf9badec879f09dc9154c582da12f0c5a7970cd3f922ed34baf83e19e2e10182582041b2bf9badec879f09dc9154c582da12f0c5a7970cd3f922ed34baf83e19e2e10282582041b2bf9badec879f09dc9154c582da12f0c5a7970cd3f922ed34baf83e19e2e1030d8182582041b2bf9badec879f09dc9154c582da12f0c5a7970cd3f922ed34baf83e19e2e103018482581d6108f30ea42a1e844e389e7e0483e69376dc8babd01ad91ab536c8b8891a02a0d28883581d71e265b741cd9ac2c4d695402f7bb4cb49cbe0d9e33eec9a26dbfa9e59821a004c4b40a1581cfb353334ac071f79f6d938e0972640a6b6650815124e9d63fbc0b5e8a1444641524d01582060cc87c3d3c639bb292b00670f83a08cb6b4abd20e018b137377b34ccf2ccf2982581d6108f30ea42a1e844e389e7e0483e69376dc8babd01ad91ab536c8b889821a02c2730da1581cfb353334ac071f79f6d938e0972640a6b6650815124e9d63fbc0b5e8a144434f524e0182581d6108f30ea42a1e844e389e7e0483e69376dc8babd01ad91ab536c8b8891a001e8480021a00031c050e8007582036663d429bded43331a968fcaa3a0aba03d6d83474176b8c85a019b0b408ff8da100818258202a61b85e796ce56b7cb960696f27caf8867e3a209fb5243e935ac3bf99aa0c0c58406ee6dfafc7fdcd553bf0c11cc93d165c77a93a265af250eafb1dca2b044ae0b62d6eb4969e7946c438be5f73b0d9a25ad83d074c8d9cd6f4c80ace7b7c62ab0df5d90103a100a11902a2a1636d736783783157656c636f6d6520746f207468652050494759204f7261636c6520616e6420746f2074686520416c6f6e7a6f206572612160781c68747470733a2f2f6f7261636c652e70696779746f6b656e2e636f6d"
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
    , plutusV3Scripts = []
    }


body8a8f8dfe : TransactionBody
body8a8f8dfe =
    { newBody
        | auxiliaryDataHash = Just (Bytes.fromHexUnchecked "36663d429bded43331a968fcaa3a0aba03d6d83474176b8c85a019b0b408ff8d")
        , fee = N.fromSafeInt 203781
        , inputs =
            [ { outputIndex = 0, transactionId = Bytes.fromHexUnchecked "41b2bf9badec879f09dc9154c582da12f0c5a7970cd3f922ed34baf83e19e2e1" }
            , { outputIndex = 1, transactionId = Bytes.fromHexUnchecked "41b2bf9badec879f09dc9154c582da12f0c5a7970cd3f922ed34baf83e19e2e1" }
            , { outputIndex = 2, transactionId = Bytes.fromHexUnchecked "41b2bf9badec879f09dc9154c582da12f0c5a7970cd3f922ed34baf83e19e2e1" }
            , { outputIndex = 3, transactionId = Bytes.fromHexUnchecked "41b2bf9badec879f09dc9154c582da12f0c5a7970cd3f922ed34baf83e19e2e1" }
            ]
        , collateral = [ { outputIndex = 3, transactionId = Bytes.fromHexUnchecked "41b2bf9badec879f09dc9154c582da12f0c5a7970cd3f922ed34baf83e19e2e1" } ]
        , outputs =
            [ { address = Address.Shelley { networkId = Mainnet, paymentCredential = Address.VKeyHash (Bytes.fromHexUnchecked "08f30ea42a1e844e389e7e0483e69376dc8babd01ad91ab536c8b889"), stakeCredential = Nothing }
              , amount = Value.onlyLovelace (N.fromSafeInt 44094088)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            , { address = Address.Shelley { networkId = Mainnet, paymentCredential = ScriptHash (Bytes.fromHexUnchecked "e265b741cd9ac2c4d695402f7bb4cb49cbe0d9e33eec9a26dbfa9e59"), stakeCredential = Nothing }
              , amount = { assets = bytesMap (Dict.fromList [ ( "fb353334ac071f79f6d938e0972640a6b6650815124e9d63fbc0b5e8", bytesMap (Dict.fromList [ ( "4641524d", N.fromSafeInt 1 ) ]) ) ]), lovelace = N.fromSafeInt 5000000 }
              , datumOption = Just (DatumHash (Bytes.fromHexUnchecked "60cc87c3d3c639bb292b00670f83a08cb6b4abd20e018b137377b34ccf2ccf29"))
              , referenceScript = Nothing
              }
            , { address = Address.Shelley { networkId = Mainnet, paymentCredential = Address.VKeyHash (Bytes.fromHexUnchecked "08f30ea42a1e844e389e7e0483e69376dc8babd01ad91ab536c8b889"), stakeCredential = Nothing }
              , amount = { assets = bytesMap (Dict.fromList [ ( "fb353334ac071f79f6d938e0972640a6b6650815124e9d63fbc0b5e8", bytesMap (Dict.fromList [ ( "434f524e", N.fromSafeInt 1 ) ]) ) ]), lovelace = N.fromSafeInt 46297869 }
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            , { address = Address.Shelley { networkId = Mainnet, paymentCredential = Address.VKeyHash (Bytes.fromHexUnchecked "08f30ea42a1e844e389e7e0483e69376dc8babd01ad91ab536c8b889"), stakeCredential = Nothing }
              , amount = Value.onlyLovelace (N.fromSafeInt 2000000)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            ]
    }


witnessSet8a8f8dfe : WitnessSet
witnessSet8a8f8dfe =
    { newWitnessSet
        | vkeywitness =
            Just
                [ { signature = Bytes.fromHexUnchecked "6ee6dfafc7fdcd553bf0c11cc93d165c77a93a265af250eafb1dca2b044ae0b62d6eb4969e7946c438be5f73b0d9a25ad83d074c8d9cd6f4c80ace7b7c62ab0d"
                  , vkey = Bytes.fromHexUnchecked "2a61b85e796ce56b7cb960696f27caf8867e3a209fb5243e935ac3bf99aa0c0c"
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
            Bytes.fromHexUnchecked "84a6008182582003b02cff29a5f2dfc827e00345eaab8b29a3d740e9878aa6e5dd2b52da0763c5000d80018182581d61d80fe69ded1ff90f41e526d0332a2ff98ba8a0d85ceb8941b51784201a05633249021a00033a9d0682a7581c162f94554ac8c225383a2248c245659eda870eaa82d0ef25fc7dcd82a2021a0001200014821a00aba9501b00000002540be400581c2075a095b3c844a29c24317a94a643ab8e22d54a3a3a72a420260af6a2021a0001200014821a00aba9501b00000002540be400581c268cfc0b89e910ead22e0ade91493d8212f53f3e2164b2e4bef0819ba2021a0001200014821a00aba9501b00000002540be400581c60baee25cbc90047e83fd01e1e57dc0b06d3d0cb150d0ab40bbfead1a2021a0001200014821a00aba9501b00000002540be400581cad5463153dc3d24b9ff133e46136028bdc1edbb897f5a7cf1b37950ca2021a0001200014821a00aba9501b00000002540be400581cb9547b8a57656539a8d9bc42c008e38d9c8bd9c8adbb1e73ad529497a2021a0001200014821a00aba9501b00000002540be400581cf7b341c14cd58fca4195a9b278cce1ef402dc0e06deb77e543cd1757a2021a0001200014821a00aba9501b00000002540be4001901310e80a1008882582061261a95b7613ee6bf2067dad77b70349729b0c50d57bc1cf30de0db4a1e73a85840c65d631ecb286668eeef3537c279fb0c5c5d54bb7ab71a6d0c795f48f6093e664f9e923fd590e3373dd9e054eb622724cb107673a83ad201f503622cdcdae6038258209180d818e69cd997e34663c418a648c076f2e19cd4194e486e159d8580bc6cda584030f64d310d2cc64178fd86681013ba0960d76f5c404d434833238ed2f73a7336fb271027239f0ad0c99436852c023ee95b68d99f02b9956db5776abc8379320a82582089c29f8c4af27b7accbe589747820134ebbaa1caf3ce949270a3d0c7dcfd541b5840fe9cafe18e8fe6c31c2c88012aede78b220857d854a6f488f49aa993aef14d891548c3fd1c2c6c8edfb749c999be26f716991447ae2c0461fa6d7a8d573b0a02825820f14f712dc600d793052d4842d50cefa4e65884ea6cf83707079eb8ce302efc85584001b76c22a07514dfd86182e350e94789c04ff868fc0351a7c74fb58ca6642a658ea1e56c373fa2de85fd1a47bad40f25a62f2caee709d40368ffc14a223171018258208b53207629f9a30e4b2015044f337c01735abe67243c19470c9dae8c7b7327985840fa2b5fc33a08f863c47a67bc5f6fc649b83f0b650ec7ed9329b11db77c562262bd15d5fc97ee71efd4b2df98db9ea133a05aa7f04955d4b21a0c086acc0479018258205fddeedade2714d6db2f9e1104743d2d8d818ecddc306e176108db14caadd4415840ee63870c4c1f27866073b05b5dd418f21712667739d289997c01fbbf28ff0cb3987629588cb13a5459844066d526ce0cde89dc26af5b8108654b0514f7578f0a825820cbc6b506e94fbefe442eecee376f3b3ebaf89415ef5cd2efb666e06ddae483935840a2da7ce96693480cd49e256819b4d7a565f3a973864fdc8e4225e8a3338605aab18d6c1e7099129671fb479a8ee38aa67dde383f012741fcec02afbc82a0f903825820e8c03a03c0b2ddbea4195caf39f41e669f7d251ecf221fbb2f275c0a5d7e05d158402086186c201f064e685ca7fd50f4b313fc1f86b0e0a68cc8db2e4548cb42e2e47ffbdf07c80352484a06332f4e9a180ff8846d3cadd92d0b6717a57482127a08f5f6"
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
    { newBody
        | fee = N.fromSafeInt 211613
        , inputs = [ { outputIndex = 0, transactionId = Bytes.fromHexUnchecked "03b02cff29a5f2dfc827e00345eaab8b29a3d740e9878aa6e5dd2b52da0763c5" } ]
        , outputs =
            [ { address = Address.Shelley { networkId = Mainnet, paymentCredential = Address.VKeyHash (Bytes.fromHexUnchecked "d80fe69ded1ff90f41e526d0332a2ff98ba8a0d85ceb8941b5178420"), stakeCredential = Nothing }
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
    { newWitnessSet
        | vkeywitness =
            Just
                [ { signature = Bytes.fromHexUnchecked "c65d631ecb286668eeef3537c279fb0c5c5d54bb7ab71a6d0c795f48f6093e664f9e923fd590e3373dd9e054eb622724cb107673a83ad201f503622cdcdae603", vkey = Bytes.fromHexUnchecked "61261a95b7613ee6bf2067dad77b70349729b0c50d57bc1cf30de0db4a1e73a8" }
                , { signature = Bytes.fromHexUnchecked "30f64d310d2cc64178fd86681013ba0960d76f5c404d434833238ed2f73a7336fb271027239f0ad0c99436852c023ee95b68d99f02b9956db5776abc8379320a", vkey = Bytes.fromHexUnchecked "9180d818e69cd997e34663c418a648c076f2e19cd4194e486e159d8580bc6cda" }
                , { signature = Bytes.fromHexUnchecked "fe9cafe18e8fe6c31c2c88012aede78b220857d854a6f488f49aa993aef14d891548c3fd1c2c6c8edfb749c999be26f716991447ae2c0461fa6d7a8d573b0a02", vkey = Bytes.fromHexUnchecked "89c29f8c4af27b7accbe589747820134ebbaa1caf3ce949270a3d0c7dcfd541b" }
                , { signature = Bytes.fromHexUnchecked "01b76c22a07514dfd86182e350e94789c04ff868fc0351a7c74fb58ca6642a658ea1e56c373fa2de85fd1a47bad40f25a62f2caee709d40368ffc14a22317101", vkey = Bytes.fromHexUnchecked "f14f712dc600d793052d4842d50cefa4e65884ea6cf83707079eb8ce302efc85" }
                , { signature = Bytes.fromHexUnchecked "fa2b5fc33a08f863c47a67bc5f6fc649b83f0b650ec7ed9329b11db77c562262bd15d5fc97ee71efd4b2df98db9ea133a05aa7f04955d4b21a0c086acc047901", vkey = Bytes.fromHexUnchecked "8b53207629f9a30e4b2015044f337c01735abe67243c19470c9dae8c7b732798" }
                , { signature = Bytes.fromHexUnchecked "ee63870c4c1f27866073b05b5dd418f21712667739d289997c01fbbf28ff0cb3987629588cb13a5459844066d526ce0cde89dc26af5b8108654b0514f7578f0a", vkey = Bytes.fromHexUnchecked "5fddeedade2714d6db2f9e1104743d2d8d818ecddc306e176108db14caadd441" }
                , { signature = Bytes.fromHexUnchecked "a2da7ce96693480cd49e256819b4d7a565f3a973864fdc8e4225e8a3338605aab18d6c1e7099129671fb479a8ee38aa67dde383f012741fcec02afbc82a0f903", vkey = Bytes.fromHexUnchecked "cbc6b506e94fbefe442eecee376f3b3ebaf89415ef5cd2efb666e06ddae48393" }
                , { signature = Bytes.fromHexUnchecked "2086186c201f064e685ca7fd50f4b313fc1f86b0e0a68cc8db2e4548cb42e2e47ffbdf07c80352484a06332f4e9a180ff8846d3cadd92d0b6717a57482127a08", vkey = Bytes.fromHexUnchecked "e8c03a03c0b2ddbea4195caf39f41e669f7d251ecf221fbb2f275c0a5d7e05d1" }
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
            Bytes.fromHexUnchecked "84a6009f82582002d96ea8d1306db626903c429da7ad93c9b3ae96b4599c7f5b08902ab015dbff008258200e6a00baf157961c7bf14ffaa7fe7bc0699566f17ea43be6611a1b90f7c98dab0082582026b5703faf4113644cef865c2862c2292e3d7ec9329653598a81a49d3309ba9a008258202f61d933eedea3cebad0f8c3cf44695785087e7394d97dcea24b16445296afd000825820378b184e5c838655450a0395459386918171f890acfb1d06256c6540879ad5a3008258203e4fb9dc745b8618de5b23d9006b0b2d9a0a11a2e432988e8a34375f834f2fa10082582052d5415953ce9865a18dc8efcfb003907424a2002108d3b66343fc3115bbf7a90082582059734e1b95b5c991805612f9c20a208ff214f14dbe5f42d808e4e3dbeac707e200825820786a6ae640a413d7076a296e4a9508a24ca9851aae0a68097eebb873ae8c8831008258207dc09eebaeda72ef2e0459c87cb3bac1f374bda9d85cb17c2dadc66df7e439c2008258208fd8687d2da0fbbcd71f0d018eff88f67c8eb062f878450b2c3dc45192fd215c008258209df80549512d3f7f449a62a5cd29487d7871e2f67c474a788e85e0f8c76eacb4008258209eff8340742ac5448e80dddec5540b7cce3c38241e1f3eda7898e197900e4fd700825820b05f55878af5cd123fb733d0f25c106c6da037bfa7001d6681f4db25385b69e700825820b380d843a45b8a4a08203d5412286d3a94d92ab3bcf40dfaec466b00a5902cec00825820bd20e5522790e0efee6a5c819aacf51aea99adf5eb32b60142c93a3ef5a2da0c00825820be32b22489772483acfbdd7105bf899f48f1c4a3c44ae5a52b453adeb724b1ea00825820bf52851e1517a8ca096f0771962755e7c428519182fda70a497f89b4a325d07c00825820c03a711361fcd752b57ceb8339f3f50d2795a12cfae35ca9d831d550f2daf07500825820c5e57f8a142e442bdc6d10dfd20d3e50a71200481dfc8551da3bf1c0706289c300825820d8f9bef425e68b4b065842e9b218e84d484ff08a39b439c4e7d999c563712e3d00825820d9f80e7f14aae77f2eef509d479a5eb60401a2b9e2086879adadc15114f212d500825820ddef7290638c29d85eb634c9e7e621ef51f60ce60dcdbef847b68726de369cb000825820e00f908ba000140d39f2ee35786878118d1b3ddbee84d95b0529a49dfc5e899400825820e956b84b0added596e405cdd0ca52a2872b7b1913bd7c8a5625484b1caff451700825820f4c075f154d0308e41d76234b97a91c979003a93af52f92550d250c15bff635800825820f6717240cc320424a8faeed76c9976537703bbf66cce71a0e5be615d289d400e00825820f969e68448c0ba0b489c64be63a87b8dcfcac5bbcdc61242dc5ed6ab037e651300ff0d80019f825839011b80d26918fdf67926d0e2b3efe2ed925fff9bbb02422a8f57313f03a0051400c3c3e87ba84c3479ad4b96fa18dc3e9868a2bb857d7f5b831a0ded1040825839018b5e37fd8abf9363a54b84c66d23a8e74693037678ad465dd8ebebd09a5ca04fcaa6a06eb9f201d1ee47a9eb9c00df4754ea8abb4d40045e1a0cbc7f8082583901d9bdde6d639a50951deef8a30e08178be0ae1e72d0471c97973ee4e22544031100d245884c0370fc901e7f09fc8b19f251059f0f8b7f174d1a0c509e50825839016630c7dace155a5d6c2ea14fd59085e0b7a17c67ec320f23b63410726f53a523c78edcb5c8fdc857ed1d5c2a9de570976d5573ef77de1f851a0c509e508258390138bba676633b25db054b4705d576ee1cf81dbd1c15666acc4f58edf7a11222873bbe7b0250f482b5a6a361181b398b7bba985b7aa8046bda1a0d8bdd70825839010f34fe3191fc19946b657ddad79c6cba4fbf6f3c5be68eaa76ac19378690d68fd06954f5b6f69f28585394616b24cb046797f6931b878fcd1a0dcc1ac082583901b58c09f5a93b478b77210e81fba0dc3171ee75275e1a2bc72f71f766691a163b8b1fef714fd8604edc1c3e9532134883668d6044cd588aba1a015e0960825839018c663f342c7e37de4e178b6a72435a2ca3c8defe8e59893a5fccb30d6575c9183f86d85907cffa74605891c36b0d83c5c1d163a23f69c0971a0ded1040825839018c663f342c7e37de4e178b6a72435a2ca3c8defe8e59893a5fccb30d6575c9183f86d85907cffa74605891c36b0d83c5c1d163a23f69c0971a004c4b408258390127943d4923eecbf8e2e44fde676ebb1395431ef71d24368ae4bbe1d7b3097e4731e2263ab175820b5b7ce79166c1afb1e230018ca37e38091a0ded10408258390107823056b202bda49afeeba405bdb8dcae3b2a2d5c6db453dba2a1dd7ae4910d818d27a16a3d4b450bfcade07bf1d9d0992cd1148858174f1a0bebc20082583901881c18e58926f8a0a4e0443e4fd3a3f77634b0aded1888d81cdb4ed1436426dc4f2125e50ef00ac160160c460a2f75fa2c614fb759c8f4181a06e60d608258390199fca72b5ce96bd5d5cecbc85e979abfd7bc929b26cda6fafaa8a4e47358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0dca434682583901b6563dbdc671edff4dedd847410c3ce35aeb0528014e7098c8b388bc7358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0c9df95482583901903eed3376b63941b3195efbb8eaee3554928fcbc6250ad26bd159ef7358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0c339bee8258390103dbaffd44bb56eddc84aba9b4d3647bf9fb8118c3d47ee327c205f67358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0c339bee82583901f5269e8d5266b46e6ed1e446771e050ab61defc26720ee7926bf6fe87358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0d6a6dda82583901f1256b96127b2b5aa90ea7ee4038c4cc3b8c1c2e60c42a152bcbd4db7358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0da9c43f825839015963b6413f0929be98ee0f2c864956a9591f8d897dc42d90fd1c395d7358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0168614882583901a47054d66a9a63feb7b0acf64fb1dd91528e48363340975e5e4a90fc7358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0dca434682583901ee067106d922da2a6c6df9d356ab0a04fc62053270d0ebd9d88c04d07358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a005a7b2a8258390140c647979432c0d13b3586f0559156157149361b42b492322299c7cd7358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0dca4346825839015be1cb60a8535828dfdcf9d532e6052918d65c3ec11aa3a03dfebd3e7358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0bd02a2d82583901b1e340be62c7a6b054cb0f8ffc15702babbb0d46b5e679b297d609437358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a06dc8335ff021a000623b1031a0328c7080e80a1009f825820d82b68e8776af0104787d575fd5ee34fc2617d8578d569189df463b7994850f85840cb880741a25f15d6b991d12bcccc77b15c449aedc840479d3f654317f6004547825db37a16b371a5a2cc3660d1498e0501937e55ed87142e051941cf4140200b825820776dab08ce4554672163ff9ca04d96ebb4d2b3663d900447f34711157305fbf3584099b2913cfe343fecc8e6ab5a7431b3ef53f30698788c00a9f9f0f4d67adad02433fdc200a69250c523ae90375a59fbd5cce35f2bc46d1598cbd4cc7c5a94c50c825820cebbc1e8b4b624e459365a765600c8ccba296a278354b8b6e0e9c983309f717158405aa1c3f3ef4f76b9c5d15b5b066dd15c3617241e63524d834610b1af5fdf65e2df72dea61ea9b0ea5ce7d542ef00f6b87f66f8c26549c08e13a613a3c7c87706825820362dc2c18266f7d1a15bb8f3770381b5cc9a5524a51030383fec9fd97014dba55840f58455930a3a65a6ae51e251fc33ee726b8606710a088225db496c43a3bdaf5618af058b1e2b1432b6123ea0f2fabedc3dd440cdf470c3eadbdbc8b89af5d40282582044b417b72eab77e853f21cbb8a9388c7fd6760b1d266e95a86bc5f166dca5c5158403481a1e0fc77f3977e628f79c184f1a0ea745cb2761b8ad7d31d2c2b6e28d8d8b65255b3f0cecdf991688ba79524156dbc3fe33c4c9152ce41717bc759956d02825820bd154a180286999f08c4ca6da13dfd4506e0e0865067ff85ce36445ea29992605840cc7019199cd5264c2210755c2aafb7ef28c7dfddf0bdb265f915c9670dc04b63bd4cfa8b3911f4dad09bd600f83aef7957504e6b90d3f41c90c56d3cdd38000c8258209c41eafe7c334e0420c7ec2ad29fb135bc84bc555ba0add035cd53b844c7ece15840502acb3489cbe2a7c6564d87f4d90151531803494675b713904ee648f244e861fb12fde2be0ef0e1a2b0f6c2a66ad2e377089c0683c9f3b9bc46d0d0dc215c068258204fbcc7a463c6b7d9bcd7cb98a521a9ec499665e1879f4a7dbba47f5087886dd158407b8dbf1ab690150b84a486dcd8a5f59fbd7fc753078b16b45c38e8b7073d2ab0a241738eb8e9b621b83f7f313ac8088104168092ee6985160f3f83ce25e0cf068258204a6ac7bd375049286c2d85c3dcbef160cda8cc7c0336e008ef2104af3e1db5fc5840948d3f05f899d323b37a9e06207b0aa066f562d834e981dfe02eed5002b4b42eab9e5cce3fc5779ab4d9964a5c2aeb7c641a739fb885526388ee1dcbd5ae1d08825820d7f065f74dda041b5138becae296b3052950583458927d5bc497a01ba17d46da58403d547d376ce8e5e9a5ae9df522ea7049a0a529dd11564a41e0c7e63ebdbbec006d5afc1e0e5e1dea7e0f7f283429f2e3ec6d73150def929fd04de7cd2b12690882582074be23e35fe1a0d0b78e118d85ba6360c5865715eb12e9ea42ceca8fc27fa85f58401d5b559d3c95fa2c9fde08acb5829d2c607eb66a6f2cc4bde90d66750d70e5f6a7ce2b49240d354a520308af2838750352719d961088d5fce8db71eb593677008258209aa034218b8f7f45227e9f8897262cfe570a028ca9dfd1453c913b909afc080258404d0475f5c869ef826a53416182fca178dae42d1b266559397046b580ced712790c7722f4e1720846519a80bfcbaf7ec2f911dd9f2dab86229924d88fd5a29e08825820d5a8a386c18e6eabc5e9a2344208179c20400d030b4513d58f04481984f99cfe5840cccd7de81ed364bd9f74fa8390ef5f4ae8aeba57793a530a3d6087804fdc369c42cd9a8badd1a9d8396284f650bca51bde418746e55596250e9dcf155c84cf0682582004ceb23e99ffd81dfac1393de760c597901d3c09d754be2465c24aa80b22ceaf5840320cf0f065482dfa35c4ae151316df8716a52952f8126c9a2768a635a70171a1f328b5b2a1283583f019d0504645fef3e38d6505ca18f7e830b3d125320e8902825820f574ee10684cc2cca113ab9694a1b4c9af7a347f527d20bbc67b24006ad8a8915840c5bfd0408ff3a6734abd551223c548bc68a329ae44356a4d0c79af6217cb2f1b6e09cb4e799a44465b038cfa871b794e695d6e290599588b4e96d636ac6d0e0f8258203b9efb6584ca0acb7e9f1dd13061dacef808cf1416e18575f2e930afa0f25e6a58402549851005bbee6454f7324c26c77d92e2e233f0905cab50d798a247380bd7401591ccd3927fba92795963eeed705048fd44f8787bb681d1dceee8007e8cdf038258209b0795de2b2619b165fd16563fc1e2f4637bdb511efc229e6d56c1e5f8ea37835840f15b7285c92c3627a1ad1f6ed54e426972817a4929475c8a1e682aeedb2fccc954e4c314a6a9f72cf31bc231c28fa1ea572076949740c15ecfd48c3e8f2cc200825820e4d27651476d05cdd43888a7d795ae2b74eeb0c3d6fb7131bd71ac0047c4de545840446665c2a75db6daeb5a4c8c8b586fa5b358c7aee35f85367c73aebe3efe96af221e3520746e6e810fdf24eef08fb71ff359f3c94644f4d64726e3398188500d825820dbbb79767f6862c0513ea51a87b7d9406711c8735c3ddb93869f376557f12d9658404e199ed793566708460c8e253f4c9e090de11a58192bec30046d3e5e2da3fba585a3a79a257fd40e0d47ff05229a6117f3cd2597fab122706835cb870f91c30c825820920aabe8c54c9f61702f46ee3561832ca2be869cff05cb11af924f1236d275085840099572aeabc6017dafa56d9a9d32781c5106f6d9fa52845485b90dcc3ceb18dae4a52935c19cd3bd90b0b6045a9a076d082a688116507557de8cc9d806e1300f8258205d292057e9e434e77f07acdd65d3013f8f143968ac9211548c1cfa6455857bdb584083a3df06fb905b8955296c1fd232766a7155d8ab3eefb8d107a1c63eb99231ceb4bda73c56513927cba3f779fffc8ee7f2d868a310cd91bb4837ee343d62cf0c8258205a36edd50ea03ecfd1689ca556d05dd95533807ce2f2207f534e84e78d24208b58401a7073596fe77ad17ac1b0de832e73665e3760370b6d1c092a7aea008a77caaedafc29b787caf56958227f5f5a165e0baf9d251f548aa256b953740a58401e0a825820caa4170429db540c9c4fa052d27d4ca4c2bc9be7550fd0debab8928e956fbfd1584087df1772964096b3b69c6e2eb553eba7f66c3f9e13025423daec031a14403e43e100a5bb6db7c5c35e97dbed31e2d3caa135414ebeca4b2819660380e10ed809825820434119e168ea0db1722acb1c011272a23f905479b2f6755acab1ffa383ca43a9584015bf2460505a2936f0ea4d0cde4c0f6caa230974570d0a95b79fb6adbe6627b64285a7f6c201348d3a33e779fa5b37dbe2150df6a2a1f5fd6299e1f02c4ae00e825820938761f68254b4517d100be84f5756931fcdbfbcc15660983138573acde03df658407dfb31c0992ab10085a2bb7a9fd47ba01622ff1308b35b0fece6674e9b0ec07702625e152fbabc25a0e67d4a5c41ed197bccd6b07d3d21357ae10659d150e90c8258208e4c18e38cf5a2bd96f7d76a9141f18552f449887053031db13f0bfc8220d6315840e774f1bcd390867a6e26a372a857d5465012faccc7fa99afa9d28b4b60ac6a52ee678ac12ce9fa04744848402d6407b230e9a09cc600133f9c77aa861c8ea40d825820a4b55d39a912fdeee6742d3afd93c3bc87a780088bd8e29a85aa1e558639e924584079f6f36ea41e4b6e7eac48f9be7abe4cc7888f4b6e9d1ef81bac6c3b85d66451ef5ed0ed4bb9fdd5480aa67ab1a828917d2a9a2864a1870085c1510d2d0ea505825820f1ad14ce051c1e6ad0789343c86f6ebfc0bcfd44082b5d7fc92d32f2820615935840af048de173488fdd083f11a309c021b17ef97b545105e3edea2a874744b50e7e5bed4216259f5a32d7152e228a5fd2f34920a276d490bd816d87f8787d2a150bfff5f6"
                |> Transaction.deserialize
                |> Expect.notEqual Nothing


decodeInputs : Test
decodeInputs =
    test "Decode inputs" <|
        \_ ->
            Bytes.fromHexUnchecked "981c82582002d96ea8d1306db626903c429da7ad93c9b3ae96b4599c7f5b08902ab015dbff008258200e6a00baf157961c7bf14ffaa7fe7bc0699566f17ea43be6611a1b90f7c98dab0082582026b5703faf4113644cef865c2862c2292e3d7ec9329653598a81a49d3309ba9a008258202f61d933eedea3cebad0f8c3cf44695785087e7394d97dcea24b16445296afd000825820378b184e5c838655450a0395459386918171f890acfb1d06256c6540879ad5a3008258203e4fb9dc745b8618de5b23d9006b0b2d9a0a11a2e432988e8a34375f834f2fa10082582052d5415953ce9865a18dc8efcfb003907424a2002108d3b66343fc3115bbf7a90082582059734e1b95b5c991805612f9c20a208ff214f14dbe5f42d808e4e3dbeac707e200825820786a6ae640a413d7076a296e4a9508a24ca9851aae0a68097eebb873ae8c8831008258207dc09eebaeda72ef2e0459c87cb3bac1f374bda9d85cb17c2dadc66df7e439c2008258208fd8687d2da0fbbcd71f0d018eff88f67c8eb062f878450b2c3dc45192fd215c008258209df80549512d3f7f449a62a5cd29487d7871e2f67c474a788e85e0f8c76eacb4008258209eff8340742ac5448e80dddec5540b7cce3c38241e1f3eda7898e197900e4fd700825820b05f55878af5cd123fb733d0f25c106c6da037bfa7001d6681f4db25385b69e700825820b380d843a45b8a4a08203d5412286d3a94d92ab3bcf40dfaec466b00a5902cec00825820bd20e5522790e0efee6a5c819aacf51aea99adf5eb32b60142c93a3ef5a2da0c00825820be32b22489772483acfbdd7105bf899f48f1c4a3c44ae5a52b453adeb724b1ea00825820bf52851e1517a8ca096f0771962755e7c428519182fda70a497f89b4a325d07c00825820c03a711361fcd752b57ceb8339f3f50d2795a12cfae35ca9d831d550f2daf07500825820c5e57f8a142e442bdc6d10dfd20d3e50a71200481dfc8551da3bf1c0706289c300825820d8f9bef425e68b4b065842e9b218e84d484ff08a39b439c4e7d999c563712e3d00825820d9f80e7f14aae77f2eef509d479a5eb60401a2b9e2086879adadc15114f212d500825820ddef7290638c29d85eb634c9e7e621ef51f60ce60dcdbef847b68726de369cb000825820e00f908ba000140d39f2ee35786878118d1b3ddbee84d95b0529a49dfc5e899400825820e956b84b0added596e405cdd0ca52a2872b7b1913bd7c8a5625484b1caff451700825820f4c075f154d0308e41d76234b97a91c979003a93af52f92550d250c15bff635800825820f6717240cc320424a8faeed76c9976537703bbf66cce71a0e5be615d289d400e00825820f969e68448c0ba0b489c64be63a87b8dcfcac5bbcdc61242dc5ed6ab037e651300"
                |> Bytes.toBytes
                |> D.decode (D.list Utxo.decodeOutputReference)
                |> Expect.notEqual Nothing


decodeOutputs : Test
decodeOutputs =
    test "Decode outputs" <|
        \_ ->
            Bytes.fromHexUnchecked "9818825839011b80d26918fdf67926d0e2b3efe2ed925fff9bbb02422a8f57313f03a0051400c3c3e87ba84c3479ad4b96fa18dc3e9868a2bb857d7f5b831a0ded1040825839018b5e37fd8abf9363a54b84c66d23a8e74693037678ad465dd8ebebd09a5ca04fcaa6a06eb9f201d1ee47a9eb9c00df4754ea8abb4d40045e1a0cbc7f8082583901d9bdde6d639a50951deef8a30e08178be0ae1e72d0471c97973ee4e22544031100d245884c0370fc901e7f09fc8b19f251059f0f8b7f174d1a0c509e50825839016630c7dace155a5d6c2ea14fd59085e0b7a17c67ec320f23b63410726f53a523c78edcb5c8fdc857ed1d5c2a9de570976d5573ef77de1f851a0c509e508258390138bba676633b25db054b4705d576ee1cf81dbd1c15666acc4f58edf7a11222873bbe7b0250f482b5a6a361181b398b7bba985b7aa8046bda1a0d8bdd70825839010f34fe3191fc19946b657ddad79c6cba4fbf6f3c5be68eaa76ac19378690d68fd06954f5b6f69f28585394616b24cb046797f6931b878fcd1a0dcc1ac082583901b58c09f5a93b478b77210e81fba0dc3171ee75275e1a2bc72f71f766691a163b8b1fef714fd8604edc1c3e9532134883668d6044cd588aba1a015e0960825839018c663f342c7e37de4e178b6a72435a2ca3c8defe8e59893a5fccb30d6575c9183f86d85907cffa74605891c36b0d83c5c1d163a23f69c0971a0ded1040825839018c663f342c7e37de4e178b6a72435a2ca3c8defe8e59893a5fccb30d6575c9183f86d85907cffa74605891c36b0d83c5c1d163a23f69c0971a004c4b408258390127943d4923eecbf8e2e44fde676ebb1395431ef71d24368ae4bbe1d7b3097e4731e2263ab175820b5b7ce79166c1afb1e230018ca37e38091a0ded10408258390107823056b202bda49afeeba405bdb8dcae3b2a2d5c6db453dba2a1dd7ae4910d818d27a16a3d4b450bfcade07bf1d9d0992cd1148858174f1a0bebc20082583901881c18e58926f8a0a4e0443e4fd3a3f77634b0aded1888d81cdb4ed1436426dc4f2125e50ef00ac160160c460a2f75fa2c614fb759c8f4181a06e60d608258390199fca72b5ce96bd5d5cecbc85e979abfd7bc929b26cda6fafaa8a4e47358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0dca434682583901b6563dbdc671edff4dedd847410c3ce35aeb0528014e7098c8b388bc7358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0c9df95482583901903eed3376b63941b3195efbb8eaee3554928fcbc6250ad26bd159ef7358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0c339bee8258390103dbaffd44bb56eddc84aba9b4d3647bf9fb8118c3d47ee327c205f67358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0c339bee82583901f5269e8d5266b46e6ed1e446771e050ab61defc26720ee7926bf6fe87358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0d6a6dda82583901f1256b96127b2b5aa90ea7ee4038c4cc3b8c1c2e60c42a152bcbd4db7358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0da9c43f825839015963b6413f0929be98ee0f2c864956a9591f8d897dc42d90fd1c395d7358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0168614882583901a47054d66a9a63feb7b0acf64fb1dd91528e48363340975e5e4a90fc7358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0dca434682583901ee067106d922da2a6c6df9d356ab0a04fc62053270d0ebd9d88c04d07358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a005a7b2a8258390140c647979432c0d13b3586f0559156157149361b42b492322299c7cd7358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0dca4346825839015be1cb60a8535828dfdcf9d532e6052918d65c3ec11aa3a03dfebd3e7358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a0bd02a2d82583901b1e340be62c7a6b054cb0f8ffc15702babbb0d46b5e679b297d609437358ddc5a287d8f7bf98a4ac988aef0f8ab3bd288f752d3f6454284c1a06dc8335"
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
            Bytes.fromHexUnchecked "84a70082825820f6e81bb9da6b4d635f3d774c7b7a58813a47b899bb52fef34caa4250fa8aa26100825820f6e81bb9da6b4d635f3d774c7b7a58813a47b899bb52fef34caa4250fa8aa261010d81825820f6e81bb9da6b4d635f3d774c7b7a58813a47b899bb52fef34caa4250fa8aa261000182a200581d610a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07011a02e94d2ba300581d716a9391d6aa51af28dd876ebb5565b69d1e83e5ac7861506bd29b56b001821a002dc6c0a1581c8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338da1434d3442030282005820605635647b7fa1f331978bdf5aeed389bdb3369274482ead60c6b86c389cf020021a00037c9d0e81581c0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e070b582074dc5b362d2bf0c2e1d9c3d1e9e4e5f21539a2095323880b8d0ca86d9d3b7af707582091ebd602815a977fff9028bbebbe7bfb7f8ae703c65a684e5c85f380f5249e24a20081825820669ed15b1bc5e97ec45af8951e9cbcbd33a3b5878943704d054a1a3ec46be2825840b18cd30d60e5e8635a3fbaf118b1caef5f8d781e385be23a1d271a0a3680fbcdc39fe9c26614ad4b84547023c19fa5b4ad5a4fd7d65471cb6b0e7de9a8bb8d0d0481d8799fd8799f40ffd8799fa2d8799fd8799fd87a80d8799fd8799f581c0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07ffd87a80ffffd8799f4040ffff1a002dc6c0d8799fd8799fd87a80d8799fd8799f581c0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07ffd87a80ffffd8799f581c8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d434d3442ffff03a0a000ffd87c9f9fd8799fd87b9fd87d9fd9050180d87a9f1b000001836721a9f8ffffffd87a9fd8799fd87a80d8799fd8799f581c0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07ffd87a80ffffd87a9fd8799fd87a80d8799fd8799f581c9205b71f3561fd659782172c34517bc859e3ea6312bace7f7143e570ffd8799fd8799fd8799f581c009dda942712dd276cad4aef90709b214d6b5e331631b2cdfea745cbffffffffffffd8799f581c8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d434d3442ffd87a9f01ffd87a9fd8799fd87a80d8799fd8799f581c0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07ffd87a80ffffd87a9fd8799fd87a80d8799fd8799f581ce19d61fddd5d2484c87370b2a97c44b2d4f012f7859c2d665ae54d29ffd8799fd8799fd8799f581cec6cd8fff4cfddf7bef15a985c0457cd1b5c0b71ef850efabcb26391ffffffffffffd8799f581c8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d434d3442ffd87a9f01ffd87a9fd8799fd87a80d8799fd8799f581c0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07ffd87a80ffffd87a9fd8799fd87a80d8799fd8799f581c2bbbdfab8c28a5703f472f6b28c61cdd066a99790d62cc839d2917daffd8799fd8799fd8799f581c0cff2d99c693c4a7c787995a69262d796d8597a9b4b24e57d81cbe9fffffffffffffd8799f581c8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d434d3442ffd87a9f01ffd87980ffffffffff1b00000183b4610df8d87980fffff5d90103a100a11902a2a1636d736781782557656c636f6d652c204d61726c6f77652c20746f2074686520426162626167652045726121"
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
    , plutusV3Scripts = []
    }


body9c91bdbb : TransactionBody
body9c91bdbb =
    { newBody
        | auxiliaryDataHash = Just (Bytes.fromHexUnchecked "91ebd602815a977fff9028bbebbe7bfb7f8ae703c65a684e5c85f380f5249e24")
        , collateral = [ { outputIndex = 0, transactionId = Bytes.fromHexUnchecked "f6e81bb9da6b4d635f3d774c7b7a58813a47b899bb52fef34caa4250fa8aa261" } ]
        , fee = N.fromSafeInt 228509
        , inputs =
            [ { outputIndex = 0, transactionId = Bytes.fromHexUnchecked "f6e81bb9da6b4d635f3d774c7b7a58813a47b899bb52fef34caa4250fa8aa261" }
            , { outputIndex = 1, transactionId = Bytes.fromHexUnchecked "f6e81bb9da6b4d635f3d774c7b7a58813a47b899bb52fef34caa4250fa8aa261" }
            ]
        , outputs =
            [ { address = Address.Shelley { networkId = Mainnet, paymentCredential = Address.VKeyHash (Bytes.fromHexUnchecked "0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07"), stakeCredential = Nothing }
              , amount = Value.onlyLovelace (N.fromSafeInt 48844075)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            , { address = Address.Shelley { networkId = Mainnet, paymentCredential = ScriptHash (Bytes.fromHexUnchecked "6a9391d6aa51af28dd876ebb5565b69d1e83e5ac7861506bd29b56b0"), stakeCredential = Nothing }
              , amount = { assets = bytesMap (Dict.fromList [ ( "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d", bytesMap (Dict.fromList [ ( "4d3442", N.fromSafeInt 3 ) ]) ) ]), lovelace = N.fromSafeInt 3000000 }
              , datumOption = Just (DatumHash (Bytes.fromHexUnchecked "605635647b7fa1f331978bdf5aeed389bdb3369274482ead60c6b86c389cf020"))
              , referenceScript = Nothing
              }
            ]
        , requiredSigners = [ Bytes.fromHexUnchecked "0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07" ]
        , scriptDataHash = Just (Bytes.fromHexUnchecked "74dc5b362d2bf0c2e1d9c3d1e9e4e5f21539a2095323880b8d0ca86d9d3b7af7")
    }


witnessSet9c91bdbb : WitnessSet
witnessSet9c91bdbb =
    { newWitnessSet
        | plutusData =
            Just
                [ Data.Constr (N.fromSafeInt 0)
                    [ Data.Constr (N.fromSafeInt 0) [ Data.Bytes (Bytes.fromHexUnchecked "") ]
                    , Data.Constr (N.fromSafeInt 0)
                        [ Data.Map
                            [ ( Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 1) [], Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 0) [ Data.Bytes (Bytes.fromHexUnchecked "0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07") ], Data.Constr (N.fromSafeInt 1) [] ] ], Data.Constr (N.fromSafeInt 0) [ Data.Bytes (Bytes.fromHexUnchecked ""), Data.Bytes (Bytes.fromHexUnchecked "") ] ], Data.Int (Integer.fromNatural (N.fromSafeInt 3000000)) )
                            , ( Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 1) [], Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 0) [ Data.Bytes (Bytes.fromHexUnchecked "0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07") ], Data.Constr (N.fromSafeInt 1) [] ] ], Data.Constr (N.fromSafeInt 0) [ Data.Bytes (Bytes.fromHexUnchecked "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d"), Data.Bytes (Bytes.fromHexUnchecked "4d3442") ] ], Data.Int (Integer.fromNatural (N.fromSafeInt 3)) )
                            ]
                        , Data.Map []
                        , Data.Map []
                        , Data.Int (Integer.fromSafeInt 0)
                        ]
                    , Data.Constr (N.fromSafeInt 3)
                        [ Data.List
                            [ Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 2) [ Data.Constr (N.fromSafeInt 4) [ Data.Constr (N.fromSafeInt 8) [], Data.Constr (N.fromSafeInt 1) [ Data.Int (Integer.fromNatural (bigNat [ 52537848, 24793 ])) ] ] ], Data.Constr (N.fromSafeInt 1) [ Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 1) [], Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 0) [ Data.Bytes (Bytes.fromHexUnchecked "0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07") ], Data.Constr (N.fromSafeInt 1) [] ] ], Data.Constr (N.fromSafeInt 1) [ Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 1) [], Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 0) [ Data.Bytes (Bytes.fromHexUnchecked "9205b71f3561fd659782172c34517bc859e3ea6312bace7f7143e570") ], Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 0) [ Data.Bytes (Bytes.fromHexUnchecked "009dda942712dd276cad4aef90709b214d6b5e331631b2cdfea745cb") ] ] ] ] ] ], Data.Constr (N.fromSafeInt 0) [ Data.Bytes (Bytes.fromHexUnchecked "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d"), Data.Bytes (Bytes.fromHexUnchecked "4d3442") ], Data.Constr (N.fromSafeInt 1) [ Data.Int (Integer.fromNatural (N.fromSafeInt 1)) ], Data.Constr (N.fromSafeInt 1) [ Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 1) [], Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 0) [ Data.Bytes (Bytes.fromHexUnchecked "0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07") ], Data.Constr (N.fromSafeInt 1) [] ] ], Data.Constr (N.fromSafeInt 1) [ Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 1) [], Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 0) [ Data.Bytes (Bytes.fromHexUnchecked "e19d61fddd5d2484c87370b2a97c44b2d4f012f7859c2d665ae54d29") ], Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 0) [ Data.Bytes (Bytes.fromHexUnchecked "ec6cd8fff4cfddf7bef15a985c0457cd1b5c0b71ef850efabcb26391") ] ] ] ] ] ], Data.Constr (N.fromSafeInt 0) [ Data.Bytes (Bytes.fromHexUnchecked "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d"), Data.Bytes (Bytes.fromHexUnchecked "4d3442") ], Data.Constr (N.fromSafeInt 1) [ Data.Int (Integer.fromNatural (N.fromSafeInt 1)) ], Data.Constr (N.fromSafeInt 1) [ Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 1) [], Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 0) [ Data.Bytes (Bytes.fromHexUnchecked "0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07") ], Data.Constr (N.fromSafeInt 1) [] ] ], Data.Constr (N.fromSafeInt 1) [ Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 1) [], Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 0) [ Data.Bytes (Bytes.fromHexUnchecked "2bbbdfab8c28a5703f472f6b28c61cdd066a99790d62cc839d2917da") ], Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 0) [ Data.Constr (N.fromSafeInt 0) [ Data.Bytes (Bytes.fromHexUnchecked "0cff2d99c693c4a7c787995a69262d796d8597a9b4b24e57d81cbe9f") ] ] ] ] ] ], Data.Constr (N.fromSafeInt 0) [ Data.Bytes (Bytes.fromHexUnchecked "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d"), Data.Bytes (Bytes.fromHexUnchecked "4d3442") ], Data.Constr (N.fromSafeInt 1) [ Data.Int (Integer.fromNatural (N.fromSafeInt 1)) ], Data.Constr (N.fromSafeInt 0) [] ] ] ] ]
                            ]
                        , Data.Int (Integer.fromNatural (bigNat [ 6360568, 24813 ]))
                        , Data.Constr (N.fromSafeInt 0) []
                        ]
                    ]
                ]
        , vkeywitness =
            Just
                [ { signature = Bytes.fromHexUnchecked "b18cd30d60e5e8635a3fbaf118b1caef5f8d781e385be23a1d271a0a3680fbcdc39fe9c26614ad4b84547023c19fa5b4ad5a4fd7d65471cb6b0e7de9a8bb8d0d"
                  , vkey = Bytes.fromHexUnchecked "669ed15b1bc5e97ec45af8951e9cbcbd33a3b5878943704d054a1a3ec46be282"
                  }
                ]
    }


{-| Next Babbage failure.

Tx id: fd83f4f99da99c9ff7ac6b01b46434a234d09308eaba547e98ba1e6e2a99a4ec
Block height: 7810979
Previous block intersection:

  - slot: 72711393
  - id: 98d1ca7c8a53e6e900d05cf72d4844cc9d584f5c01ce4436ba3c10e61c3d9d3b

-}
decodefd83f4f9 : Test
decodefd83f4f9 =
    test "Tx id fd83f4f99da99c9ff7ac6b01b46434a234d09308eaba547e98ba1e6e2a99a4ec" <|
        \_ ->
            Bytes.fromHexUnchecked "84a30083825820a03c1f56290b2100a8cb3a8035db746d0cbc961298679d2a599e18946eddbf7501825820c919cd7e67b99b0c4da2dfff5db7816fcbe8524b419b6de06a4d68112d32015401825820c0296962baec6bb9b8d605bbfdb351d93af4ae4848331ac9c2db95ddedc72424010183a300581d7190ba520250f3b4f416f55eb384650f05fc3a771aa2e5a5caac67570b01821a001e8480a1581c653aae5e966b9ad14448446ff177bab27ede6587e03fa926d2e7286fa1546a7572617373696b636861696e6564303830333201028201d818586ed8799f581c1c471b31ea0b04c652bd8f76b239aea5f57139bdc5a2b28ab1e69175581cfd3a6bfce30d7744ac55e9cf9146d8a2a04ec7fb2ce2ee6986260653192710581cfee8e1c842ac273e38c4d30caa08855a93b99a041cb566ba4d44bb6a464d4554454f521a07ed6b4001ff825839011c471b31ea0b04c652bd8f76b239aea5f57139bdc5a2b28ab1e69175fd3a6bfce30d7744ac55e9cf9146d8a2a04ec7fb2ce2ee6986260653821a014fdd22b845581c11ff0e0d9ad037d18e3ed575cd35a0513b8473f83008124db89f1d8fa5504861707079486f70706572303031373801504861707079486f70706572303038313001504861707079486f70706572303233323601504861707079486f70706572303332333301504861707079486f70706572303931363301581c1dd1a7dde0e1e82761325ee5f4719d0d4b7c24dfba77d9bee01eed4ba1444d454f571a00692a46581c23a000a839f04bb71b59b4ae74bf8cf5ed21ee0be520690e665ae4b4a45818457175696e6550696f6e6565724a6f636b65793031343138015818457175696e6550696f6e6565724a6f636b65793031343139015818457175696e6550696f6e6565724a6f636b65793031343230015818457175696e6550696f6e6565724a6f636b6579303134323101581c28a9ffc30a5f4e18315fc6c5d192c349efb5b259bccb40b703999de9a14f44656570566973696f6e303739393601581c29d222ce763455e3d7a09a665ce554f00ac89d2e99a1a83d267170c6a1434d494e1aa463b239581c2a54d0a4ff765ad06a84440494b92419e769b17898415341f3b54675a65347616d696e6743617264616e6f564950313937015347616d696e6743617264616e6f564950333034015347616d696e6743617264616e6f564950333136015347616d696e6743617264616e6f564950353239015347616d696e6743617264616e6f564950353530015347616d696e6743617264616e6f56495035383701581c2a89138bffea582b621a747015c1c90259d6b4751eeccaa39c4a7dfba149467572696e3032393601581c2bc31b7881acbd6558b680d42248e201d9c7a2931e278b95e96daab8a2484455534b3038323501484455534b3038343901581c2d8ea4383f7c2cfdd1f7fa1b9d3cf24ea10ccb2ab703febd3481e557a648446f646f30303133014854526578546573740149436f6d707954657374014a4d616d6d757454657374054d4d6567616c6f646f6e54657374035042726f6e746f7361757275735465737402581c4247d5091db82330100904963ab8d0850976c80d3f1b927e052e07bda146546f6b68756e01581c43206de9e07fbd36ce6c109b3d34637727233c58a0b38f1da00a9ccfa35050616e6461536f636965747930333133015050616e6461536f636965747931363230015050616e6461536f63696574793332363101581c445209a2fd0c4ef94c7e1bb19b22e24c0f24d16355abe62620667798a2514152776169667573696c7665723030303401581d63617264616e6f77616966756d696b616b6f7370656369616c3030303701581c4b9c44c53a20cdfaec04f6a24dd6572037a54aed27a4647b400255f3a14b48617264466f726b31303401581c4d007b4084d7976049d57aa59008942efc93141a43191c4c6e3012cfa24e4465617468536861706573303431014e446561746853686170657331363801581c4d192b72800649271a7518477333f541facd9b955485600029857ffea15553485f4d6f64756c655f53454747435f315f34353601581c4f15bc224e187a2e7774815b5d632600e62c9b3a570da6eb5e123359a1564e4654553030323043617264616e6f4275647a31373701581c4fd6d8b1eac12b029cd68acd25220c20dd13b9c4b0d0b585d49135d3a144534143541864581c501a31b491707e0985322a6141e5b2af607588661e63ad47b2faed5fa351556e6465727468655472656531303630390151556e6465727468655472656531303738360151556e64657274686554726565313232303801581c571e017f4bc8178c929ab97c4892b71f2ead02e38fc1cf48b58ff93fa24a426c7565426f78313938014b426c7565426f783131323301581c5a80b892e8c606e20dc6603887c4334a3490332015af9a84c77f12d5a157596178496e5370616365436f6c6f6e794c6f737430303301581c5ad8deb64bfec21ad2d96e1270b5873d0c4d0f231b928b4c39eb2435a14661646f7369611a017d7840581c61ac9a39454cd573de2206ef362cfe8f72bff99b12cd2b79a4e10c2ea1524275646a61506c6174696e756d303030383601581c63ebfba8115aff54cb3e066d2ac898c29aadad77892f0f88d26b6778a544546573740144746573740145746573743201487465737420232033014c74686520776973652065676701581c64036a8c21866b47d4597657151e1490a7c225caef4715b37b9eca38a1443934383801581c6ae8d99e095a01522591a76fd42ac6ed406bd2d58cd8e504e72daf18b049537469676d61363938014a537469676d6131303235014a537469676d6131383835014a537469676d6131383938014a537469676d6131393038014a537469676d6132303234014a537469676d6132353933014a537469676d6132373930014a537469676d6133303435014a537469676d6133323436014a537469676d6133343030014a537469676d6133353731014a537469676d6133363839014a537469676d6133373031014a537469676d6134393133014a537469676d613439383201581c6b653e5f6ea5d8b7dd2365d7a1e3114460f50f7f6df32c1c9bca9431a14a73656e7365693335313001581c6bc0188b5b0857d5996d43f0e92653419aa912b1b7bf5f52b8b5a7f6a45043617264616e6f4275647a4230303033015143617264616e6f4275647a464230303033015243617264616e6f4275647a55495330303033015343617264616e6f4275647a464c4c563030303301581c6cfbfedd8c8ea23d264f5ae3ef039217100c210bb66de8711f21c903a144434e46541a000597f8581c729e0d019be399a241197a31c6777d6983e89ad6970e9c5ab5c5ff4da15541424853785061706572536f63696574794330353201581c73493b13a86620898c9da638724fb085883ce91db1e74aa015fdc91da240014f612072616e646f6d2074726578212301581c74ec3a131095f2ef0cfa886cef3694974ea5a5b7d71829d249aa7be1a158184375746548616c6c6f7765656e437265617475726573313101581c7967f354b44b274c7b8ebbdf432aebe7a12e8ed743238930a9a1c812a1581853485f53746172626173655f57616e64657265725f34323301581c7eb7cf060a9740b63de68afb1c0fde7f74f2895c981573259046ae3fa14f4c696f6e4c6567656e64733532313401581c82ee6b6e82aac09ecc96744c879e87403629df056b8b0be8d7c75956a14677696e74657201581c837f24191e901d486df3783af8da8add0eeeb6f268e2ad2cee16d4fea2466d696178693301466d696178693401581c862cd06c4504de6114a29e0b863751ee84ad455493d43aeeb727d896a24d4465786f576f726c6431373931014d4465786f576f726c643639323201581c86d318effcb8b09f4a8c7c4bddb70b9745d9772b35235853f0104624a54b5368617065733032343233014b5368617065733037353438014b5368617065733231383238014b5368617065733238343330014b536861706573333230343101581c884892bcdc360bcef87d6b3f806e7f9cd5ac30d999d49970e7a903aea14550415649411917ad581c8a1cfae21368b8bebbbed9800fec304e95cce39a2a57dc35e2e3ebaaa1444d494c4b1864581c8e0829598ee023424bf6a8e178c2f10c5e51f0fbc8316eacdbd42af0a15143617264616e6f4275647a4d617934746801581c8f80ebfaf62a8c33ae2adf047572604c74db8bc1daba2b43f9a65635a45243617264616e6f57617272696f7231363737015243617264616e6f57617272696f7234313332015243617264616e6f57617272696f7234353838015243617264616e6f57617272696f723635303801581c91acca0a2614212d68a5ae7313c85962849994aab54e340d3a68aabba14653483331393101581c95c248e17f0fc35be4d2a7d186a84cdcda5b99d7ad2799ebe98a9865a34861776f6b656e3633014b61776f6b656e3134343236014b61776f6b656e313434323701581c97305ec3684b4e5ac2977d44ee05fd453b038ae882e665fd6499484ca55818456e70696d6f6e7943616e61646154656d706c6172303134015819456e70696d6f6e79436f6c6f6d62696141726368657230313201581b456e70696d6f6e794e65775a65616c616e644b6e6967687430313501581b456e70696d6f6e794e65775a65616c616e644b6e6967687430313601581c456e70696d6f6e794e65775a65616c616e6454656d706c617230313901581c973adfc28b03fb28a8dda1be23f186b720b248d46490fda2d9ae59a5a84a68617070707930303037014a68617070707930303135014a68617070707930303938014a68617070707930303939014a68617070707930313636014a68617070707930333332014a68617070707930343239014a6861707070793037313701581c9997c9e9e6d0c2f516a2f2229fa39bff9c30b729e3e02679a422d6eca24c6b6964796f6b616931303239014c6b6964796f6b61693234373101581c9a9693a9a37912a5097918f97918d15240c92ab729a0b7c4aa144d77a14653554e4441451a4923282a581c9ce2253eca3c730d2f2c29cbaa90806ab006e08a6d8d7604bf3f74e6a15063727970746f7472616974733030303501581c9d4c40c114d3d69d4f8209e205686db296683073c0ca5c63a8d2456ea24b4c696f6e65737334313633014b4c696f6e6573733431383001581c9def2b8de3736fdbd611109878e3ac44f46c352ae51961ea34f0d296a15246616861646f756b656e31304b763235323601581c9fd1333b6fa38ceb957b089398a2dc32727cf0bd4e60bf43c308609ba1534d6f624d6567616d69416b756e696e3238313001581ca0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235a145484f534b591a036bbd41581ca1c1e3811c5adb591e026164a519eaf3d2708ee02e5712f891f80b78a14346414219249f581ca8830ba155386c9837f7fd86ae8917265747fdb55a440634f058639da24b4c696f6e43756230303536014b4c696f6e4375623032303201581caccaf1783286811bba4124ab328b70742ed5e286b284d3e18c4e29f4a149656c7261756c69746f01581caf2e27f580f7f08e93190a81f72462f153026d06450924726645891ba144445249501a3b9aca00581caf4bdecba25c009b570e2368f7c8f3459f5d57d46bd7f01afd4dcaf5aa4b726f6c6567677330303031014b726f6c6567677330303032014b726f6c6567677330303033014b726f6c6567677330303034014b726f6c6567677330303035014b726f6c6567677330303036014b726f6c6567677330303037014b726f6c6567677330303038014b726f6c6567677330303039014b726f6c656767733030313001581cafc910d7a306d20c12903979d4935ae4307241d03245743548e76783a14541534849421a3b9aca00581cb001076b34a87e7d48ec46703a6f50f93289582ad9bdbeff7f1e3295a94e4879706562656173747330313336014e4879706562656173747330343835014e4879706562656173747331363039014e4879706562656173747331373737014e4879706562656173747331383939014e4879706562656173747333373336014e4879706562656173747333383238014e4879706562656173747334333039014e487970656265617374733436303301581cb034bacc10e558667ff2aedc33804a42d3f142c324acafb6f16ce24da3456268313430014562683231340145626834343201581cb211283237d50e5fae22fef8dda898297a02ebe8d36be51f17ff3391a14f6d616e676132636f7665723030313001581cb24a29b9c16d349df16d9b5553b119e399e46ae19d6150c1a843ef61a1466469646974731a02faf080581cb43131f2c82825ee3d81705de0896c611f35ed38e48e33a3bdf298dca14f43727970746f4d616765303539313601581cb90f090c86148b8a703326bb2b2c3b6d7c222a1fad2355a3dac6ac39a15263617264616e6f667269656e64733030393701581cb97919795280e20e60b8b3f222ba9e42f101e10d03ca4d29586f005ca15243617264616e6f4275647a4831533133303301581cbcf9ce45e986b8f57fbcece6ab456c39adca4ebdc2dd9113c5574b1fa1581a43617264616e6f4361744279726f6e476c61737365733234313301581cc089a5eeebb1d8b3e67595b934fc028ce5f11b4cf18858adfef31186a146535455524e491a05f5e100581cc96215c844a4dc49075c29c003a025f493e1ba56e75b3d10161e1a14a25563617264616e6f7761696675733030313174657374015563617264616e6f776169667573303031387465737401581cc9f082d0e2e1c46d7e43dc2633b91054841001ee618004de22f66a8ea858203037383964393634393833383264326263663730363265333030633830616133015820316132346362396131626263613138623631613835393461616137336335313401582032616633313038313039333162353739396166373931376461383830613861310158203565633665373561396264333839323031633462393237616336346132326632015820383365386535326139643933343038323362656664393965646666393530316201582061643066326463636434646663653464613331643064383564323231663263660158206266316264346435643261633332616336303330356334656233633632633838015820663036643934663664663936353562313665653639383863383439613663656101825839011c471b31ea0b04c652bd8f76b239aea5f57139bdc5a2b28ab1e69175fd3a6bfce30d7744ac55e9cf9146d8a2a04ec7fb2ce2ee69862606531a00817705021a00060831a10081825820041a1295ee7145e95a946526320452a779b4d35975a7d0b37b467e21714de0375840335dc9ac3489f5bf3202fcb8b8926c9815a9118f21c51ce1d06c52e460361fcb52ada93eda605f74160375cf28003e84e852c6155366d452d4ae31718b77dc06f5f6"
                |> Transaction.deserialize
                |> Expect.notEqual Nothing


decodeOutputfd83f4f9 : Test
decodeOutputfd83f4f9 =
    test "First output in Tx id fd83f4f99da99c9ff7ac6b01b46434a234d09308eaba547e98ba1e6e2a99a4ec" <|
        \_ ->
            Bytes.fromHexUnchecked "a300581d7190ba520250f3b4f416f55eb384650f05fc3a771aa2e5a5caac67570b01821a001e8480a1581c653aae5e966b9ad14448446ff177bab27ede6587e03fa926d2e7286fa1546a7572617373696b636861696e6564303830333201028201d818586ed8799f581c1c471b31ea0b04c652bd8f76b239aea5f57139bdc5a2b28ab1e69175581cfd3a6bfce30d7744ac55e9cf9146d8a2a04ec7fb2ce2ee6986260653192710581cfee8e1c842ac273e38c4d30caa08855a93b99a041cb566ba4d44bb6a464d4554454f521a07ed6b4001ff"
                |> Bytes.toBytes
                |> D.decode Utxo.decodeOutput
                |> Expect.notEqual Nothing


{-| First Conway failure.

Tx id: ef45fe8e90c02ca628d48aff26b7da66ae98ab15b898cb791642b28eb923d70d
Block height: 10781333
Previous block intersection:

  - slot: 133661020
  - id: d32af8cb24a8ee9ba33a00656d39e8204004229421ab62ea6c09487652e24f4f

-}
decodeef45fe8e : Test
decodeef45fe8e =
    test "Tx id ef45fe8e90c02ca628d48aff26b7da66ae98ab15b898cb791642b28eb923d70d" <|
        \_ ->
            Bytes.fromHexUnchecked "84a400d901028182582088e54d6ae3c2c6e456d42c6b9aea6b3f8ebc31cd2b65ac273536e7eaa144d3ce000181825839013141f3d912674ec64ae07c2a80e6ad8f1a162806401c729f9ad6bdf33b7fdf80fb70788ff9aa347893bf58bf0e923c9d258f1db7dc8ff5c31a00895440021a000f424004d901028184108200581c7bdef7aaf3c925e97ca42d36f119b0469a12cca4a17ecfefc69003501a1dcd6500f6a100d90102828258206027b9f5c62941dcd2727779b0a466e5214f34fe889f058d0745320ee4bdb2f65840000781b3b8e443071629727acedf460ace97963c2138cff2990ec9b820ff740669895e3d41d4e9691262fc1ba51d78a567b26de5881646c3c8bf5c16132b450a825820bae620564f80ef33ab7b7e4ebc0a371f3ca42e8c6c1da0184986169b1d6876d35840c8e1115882ede8737dedccd05d5ff4a0735d63adb081844cf32fcf11ace97e50883ef63d6256fa6dafe26894741de65754dd0cb8f958d784694f1b8e908de40df5f6"
                |> Transaction.deserialize
                |> Expect.equal
                    (Just
                        { body = bodyef45fe8e
                        , witnessSet = witnessSetef45fe8e
                        , isValid = True
                        , auxiliaryData = Nothing
                        }
                    )


bodyef45fe8e : TransactionBody
bodyef45fe8e =
    { newBody
        | certificates =
            [ RegDrepCert
                { anchor = Nothing
                , deposit = bigNat [ 30237952, 7 ]
                , drepCredential = Address.VKeyHash (Bytes.fromHexUnchecked "7bdef7aaf3c925e97ca42d36f119b0469a12cca4a17ecfefc6900350")
                }
            ]
        , fee = N.fromSafeInt 1000000
        , inputs = [ { outputIndex = 0, transactionId = Bytes.fromHexUnchecked "88e54d6ae3c2c6e456d42c6b9aea6b3f8ebc31cd2b65ac273536e7eaa144d3ce" } ]
        , outputs =
            [ { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromHexUnchecked "3141f3d912674ec64ae07c2a80e6ad8f1a162806401c729f9ad6bdf3")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromHexUnchecked "3b7fdf80fb70788ff9aa347893bf58bf0e923c9d258f1db7dc8ff5c3")))
                        }
              , amount = Value.onlyLovelace (N.fromSafeInt 9000000)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            ]
    }


witnessSetef45fe8e : WitnessSet
witnessSetef45fe8e =
    { newWitnessSet
        | vkeywitness =
            Just
                [ { signature = Bytes.fromHexUnchecked "000781b3b8e443071629727acedf460ace97963c2138cff2990ec9b820ff740669895e3d41d4e9691262fc1ba51d78a567b26de5881646c3c8bf5c16132b450a"
                  , vkey = Bytes.fromHexUnchecked "6027b9f5c62941dcd2727779b0a466e5214f34fe889f058d0745320ee4bdb2f6"
                  }
                , { signature = Bytes.fromHexUnchecked "c8e1115882ede8737dedccd05d5ff4a0735d63adb081844cf32fcf11ace97e50883ef63d6256fa6dafe26894741de65754dd0cb8f958d784694f1b8e908de40d"
                  , vkey = Bytes.fromHexUnchecked "bae620564f80ef33ab7b7e4ebc0a371f3ca42e8c6c1da0184986169b1d6876d3"
                  }
                ]
    }


{-| Next Conway failure.

Tx id: 4385b3d89b8e900d09458d327d29b7cf431f8d241aa4c843039addb8d162e5a1
Block height: 10781335
Previous block intersection:

  - slot: 133661077
  - id: f32f4445024a9dbb008fdfe31f2fef1a5c87e3e471b39ba17cf169c7dc746c3e

-}
decode4385b3d8 : Test
decode4385b3d8 =
    test "Tx id 4385b3d89b8e900d09458d327d29b7cf431f8d241aa4c843039addb8d162e5a1" <|
        \_ ->
            Bytes.fromHexUnchecked "84a600828258208b70ecda03d3340232f71bad2411c5bb55b1b9b9ee3942db9d85b3ac327809ee00825820e89a66d40e3980117b246676f3de15f8f5c6dce901513b9c220aa8c16fc34ff800018182583901fbce7abc483c808d29c1e1d69dd6609b5722918e4784ac1264f16621759077966225f21836cdf733f74527aeae00bb08727fb43bbc63349b1a0046b0d2021a0002e60d031a07f7abb3048284108200581c6c8a0c80777f9e4dc043dabc48e8391c7821a5b42c850be268cf1a511a1dcd650082784d68747470733a2f2f632d697066732d67772e6e6d6b722e696f2f697066732f516d50337367435a59346a745552726d5147645576573531784d3471646f71324a7544627177666f4469584167505820451db8e93164694527f3ae06ffc11c8da058ea0690e4ccaaa825189707cac60883098200581c759077966225f21836cdf733f74527aeae00bb08727fb43bbc63349b8200581c6c8a0c80777f9e4dc043dabc48e8391c7821a5b42c850be268cf1a510800a10083825820059d3aae46ac0b78296b3a389101bbcc67eec6daa01b3203100f51fd3460e5a1584095f0edb1921d5ef34d231cb2b0b0c5693d4600c15cc2aed3041dd1a40be7e2f32734eb055ab9cefc9bf5d1657fb6ac7643f97bbffbaf2f4f9b4dc9badce163078258207a601539db47a521bc7902f23004f425e99f9af0fe2c6f97337a1935a537fbbe58409966cda9583216e9bfa8773f83d56289d89b10677c1fcefcb66828c1c9ab403fc212b0324a07041f89a3d572df5d4c1bd68e944e6d73ea35ed4183713a3b8606825820da32bb32751537ed37855297094dc0c051267967329084e66047938139eee82e5840a625d5aada40174a8d71f918476eb4c2489ceee943c337ab69fab157a007803c1799b860a964d4f602d07ac085d511a0d0daf68c55c99b0206739386ab70350bf5f6"
                |> Transaction.deserialize
                |> Expect.equal
                    (Just
                        { body = body4385b3d8
                        , witnessSet = witnessSet4385b3d8
                        , isValid = True
                        , auxiliaryData = Nothing
                        }
                    )


body4385b3d8 : TransactionBody
body4385b3d8 =
    { newBody
        | certificates =
            [ RegDrepCert
                { anchor = Just { dataHash = Bytes.fromHexUnchecked "451db8e93164694527f3ae06ffc11c8da058ea0690e4ccaaa825189707cac608", url = "https://c-ipfs-gw.nmkr.io/ipfs/QmP3sgCZY4jtURrmQGdUvW51xM4qdoq2JuDbqwfoDiXAgP" }
                , deposit = bigNat [ 30237952, 7 ]
                , drepCredential = Address.VKeyHash (Bytes.fromHexUnchecked "6c8a0c80777f9e4dc043dabc48e8391c7821a5b42c850be268cf1a51")
                }
            , VoteDelegCert
                { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "759077966225f21836cdf733f74527aeae00bb08727fb43bbc63349b")
                , drep = DrepCredential (Address.VKeyHash (Bytes.fromHexUnchecked "6c8a0c80777f9e4dc043dabc48e8391c7821a5b42c850be268cf1a51"))
                }
            ]
        , fee = N.fromSafeInt 189965
        , inputs =
            [ { outputIndex = 0
              , transactionId = Bytes.fromHexUnchecked "8b70ecda03d3340232f71bad2411c5bb55b1b9b9ee3942db9d85b3ac327809ee"
              }
            , { outputIndex = 0
              , transactionId = Bytes.fromHexUnchecked "e89a66d40e3980117b246676f3de15f8f5c6dce901513b9c220aa8c16fc34ff8"
              }
            ]
        , outputs =
            [ { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromHexUnchecked "fbce7abc483c808d29c1e1d69dd6609b5722918e4784ac1264f16621")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromHexUnchecked "759077966225f21836cdf733f74527aeae00bb08727fb43bbc63349b")))
                        }
              , amount = Value.onlyLovelace (N.fromSafeInt 4632786)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            ]
        , ttl = Just (bigNat [ 66562995, 1 ])
        , validityIntervalStart = Just 0
    }


witnessSet4385b3d8 : WitnessSet
witnessSet4385b3d8 =
    { newWitnessSet
        | vkeywitness =
            Just
                [ { signature = Bytes.fromHexUnchecked "95f0edb1921d5ef34d231cb2b0b0c5693d4600c15cc2aed3041dd1a40be7e2f32734eb055ab9cefc9bf5d1657fb6ac7643f97bbffbaf2f4f9b4dc9badce16307"
                  , vkey = Bytes.fromHexUnchecked "059d3aae46ac0b78296b3a389101bbcc67eec6daa01b3203100f51fd3460e5a1"
                  }
                , { signature = Bytes.fromHexUnchecked "9966cda9583216e9bfa8773f83d56289d89b10677c1fcefcb66828c1c9ab403fc212b0324a07041f89a3d572df5d4c1bd68e944e6d73ea35ed4183713a3b8606"
                  , vkey = Bytes.fromHexUnchecked "7a601539db47a521bc7902f23004f425e99f9af0fe2c6f97337a1935a537fbbe"
                  }
                , { signature = Bytes.fromHexUnchecked "a625d5aada40174a8d71f918476eb4c2489ceee943c337ab69fab157a007803c1799b860a964d4f602d07ac085d511a0d0daf68c55c99b0206739386ab70350b"
                  , vkey = Bytes.fromHexUnchecked "da32bb32751537ed37855297094dc0c051267967329084e66047938139eee82e"
                  }
                ]
    }


{-| Next Conway failure.

Tx id: 1e5dd53bf3b3930fb3f3bbb7ea1f544dc82ca3986d8b4c2f90248f0f041d30f3
Block height: 10781357
Previous block intersection:

  - slot: 133661475
  - id: f16b6eb06e829d0529a95d03d373618e3ef6a0e933c2f18da7bc43dcfdf87c92

-}
decode1e5dd53b : Test
decode1e5dd53b =
    test "Tx id 1e5dd53bf3b3930fb3f3bbb7ea1f544dc82ca3986d8b4c2f90248f0f041d30f3" <|
        \_ ->
            Bytes.fromHexUnchecked "84a70081825820856073e491ed7b92134da94468bd4b68f971d338718241f98ca326f1782aef2901018182583901944831b82cfe62d8255294b6ae383afb983b0d720f25556662462a10ad686ce88581aa6fbb0d2d8e0e6e6216d5df6607ef909b48c8bb02fc1a9ac28304021a0002a515031a07f7ad42048183098200581cad686ce88581aa6fbb0d2d8e0e6e6216d5df6607ef909b48c8bb02fc810205a1581de1ad686ce88581aa6fbb0d2d8e0e6e6216d5df6607ef909b48c8bb02fc1a29467fac0800a10082825820cb473de65fc943326373fa47c35e99ece246e58fe8cdf325973e2224b8a8a7d25840d96ce06880b7bd41d195dbec2a36ac2011d074f84d22a25646c9232c49359ac9da12d5267ab7e98ae0c13b5ad665dfc9a4c352fe0c61b6195653831181927006825820e61a7b67c311ad9de99100803567641356e9a16b3a318e902feb3ff2518f38dd584094cb537a291d96030c00468d55e579924695beb99fea2039796b56cd84a1acec0de3dbdfc0b41ce9989ea2da43710eec42877ef9a22fc000b175db33fe41470cf5f6"
                |> Transaction.deserialize
                |> Expect.equal
                    (Just
                        { body = body1e5dd53b
                        , witnessSet = witnessSet1e5dd53b
                        , isValid = True
                        , auxiliaryData = Nothing
                        }
                    )


body1e5dd53b : TransactionBody
body1e5dd53b =
    { newBody
        | certificates =
            [ VoteDelegCert
                { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "ad686ce88581aa6fbb0d2d8e0e6e6216d5df6607ef909b48c8bb02fc")
                , drep = AlwaysAbstain
                }
            ]
        , fee = N.fromSafeInt 173333
        , inputs =
            [ { outputIndex = 1
              , transactionId = Bytes.fromHexUnchecked "856073e491ed7b92134da94468bd4b68f971d338718241f98ca326f1782aef29"
              }
            ]
        , outputs =
            [ { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromHexUnchecked "944831b82cfe62d8255294b6ae383afb983b0d720f25556662462a10")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromHexUnchecked "ad686ce88581aa6fbb0d2d8e0e6e6216d5df6607ef909b48c8bb02fc")))
                        }
              , amount = Value.onlyLovelace (bigNat [ 46301956, 38 ])
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            ]
        , ttl = Just (bigNat [ 66563394, 1 ])
        , validityIntervalStart = Just 0
        , withdrawals =
            [ ( { networkId = Mainnet
                , stakeCredential = Address.VKeyHash (Bytes.fromHexUnchecked "ad686ce88581aa6fbb0d2d8e0e6e6216d5df6607ef909b48c8bb02fc")
                }
              , bigNat [ 21397420, 10 ]
              )
            ]
    }


witnessSet1e5dd53b : WitnessSet
witnessSet1e5dd53b =
    { newWitnessSet
        | vkeywitness =
            Just
                [ { signature = Bytes.fromHexUnchecked "d96ce06880b7bd41d195dbec2a36ac2011d074f84d22a25646c9232c49359ac9da12d5267ab7e98ae0c13b5ad665dfc9a4c352fe0c61b6195653831181927006"
                  , vkey = Bytes.fromHexUnchecked "cb473de65fc943326373fa47c35e99ece246e58fe8cdf325973e2224b8a8a7d2"
                  }
                , { signature = Bytes.fromHexUnchecked "94cb537a291d96030c00468d55e579924695beb99fea2039796b56cd84a1acec0de3dbdfc0b41ce9989ea2da43710eec42877ef9a22fc000b175db33fe41470c"
                  , vkey = Bytes.fromHexUnchecked "e61a7b67c311ad9de99100803567641356e9a16b3a318e902feb3ff2518f38dd"
                  }
                ]
    }


{-| Next Conway failure.

Tx id: 15f82a365bdee483a4b03873a40d3829cc88c048ff3703e11bd01dd9e035c916
Block height: 10789298
Previous block intersection:

  - slot: 133826442
  - id: 3e48cc2f707bda35bd2cffd4b3624f4ac4b20334dc5c348ff7ac037ced9bc49f

-}
decode15f82a36 : Test
decode15f82a36 =
    test "Tx id 15f82a365bdee483a4b03873a40d3829cc88c048ff3703e11bd01dd9e035c916" <|
        \_ ->
            Bytes.fromHexUnchecked "84a400d9010281825820a584f292713ef96210dbcd377cb1fcc537f6f055c4f99df83cb0eb1e3079983d000181825839014dd37a5e94e082f9096e4e498f36686b58df4a6d8eeb0f918c854d6c45dee6ee5d7f631b6226d45f29da411c42fa7e816dc0948d31e0dba71a03e9befe021a0002af3914d9010281841b000000174876e800581de145dee6ee5d7f631b6226d45f29da411c42fa7e816dc0948d31e0dba78106827835697066733a2f2f516d576a6348737271396b4b485a5a37615050466a714e36774c75784839643862637173736d724537483463766258202f98f57c4149fdfed2b73cbd821226fe417ef5ed49d8f836a37b31edf14dea47a100d9010281825820a47afdef5fd0f70a7dc394fe540ddddd86cbaf28f2353acedbd9c6e6f71714db584004980038691b87592a7b66546b30f760bee4f778905a6ca7de9aa1687c8b5dcf199ba671475c9379f74000e09498324f44755a7aca3658f742a6f1ab06f6400ef5f6"
                |> Transaction.deserialize
                |> Expect.equal
                    (Just
                        { body = body15f82a36
                        , witnessSet = witnessSet15f82a36
                        , isValid = True
                        , auxiliaryData = Nothing
                        }
                    )


body15f82a36 : TransactionBody
body15f82a36 =
    { newBody
        | fee = N.fromSafeInt 175929
        , inputs = [ { outputIndex = 0, transactionId = Bytes.fromHexUnchecked "a584f292713ef96210dbcd377cb1fcc537f6f055c4f99df83cb0eb1e3079983d" } ]
        , outputs =
            [ { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromHexUnchecked "4dd37a5e94e082f9096e4e498f36686b58df4a6d8eeb0f918c854d6c")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromHexUnchecked "45dee6ee5d7f631b6226d45f29da411c42fa7e816dc0948d31e0dba7")))
                        }
              , amount = Value.onlyLovelace (N.fromSafeInt 65650430)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            ]
        , proposalProcedures =
            [ { anchor =
                    { dataHash = Bytes.fromHexUnchecked "2f98f57c4149fdfed2b73cbd821226fe417ef5ed49d8f836a37b31edf14dea47"
                    , url = "ipfs://QmWjcHsrq9kKHZZ7aPPFjqN6wLuxH9d8bcqssmrE7H4cvb"
                    }
              , deposit = bigNat [ 7792640, 1490 ]
              , govAction = Info
              , depositReturnAccount = { networkId = Mainnet, stakeCredential = Address.VKeyHash (Bytes.fromHexUnchecked "45dee6ee5d7f631b6226d45f29da411c42fa7e816dc0948d31e0dba7") }
              }
            ]
    }


witnessSet15f82a36 : WitnessSet
witnessSet15f82a36 =
    { newWitnessSet
        | vkeywitness =
            Just
                [ { signature = Bytes.fromHexUnchecked "04980038691b87592a7b66546b30f760bee4f778905a6ca7de9aa1687c8b5dcf199ba671475c9379f74000e09498324f44755a7aca3658f742a6f1ab06f6400e"
                  , vkey = Bytes.fromHexUnchecked "a47afdef5fd0f70a7dc394fe540ddddd86cbaf28f2353acedbd9c6e6f71714db"
                  }
                ]
    }


{-| Next Conway failure.

Tx id: 7925c2e6848235f18440249b3b4ab0111dd6c56fdcc518ba6fd8f02e37e39d24
Block height: 10789398
Previous block intersection:

  - slot: 133828617
  - id: fc09764505b3564007d547ee61d4d8068fe134dea205f913de0f4c987267764a

-}
decode7925c2e6 : Test
decode7925c2e6 =
    test "Tx id 7925c2e6848235f18440249b3b4ab0111dd6c56fdcc518ba6fd8f02e37e39d24" <|
        \_ ->
            Bytes.fromHexUnchecked "84a500d9010281825820a6a9c42c3d88b0839c10d794476b9a6e96bfb0d7bedef7c1e30821ada172d6e30701818258390188802c8e874cad837d58946c0d292adb5d3edba8891b80dc0552dfda4c9dd252d63b877363cb7507061d28278769172826d96c054493c5cb1a0044aa20021a0007a120031a07fa137c13a18204581cdacf06a23e4aaf119024e63deb79861ca175b24e7d44d97fb92b1a22a182582015f82a365bdee483a4b03873a40d3829cc88c048ff3703e11bd01dd9e035c916008201f6a100d90102828258207c185b3cb5d4b0fcbcca69c1f5125b3675f1e8db65e8defdf694521adffbdc885840f40f35329837602a59703c620dae574da577ef809adbeafd58efaf1aa244e98adc577885f0e3ffc7496f658ede85b2c55d70e64fc416a5e908ff6a72a0b65c038258204f30f1008a9814be96f3e8000261f4a37c6f05a98d62128597338d36b2d381955840d8204a663244ecaadc0f2d92230cc3a2044a19944ec0b57d29954dbdf5ffabdc57d6fbce6c2e6e828f36899dbd45a0aedf9d364b5f8ce5dfc9b46b8fca40fc0df5f6"
                |> Transaction.deserialize
                |> Expect.equal
                    (Just
                        { body = body7925c2e6
                        , witnessSet = witnessSet7925c2e6
                        , isValid = True
                        , auxiliaryData = Nothing
                        }
                    )


body7925c2e6 : TransactionBody
body7925c2e6 =
    { newBody
        | fee = N.fromSafeInt 500000
        , inputs = [ { outputIndex = 7, transactionId = Bytes.fromHexUnchecked "a6a9c42c3d88b0839c10d794476b9a6e96bfb0d7bedef7c1e30821ada172d6e3" } ]
        , outputs =
            [ { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromHexUnchecked "88802c8e874cad837d58946c0d292adb5d3edba8891b80dc0552dfda")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromHexUnchecked "4c9dd252d63b877363cb7507061d28278769172826d96c054493c5cb")))
                        }
              , amount = Value.onlyLovelace (N.fromSafeInt 4500000)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            ]
        , ttl = Just (bigNat [ 66720636, 1 ])
        , votingProcedures =
            [ ( VoterPoolId (Bytes.fromHexUnchecked "dacf06a23e4aaf119024e63deb79861ca175b24e7d44d97fb92b1a22")
              , [ ( { govActionIndex = 0, transactionId = Bytes.fromHexUnchecked "15f82a365bdee483a4b03873a40d3829cc88c048ff3703e11bd01dd9e035c916" }
                  , { anchor = Nothing, vote = VoteYes }
                  )
                ]
              )
            ]
    }


witnessSet7925c2e6 : WitnessSet
witnessSet7925c2e6 =
    { newWitnessSet
        | vkeywitness =
            Just
                [ { signature = Bytes.fromHexUnchecked "f40f35329837602a59703c620dae574da577ef809adbeafd58efaf1aa244e98adc577885f0e3ffc7496f658ede85b2c55d70e64fc416a5e908ff6a72a0b65c03"
                  , vkey = Bytes.fromHexUnchecked "7c185b3cb5d4b0fcbcca69c1f5125b3675f1e8db65e8defdf694521adffbdc88"
                  }
                , { signature = Bytes.fromHexUnchecked "d8204a663244ecaadc0f2d92230cc3a2044a19944ec0b57d29954dbdf5ffabdc57d6fbce6c2e6e828f36899dbd45a0aedf9d364b5f8ce5dfc9b46b8fca40fc0d"
                  , vkey = Bytes.fromHexUnchecked "4f30f1008a9814be96f3e8000261f4a37c6f05a98d62128597338d36b2d38195"
                  }
                ]
    }


{-| Next Conway failure. First PlutusV3 transaction!

Tx id: 2264f554c225262acfafba61bb02035b50541924bc5386c08d817e8ae9b2fa27
Block height: 10806773
Previous block intersection:

  - slot: 134185006
  - id: eb87093a7548978b2b913a77d4d2720782bb233e445f074e9dbc29184f68f081

-}
decode2264f554 : Test
decode2264f554 =
    test "Tx id 2264f554c225262acfafba61bb02035b50541924bc5386c08d817e8ae9b2fa27" <|
        \_ ->
            Bytes.fromHexUnchecked "84a300d90102818258209a3323c2872d53ca8bea810b1288f2a267ba139503da71b8836220af70c77ac201018282581d61f56dee6f3bc9460b3653d23a001af813cf301c76c31bf4f404f758f71a0013ccd7a300581d61f56dee6f3bc9460b3653d23a001af813cf301c76c31bf4f404f758f7011a00347b1403d818590251820359024c59024901010033232323232323223225333004323232323232323232533300d300200413232323300400913253330113375e00464a6466602666e1d20000021323374a90041980c18011980c180c800a5eb80cc060c064c0680052f5c0602a6ea803054ccc04ccdc3a400400426466e95200c3301830023301830190014bd701980c180c980d000a5eb80c054dd50060a99980999b87480100084cdd2a40146602e60026602e6030602a6ea80312f5c097ae016374a900118091baa00a13300700100f14a06eb0c054c058c058c058c058c058c058c058c058c048dd5005980a180a8011bad3013001300f375400a2a66601a66e1d200800413233002007132533300f30043010375401026600a00201a2c6eb0c04cc050c050c050c050c050c050c050c050c040dd5004980918079baa005162232533300f3370e9000000899191919299980b180c8010a8030b1bad30170013017002375c602a00260226ea800c54ccc03ccdc3a4004002264646464a66602c60320042a00c2c6eb4c05c004c05c008dd7180a80098089baa0031533300f3370e900200089919299980a180b8010a8020b1bae3015001301137540062a66601e60080022a66602460226ea800c540085858c03cdd50011b874801888c8cc00400400c894ccc044004528099299980799b8f375c602800400829444cc00c00c004c050004c028dd50009806980700118060009806001180500098031baa00114984d958dd7000ab9a5573aaae7955cfaba05742ae8930011e581c5d0a25dd9e2d8d33762d4157183eef452b3b0090025bbf52eeb938aa0001021a00040355a100d9010281825820aa22eb509ee9697b2721beec757ee03a119bbb61dea11307f810ca06ed875aed5840781ec72c3d7a436ff0d28bbff035d3251b5c0e75b22afac48e537bf607ab965c16bd699fe1764f284cdb05dcbcff5064c65b48f9f4f7ba512f52e0ac61094908f5f6"
                |> Transaction.deserialize
                |> Expect.notEqual Nothing



-- last Conway successful block (to avoid starting from the last error):
-- Last (max 2) blocks processed:
--   height: 10825161, slot: 134566787, id: 75e8e41ce84f72fbf06cd427628cd5383e85ca96befe862c6220e56973a89ca8
--   height: 10825160, slot: 134566783, id: c0dfc4acb0f2ef91c13d9e739c12b8796773a1063024be98e5ac722d8b302b78


{-| Conway transaction on Preview with some governance stuff

Tx id: 437389efa6ef8ce5f230b9b991d84aa316664076649cbce47713d7fd3511566d

-}
decode437389ef : Test
decode437389ef =
    test "Tx (Preview) id 437389efa6ef8ce5f230b9b991d84aa316664076649cbce47713d7fd3511566d" <|
        \_ ->
            Bytes.fromHexUnchecked "84a400818258205dd37b30379c9f200c1cc4008525e55d044e139becd4148e071f88ba0bffb137000182825839005dd37e2fcd78fef83200d645ca4ce6e3052c39c9ffe0456dd27e7727ee3f333b37909e7ff5094ac62ca6f7881fba93e83d29226fadcdb439821a00115cb0a1581c2fe3c3364b443194b10954771c95819b8d6ed464033c21f03f8facb5a1446955534401825839005dd37e2fcd78fef83200d645ca4ce6e3052c39c9ffe0456dd27e7727ee3f333b37909e7ff5094ac62ca6f7881fba93e83d29226fadcdb4391a0065fa67021a0002bae9048283078200581cee3f333b37909e7ff5094ac62ca6f7881fba93e83d29226fadcdb4391a001e848083098200581cee3f333b37909e7ff5094ac62ca6f7881fba93e83d29226fadcdb4398200581c9b239a50a537c0e366ffe6e0577ef462cd80be375e8f1c654771e3c2a1008282582052967973501373037fdd0e830be19af37cbd2b7247a1249eda9fa59d48413add5840c5d53f6f4e3d5c4069cb181485cabfec3d028d32248db63835d6d6faaa68331a25fc0015b342bf43132ad19f1ef0b56792532dcdf12caebcd6ce831978e0330e8258201c0241c418256d83462323e12c9428082afe9bc93d4dc1deafbddd48e3400a6d5840348f7188359afc2b7167421853fec79606f7a2691db23470846e615e0bc15ee470349a3fd02302222f1955e58e259175ef592e365460cd3e68f3076e3c6dc60bf5f6"
                |> Transaction.deserialize
                |> Expect.equal
                    (Just
                        { body = body437389ef
                        , witnessSet = witnessSet437389ef
                        , isValid = True
                        , auxiliaryData = Nothing
                        }
                    )


body437389ef =
    { newBody
        | certificates =
            [ RegCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "ee3f333b37909e7ff5094ac62ca6f7881fba93e83d29226fadcdb439"), deposit = N.fromSafeInt 2000000 }
            , VoteDelegCert { delegator = Address.VKeyHash (Bytes.fromHexUnchecked "ee3f333b37909e7ff5094ac62ca6f7881fba93e83d29226fadcdb439"), drep = DrepCredential (Address.VKeyHash (Bytes.fromHexUnchecked "9b239a50a537c0e366ffe6e0577ef462cd80be375e8f1c654771e3c2")) }
            ]
        , fee = N.fromSafeInt 178921
        , inputs =
            [ { outputIndex = 0
              , transactionId = Bytes.fromHexUnchecked "5dd37b30379c9f200c1cc4008525e55d044e139becd4148e071f88ba0bffb137"
              }
            ]
        , outputs =
            [ { address =
                    Address.Shelley
                        { networkId = Testnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromHexUnchecked "5dd37e2fcd78fef83200d645ca4ce6e3052c39c9ffe0456dd27e7727")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromHexUnchecked "ee3f333b37909e7ff5094ac62ca6f7881fba93e83d29226fadcdb439")))
                        }
              , amount = { assets = bytesMap (Dict.fromList [ ( "2fe3c3364b443194b10954771c95819b8d6ed464033c21f03f8facb5", bytesMap (Dict.fromList [ ( "69555344", N.fromSafeInt 1 ) ]) ) ]), lovelace = N.fromSafeInt 1137840 }
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            , { address =
                    Address.Shelley
                        { networkId = Testnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromHexUnchecked "5dd37e2fcd78fef83200d645ca4ce6e3052c39c9ffe0456dd27e7727")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromHexUnchecked "ee3f333b37909e7ff5094ac62ca6f7881fba93e83d29226fadcdb439")))
                        }
              , amount = Value.onlyLovelace (N.fromSafeInt 6683239)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            ]
    }


witnessSet437389ef =
    { newWitnessSet
        | vkeywitness =
            Just
                [ { signature = Bytes.fromHexUnchecked "c5d53f6f4e3d5c4069cb181485cabfec3d028d32248db63835d6d6faaa68331a25fc0015b342bf43132ad19f1ef0b56792532dcdf12caebcd6ce831978e0330e"
                  , vkey = Bytes.fromHexUnchecked "52967973501373037fdd0e830be19af37cbd2b7247a1249eda9fa59d48413add"
                  }
                , { signature = Bytes.fromHexUnchecked "348f7188359afc2b7167421853fec79606f7a2691db23470846e615e0bc15ee470349a3fd02302222f1955e58e259175ef592e365460cd3e68f3076e3c6dc60b"
                  , vkey = Bytes.fromHexUnchecked "1c0241c418256d83462323e12c9428082afe9bc93d4dc1deafbddd48e3400a6d"
                  }
                ]
    }


{-| Conway transaction, Gov proposal Ikigai

Tx id: 59fd353253eb177e2104e8f23ea4c63e3d32ef95c7865d03e90d3884424dc1db

-}
decode59fd3532 : Test
decode59fd3532 =
    test "Tx id 59fd353253eb177e2104e8f23ea4c63e3d32ef95c7865d03e90d3884424dc1db" <|
        \_ ->
            Bytes.fromHexUnchecked "84a400d9010281825820a1b1f8d369dfbd92bb326ee0988df821223b697c414f12d4accd3bbc9c0fd988000181825839015337c0baec2ed3cd5542408e9b4416cc9347328301638f0cc9ecce352058a6f90d7f85735fe3c0f2db06e90c25dfb06e5b1a61e67bb4fe231a0095e327021a0002b35914d9010281841b000000174876e800581de12058a6f90d7f85735fe3c0f2db06e90c25dfb06e5b1a61e67bb4fe23810682784d68747470733a2f2f632d697066732d67772e6e6d6b722e696f2f697066732f516d5864664453536b52385459426e744d54346e75544c39734539453434464a6e705756706159385a5466776a6a5820d4efa9e0c3d80f5be35def08551eaf5aef17eae832fdb835e5f4b208d98f3dffa100d9010281825820fda2a51fcdfefc31161ca70f05140f679f64262f9a3e9620291de296219f099a58406735e04d05c6529c18e217d0951582c42c9d44eab53d45b33e6c657ca1ea6fcab5b9effd3eccd422e49ef7e4aeaa2dfcab14e74d510d6d24bc30d8846a124009f5f6"
                |> Transaction.deserialize
                |> Expect.equal
                    (Just
                        { body = body59fd3532
                        , witnessSet = witnessSet59fd3532
                        , isValid = True
                        , auxiliaryData = Nothing
                        }
                    )


body59fd3532 =
    { newBody
        | fee = N.fromSafeInt 176985
        , inputs = [ { outputIndex = 0, transactionId = Bytes.fromHexUnchecked "a1b1f8d369dfbd92bb326ee0988df821223b697c414f12d4accd3bbc9c0fd988" } ]
        , outputs =
            [ { address = Address.Shelley { networkId = Mainnet, paymentCredential = Address.VKeyHash (Bytes.fromHexUnchecked "5337c0baec2ed3cd5542408e9b4416cc9347328301638f0cc9ecce35"), stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromHexUnchecked "2058a6f90d7f85735fe3c0f2db06e90c25dfb06e5b1a61e67bb4fe23"))) }
              , amount = Value.onlyLovelace (N.fromSafeInt 9823015)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            ]
        , proposalProcedures =
            [ { anchor = { dataHash = Bytes.fromHexUnchecked "d4efa9e0c3d80f5be35def08551eaf5aef17eae832fdb835e5f4b208d98f3dff", url = "https://c-ipfs-gw.nmkr.io/ipfs/QmXdfDSSkR8TYBntMT4nuTL9sE9E44FJnpWVpaY8ZTfwjj" }
              , deposit = bigNat [ 7792640, 1490 ]
              , govAction = Info
              , depositReturnAccount = { networkId = Mainnet, stakeCredential = Address.VKeyHash (Bytes.fromHexUnchecked "2058a6f90d7f85735fe3c0f2db06e90c25dfb06e5b1a61e67bb4fe23") }
              }
            ]
    }


witnessSet59fd3532 =
    { newWitnessSet
        | vkeywitness =
            Just
                [ { signature = Bytes.fromHexUnchecked "6735e04d05c6529c18e217d0951582c42c9d44eab53d45b33e6c657ca1ea6fcab5b9effd3eccd422e49ef7e4aeaa2dfcab14e74d510d6d24bc30d8846a124009"
                  , vkey = Bytes.fromHexUnchecked "fda2a51fcdfefc31161ca70f05140f679f64262f9a3e9620291de296219f099a"
                  }
                ]
    }



{-| Test decoding an output containing an invalid reference script (not cbor-wrapped).
-}
decodeOutputa4005839 : Test
decodeOutputa4005839 =
    test "Output a4005839... (ref script not cbor-wrapped)" <|
        \_ ->
            Bytes.fromHexUnchecked "a40058390068c14f253ceb1934ae8c69c68036273b425bdd3c86987c4d061e119cc043835c5336be17159088c24bf19b3a497106b3d4f41c95a1e860fb01821a01f97904a1581cd4c84babcb60f1e301fa4fd1c1759a2ebcd1d346988418ac459088e4a15653746172636164615265666572656e6365546f6b656e01028201d81846455374616d7003d818458201420101"
                |> Bytes.toBytes
                |> D.decode Utxo.decodeOutput
                |> Expect.notEqual Nothing



--
--
--
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
