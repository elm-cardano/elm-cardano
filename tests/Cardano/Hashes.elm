module Cardano.Hashes exposing (suite)

import Bytes.Comparable as Bytes
import Cardano.AuxiliaryData as AuxiliaryData exposing (AuxiliaryData)
import Cardano.Metadatum as Metadatum
import Cbor.Encode
import Expect
import Natural as N
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Hashes found in Cardano data structures" <|
        [ test "Auxiliary data hash found in Tx 8a8f8dfe" <|
            \_ ->
                let
                    metadataCbor =
                        Cbor.Encode.encode (AuxiliaryData.toCbor auxiliaryData8a8f8dfe)
                            |> Bytes.fromBytes
                in
                Bytes.fromHex "36663d429bded43331a968fcaa3a0aba03d6d83474176b8c85a019b0b408ff8d"
                    |> Expect.equal (Just <| Bytes.blake2b256 metadataCbor)
        ]


auxiliaryData8a8f8dfe : AuxiliaryData
auxiliaryData8a8f8dfe =
    { labels = [ ( N.fromSafeInt 674, Metadatum.Map [ ( Metadatum.String "msg", Metadatum.List [ Metadatum.String "Welcome to the PIGY Oracle and to the Alonzo era!", Metadatum.String "", Metadatum.String "https://oracle.pigytoken.com" ] ) ] ) ]
    , nativeScripts = []
    , plutusV1Scripts = []
    , plutusV2Scripts = []
    , plutusV3Scripts = []
    }
