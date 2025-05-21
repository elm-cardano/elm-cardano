module Cardano.Cip95 exposing
    ( getPubDRepKey, getRegisteredPubStakeKeys, getUnregisteredPubStakeKeys, signData
    , ApiResponse(..), apiDecoder
    )

{-| CIP 95 support.

@docs getPubDRepKey, getRegisteredPubStakeKeys, getUnregisteredPubStakeKeys, signData

@docs ApiResponse, apiDecoder

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address as Address exposing (CredentialHash, NetworkId)
import Cardano.Cip30 as Cip30
import Cardano.Transaction exposing (Ed25519PublicKey)
import Json.Decode as JDecode exposing (Decoder)
import Json.Encode as JEncode


{-| Retrieve the wallet account public DRep key, derived as described in CIP-105.
-}
getPubDRepKey : Cip30.Wallet -> Cip30.Request
getPubDRepKey wallet =
    Cip30.apiRequest wallet (Just 95) "getPubDRepKey" []


{-| Retrieve the wallet account registered public stake keys.
-}
getRegisteredPubStakeKeys : Cip30.Wallet -> Cip30.Request
getRegisteredPubStakeKeys wallet =
    Cip30.apiRequest wallet (Just 95) "getRegisteredPubStakeKeys" []


{-| Retrieve the wallet account unregistered public stake keys.
-}
getUnregisteredPubStakeKeys : Cip30.Wallet -> Cip30.Request
getUnregisteredPubStakeKeys wallet =
    Cip30.apiRequest wallet (Just 95) "getUnregisteredPubStakeKeys" []


{-| Sign a payload with a given key, using CIP-8 signature scheme.
The credential should correspond to the DRep ID expected to sign.
-}
signData : Cip30.Wallet -> { drepId : Bytes CredentialHash, payload : Bytes a } -> Cip30.Request
signData wallet { drepId, payload } =
    Cip30.apiRequest wallet (Just 95) "signData" [ JEncode.string <| Bytes.toHex drepId, JEncode.string <| Bytes.toHex payload ]


{-| Response type for all API requests done through the `api` object returned when enabling a wallet.
-}
type ApiResponse
    = DrepKey (Bytes Ed25519PublicKey)
    | RegisteredStakeKeys (List (Bytes Ed25519PublicKey))
    | UnregisteredStakeKeys (List (Bytes Ed25519PublicKey))
    | SignedData Cip30.DataSignature
    | UnhandledApiResponse String


{-| API response decoder for CIP-30.
Intented to be provided as argument to the `responseDecoder` function.
-}
apiDecoder : String -> Decoder ApiResponse
apiDecoder method =
    let
        bytesHexDecoder =
            JDecode.map Bytes.fromHexUnchecked JDecode.string
    in
    case method of
        "getPubDRepKey" ->
            JDecode.field "response" bytesHexDecoder
                |> JDecode.map DrepKey

        "getRegisteredPubStakeKeys" ->
            JDecode.field "response" (JDecode.list bytesHexDecoder)
                |> JDecode.map RegisteredStakeKeys

        "getUnregisteredPubStakeKeys" ->
            JDecode.field "response" (JDecode.list bytesHexDecoder)
                |> JDecode.map UnregisteredStakeKeys

        "signData" ->
            (JDecode.field "response" <| Cip30.dataSignatureDecoder)
                |> JDecode.map SignedData

        _ ->
            JDecode.succeed <| UnhandledApiResponse ("Unknown CIP-95 API call: " ++ method)
