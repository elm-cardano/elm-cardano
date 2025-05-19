module Cardano.Cip30 exposing
    ( WalletDescriptor, Wallet, walletDescriptor
    , Request, encodeRequest, Paginate
    , discoverWallets, enableWallet
    , getExtensions, getNetworkId, getUtxos, getCollateral, getBalance
    , getUsedAddresses, getUnusedAddresses, getChangeAddress, getRewardAddresses
    , signTx, signTxCbor, signData, submitTx, submitTxCbor
    , Response(..), ApiResponse(..), Utxo, DataSignature, responseDecoder, apiDecoder, utxoDecoder, hexCborDecoder, addressDecoder
    )

{-| CIP 30 support.

@docs WalletDescriptor, Wallet, walletDescriptor

@docs Request, encodeRequest, Paginate

@docs discoverWallets, enableWallet

@docs getExtensions, getNetworkId, getUtxos, getCollateral, getBalance

@docs getUsedAddresses, getUnusedAddresses, getChangeAddress, getRewardAddresses

@docs signTx, signTxCbor, signData, submitTx, submitTxCbor

@docs Response, ApiResponse, Utxo, DataSignature, responseDecoder, apiDecoder, utxoDecoder, hexCborDecoder, addressDecoder

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address as Address exposing (Address, NetworkId)
import Cardano.Transaction as Transaction exposing (Transaction, VKeyWitness)
import Cardano.Utxo as Utxo exposing (TransactionId)
import Cardano.Value as CValue
import Cbor exposing (CborItem)
import Cbor.Decode
import Cbor.Encode
import Cbor.Encode.Extra
import Dict exposing (Dict)
import Hex.Convert
import Json.Decode as JDecode exposing (Decoder, Value)
import Json.Encode as JEncode
import Natural exposing (Natural)


{-| The type returned when asking for available wallets.
-}
type alias WalletDescriptor =
    { id : String
    , name : String
    , icon : String
    , apiVersion : String
    , isEnabled : Bool
    , supportedExtensions : List Int
    }


{-| Opaque Wallet object to be used for all API requests.
-}
type Wallet
    = Wallet
        { descriptor : WalletDescriptor
        , api : Value
        , walletHandle : Value
        }


{-| Retrieve the descriptor associated with a [Wallet] object.
-}
walletDescriptor : Wallet -> WalletDescriptor
walletDescriptor (Wallet { descriptor }) =
    descriptor


{-| Opaque type for requests to be sent to the wallets.
-}
type Request
    = DiscoverWallets
    | Enable { id : String, extensions : List Int }
    | ApiRequest
        { id : String
        , api : Value
        , extension : Maybe Int
        , method : String
        , args : List Value
        }


{-| Typically the first request you have to send, to discover which wallets are installed.

Will typically be followed by a response of the [AvailableWallets] variant
containing a [WalletDescriptor] for each discovered wallet.

-}
discoverWallets : Request
discoverWallets =
    DiscoverWallets


{-| Enable an installed wallet.

Will typically be followed by a response of the [EnabledWallet] variant
containing a [Wallet] to be stored in your model.

-}
enableWallet : { id : String, extensions : List Int } -> Request
enableWallet idAndExtensions =
    Enable idAndExtensions


{-| Get the list of extensions enabled by the wallet.

This feature isn't well supported yet by wallets (as of 2023-10).

-}
getExtensions : Wallet -> Request
getExtensions wallet =
    apiRequest wallet Nothing "getExtensions" []


{-| Get the current network ID of the wallet.
-}
getNetworkId : Wallet -> Request
getNetworkId wallet =
    apiRequest wallet Nothing "getNetworkId" []


{-| Get a list of UTxOs in the wallet.
-}
getUtxos : Wallet -> { amount : Maybe CValue.Value, paginate : Maybe Paginate } -> Request
getUtxos wallet { amount, paginate } =
    apiRequest wallet
        Nothing
        "getUtxos"
        [ encodeMaybe (\a -> CValue.encode a |> encodeCborHex) amount
        , encodeMaybe encodePaginate paginate
        ]


{-| Get a list of UTxOs to be used for collateral.

You need to specify the amount of lovelace you need for collateral.
More info about why that is in the [CIP 30 spec][cip-collateral].

[cip-collateral]: https://cips.cardano.org/cips/cip30/#apigetcollateralparamsamountcborcoinpromisetransactionunspentoutputnull

-}
getCollateral : Wallet -> { amount : Natural } -> Request
getCollateral wallet { amount } =
    let
        params =
            JEncode.object [ ( "amount", Cbor.Encode.Extra.natural amount |> encodeCborHex ) ]
    in
    apiRequest wallet Nothing "getCollateral" [ params ]


encodeCborHex : Cbor.Encode.Encoder -> Value
encodeCborHex cborEncoder =
    Cbor.Encode.encode cborEncoder
        |> Hex.Convert.toString
        |> JEncode.string


encodeMaybe : (a -> Value) -> Maybe a -> Value
encodeMaybe encode maybe =
    Maybe.map encode maybe
        |> Maybe.withDefault JEncode.null


{-| Get the current wallet balance.
-}
getBalance : Wallet -> Request
getBalance wallet =
    apiRequest wallet Nothing "getBalance" []


{-| Get a list of used addresses from the wallet.

That list is wallet-dependent and may not contain all used addresses.
Do not rely on this as a source of truth to get all addresses of a user.

-}
getUsedAddresses : Wallet -> { paginate : Maybe Paginate } -> Request
getUsedAddresses wallet { paginate } =
    apiRequest wallet Nothing "getUsedAddresses" [ encodeMaybe encodePaginate paginate ]


{-| Get a list of unused addresses.

Avoid this feature if possible.
It is not consistent and not compatible with single-address wallets.

-}
getUnusedAddresses : Wallet -> Request
getUnusedAddresses wallet =
    apiRequest wallet Nothing "getUnusedAddresses" []


{-| Get an address that can be used to send funds to this wallet.
-}
getChangeAddress : Wallet -> Request
getChangeAddress wallet =
    apiRequest wallet Nothing "getChangeAddress" []


{-| Get addresses used to withdraw staking rewards.
-}
getRewardAddresses : Wallet -> Request
getRewardAddresses wallet =
    apiRequest wallet Nothing "getRewardAddresses" []


{-| Sign a transaction.
-}
signTx : Wallet -> { partialSign : Bool } -> Transaction -> Request
signTx wallet partialSign tx =
    signTxCbor wallet partialSign (Transaction.serialize tx)


{-| Sign a transaction, already CBOR-encoded (to avoid deserialization-serialization mismatch).
-}
signTxCbor : Wallet -> { partialSign : Bool } -> Bytes Transaction -> Request
signTxCbor wallet { partialSign } txBytes =
    apiRequest wallet Nothing "signTx" [ JEncode.string (Bytes.toHex txBytes), JEncode.bool partialSign ]


{-| Sign an arbitrary payload with your stake key.
-}
signData : Wallet -> { addr : String, payload : Bytes a } -> Request
signData wallet { addr, payload } =
    apiRequest wallet Nothing "signData" [ JEncode.string addr, JEncode.string <| Bytes.toHex payload ]


{-| Encode a transaction and submit it via the wallet.
-}
submitTx : Wallet -> Transaction -> Request
submitTx wallet tx =
    submitTxCbor wallet (Transaction.serialize tx)


{-| Submit a transaction, already CBOR-encoded (to avoid deserialization-serialization mismatch).
-}
submitTxCbor : Wallet -> Bytes Transaction -> Request
submitTxCbor wallet txBytes =
    apiRequest wallet Nothing "submitTx" [ JEncode.string (Bytes.toHex txBytes) ]



-- api.submitTx(tx: cbor\)


{-| Paginate requests that may return many elements.
-}
type alias Paginate =
    { page : Int, limit : Int }


encodePaginate : Paginate -> Value
encodePaginate { page, limit } =
    JEncode.object [ ( "page", JEncode.int page ), ( "limit", JEncode.int limit ) ]


apiRequest : Wallet -> Maybe Int -> String -> List Value -> Request
apiRequest (Wallet { descriptor, api }) extension method args =
    ApiRequest
        { id = descriptor.id
        , api = api
        , extension = extension
        , method = method
        , args = args
        }


{-| Encode a [Request] into a JS value that can be sent through a port.
-}
encodeRequest : Request -> Value
encodeRequest request =
    case request of
        DiscoverWallets ->
            JEncode.object
                [ ( "requestType", JEncode.string "cip30-discover" ) ]

        Enable { id, extensions } ->
            JEncode.object
                [ ( "requestType", JEncode.string "cip30-enable" )
                , ( "id", JEncode.string id )
                , ( "extensions", JEncode.list JEncode.int extensions )
                ]

        ApiRequest { id, api, extension, method, args } ->
            JEncode.object
                [ ( "requestType", JEncode.string "cip30-api" )
                , ( "id", JEncode.string id )
                , ( "api", api )
                , ( "extension", JEncode.int <| Maybe.withDefault 30 extension )
                , ( "method", JEncode.string method )
                , ( "args", JEncode.list identity args )
                ]


{-| Response type for responses from the browser wallets.
-}
type Response apiResponse
    = AvailableWallets (List WalletDescriptor)
    | EnabledWallet Wallet
    | ApiResponse { walletId : String } apiResponse
    | ApiError { code : Int, info : String }
    | UnhandledResponseType String


{-| Response type for all API requests done through the `api` object returned when enabling a wallet.
-}
type ApiResponse
    = Extensions (List Int)
    | NetworkId NetworkId
    | WalletUtxos (List Utxo)
    | Collateral (List Utxo)
    | WalletBalance CValue.Value
    | UsedAddresses (List Address)
    | UnusedAddresses (List Address)
    | ChangeAddress Address
    | RewardAddresses (List Address)
      -- TODO: should we also return the txBeforeSignature?
      -- It would make auto submitting easier
    | SignedTx (List VKeyWitness)
    | SignedData DataSignature
    | SubmittedTx (Bytes TransactionId)
    | UnhandledApiResponse String


{-| UTxO type holding the reference and actual output.
-}
type alias Utxo =
    ( Utxo.OutputReference -- Transaction.Input
    , Utxo.Output -- Transaction.Output
    )


{-| Signature returned from the wallet after signing a payload with your stake key.
-}
type alias DataSignature =
    { signature : CborItem
    , key : CborItem
    }


{-| Decoder for the [Response] type.
-}
responseDecoder : Dict Int (String -> Decoder apiResponse) -> Decoder (Response apiResponse)
responseDecoder apiDecoders =
    JDecode.field "responseType" JDecode.string
        |> JDecode.andThen
            (\responseType ->
                case responseType of
                    "cip30-discover" ->
                        discoverDecoder

                    "cip30-enable" ->
                        enableDecoder

                    "cip30-api" ->
                        JDecode.map3 (\ext id method -> ( ext, id, method ))
                            (JDecode.field "extension" JDecode.int)
                            (JDecode.field "walletId" JDecode.string)
                            (JDecode.field "method" JDecode.string)
                            |> JDecode.andThen
                                (\( ext, id, method ) ->
                                    Dict.get ext apiDecoders
                                        |> Maybe.map (\decoder -> JDecode.map (ApiResponse { walletId = id }) <| decoder method)
                                        |> Maybe.withDefault (JDecode.fail <| "No API decoder provided for extension " ++ String.fromInt ext)
                                )

                    "cip30-error" ->
                        JDecode.field "error" errorDecoder
                            |> JDecode.map ApiError

                    _ ->
                        JDecode.succeed (UnhandledResponseType responseType)
            )


errorDecoder : Decoder { code : Int, info : String }
errorDecoder =
    JDecode.oneOf
        [ JDecode.map2 (\code info -> { code = code, info = info })
            (JDecode.field "code" JDecode.int)
            (JDecode.field "info" JDecode.string)
        , JDecode.map (\msg -> { code = 0, info = msg })
            (JDecode.field "message" JDecode.string)
        , JDecode.map (\msg -> { code = 0, info = msg }) JDecode.string
        ]


discoverDecoder : Decoder (Response a)
discoverDecoder =
    JDecode.list descriptorDecoder
        |> JDecode.field "wallets"
        |> JDecode.map AvailableWallets


descriptorDecoder : Decoder WalletDescriptor
descriptorDecoder =
    JDecode.map6
        -- Explicit constructor to avoid messing with fields order
        (\id name icon apiVersion isEnabled supportedExtensions ->
            { id = id
            , name = name
            , icon = icon
            , apiVersion = apiVersion
            , isEnabled = isEnabled
            , supportedExtensions = supportedExtensions
            }
        )
        (JDecode.field "id" JDecode.string)
        (JDecode.field "name" JDecode.string)
        (JDecode.field "icon" JDecode.string)
        (JDecode.field "apiVersion" JDecode.string)
        (JDecode.field "isEnabled" JDecode.bool)
        (JDecode.field "supportedExtensions" (JDecode.list extensionDecoder))


enableDecoder : Decoder (Response a)
enableDecoder =
    JDecode.map EnabledWallet <|
        JDecode.map3
            -- Explicit constructor to avoid messing with fields order
            (\descriptor api walletHandle ->
                Wallet
                    { descriptor = descriptor
                    , api = api
                    , walletHandle = walletHandle
                    }
            )
            (JDecode.field "descriptor" descriptorDecoder)
            (JDecode.field "api" JDecode.value)
            (JDecode.field "walletHandle" JDecode.value)


{-| API response decoder for CIP-30.
Intented to be provided as argument to the `responseDecoder` function.
-}
apiDecoder : String -> Decoder ApiResponse
apiDecoder method =
    case method of
        "getExtensions" ->
            (JDecode.field "response" <| JDecode.list extensionDecoder)
                |> JDecode.map Extensions

        "getNetworkId" ->
            JDecode.field "response" networkIdDecoder
                |> JDecode.map NetworkId

        "getUtxos" ->
            JDecode.list utxoDecoder
                |> JDecode.nullable
                |> JDecode.field "response"
                |> JDecode.map (\utxos -> WalletUtxos <| Maybe.withDefault [] utxos)

        "getCollateral" ->
            JDecode.list utxoDecoder
                |> JDecode.nullable
                |> JDecode.field "response"
                |> JDecode.map (\utxos -> Collateral <| Maybe.withDefault [] utxos)

        "getBalance" ->
            (JDecode.field "response" <| hexCborDecoder CValue.fromCbor)
                |> JDecode.map WalletBalance

        "getUsedAddresses" ->
            (JDecode.field "response" <| JDecode.list addressDecoder)
                |> JDecode.map UsedAddresses

        "getUnusedAddresses" ->
            (JDecode.field "response" <| JDecode.list addressDecoder)
                |> JDecode.map UnusedAddresses

        "getChangeAddress" ->
            JDecode.field "response" addressDecoder
                |> JDecode.map ChangeAddress

        "getRewardAddresses" ->
            (JDecode.field "response" <| JDecode.list addressDecoder)
                |> JDecode.map RewardAddresses

        "signTx" ->
            Transaction.decodeWitnessSet
                |> Cbor.Decode.map (\w -> Maybe.withDefault [] w.vkeywitness)
                |> hexCborDecoder
                |> JDecode.field "response"
                |> JDecode.map SignedTx

        "signData" ->
            (JDecode.field "response" <| dataSignatureDecoder)
                |> JDecode.map SignedData

        "submitTx" ->
            JDecode.field "response" JDecode.string
                |> JDecode.map (\r -> SubmittedTx (Bytes.fromHexUnchecked r))

        _ ->
            JDecode.succeed <| UnhandledApiResponse ("Unknown API call: " ++ method)


extensionDecoder : Decoder Int
extensionDecoder =
    JDecode.field "cip" JDecode.int


{-| Decode UTxO pairs encoded as CBOR in a hex JSON field.
-}
utxoDecoder : Decoder Utxo
utxoDecoder =
    hexCborDecoder <|
        Cbor.Decode.tuple Tuple.pair <|
            Cbor.Decode.elems
                >> Cbor.Decode.elem Utxo.decodeOutputReference
                >> Cbor.Decode.elem Utxo.decodeOutput


networkIdDecoder : Decoder NetworkId
networkIdDecoder =
    JDecode.map Address.networkIdFromInt JDecode.int
        |> JDecode.andThen (Maybe.map JDecode.succeed >> Maybe.withDefault (JDecode.fail "unknown network id"))


{-| JSON decoder for an [Address] encoded as hexadecimal string.
-}
addressDecoder : Decoder Address
addressDecoder =
    JDecode.string
        |> JDecode.andThen
            (\str ->
                case Maybe.andThen Address.fromBytes (Bytes.fromHex str) of
                    Just address ->
                        JDecode.succeed address

                    _ ->
                        JDecode.fail ("Invalid address: " ++ str)
            )


dataSignatureDecoder : Decoder DataSignature
dataSignatureDecoder =
    JDecode.map2 DataSignature
        (JDecode.field "signature" <| hexCborDecoder Cbor.Decode.any)
        (JDecode.field "key" <| hexCborDecoder Cbor.Decode.any)


{-| Helper function to decode CBOR as hex in JSON.
-}
hexCborDecoder : Cbor.Decode.Decoder a -> Decoder a
hexCborDecoder decoder =
    JDecode.string
        |> JDecode.andThen
            (\str ->
                case Hex.Convert.toBytes str of
                    Just bytes ->
                        case Cbor.Decode.decode decoder bytes of
                            Just a ->
                                JDecode.succeed a

                            Nothing ->
                                JDecode.fail "Failed to decode CBOR"

                    Nothing ->
                        JDecode.fail "Invalid hex bytes"
            )
