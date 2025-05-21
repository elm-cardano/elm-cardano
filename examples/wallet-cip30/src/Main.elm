port module Main exposing (..)

import Browser
import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Encode
import Cardano.Address as Address exposing (Address, CredentialHash, NetworkId)
import Cardano.Cip30 as Cip30
import Cardano.Cip95 as Cip95
import Cardano.Transaction exposing (Transaction)
import Cardano.TxIntent as TxIntent exposing (SpendSource(..), TxIntent(..))
import Cardano.Utxo as Utxo
import Cardano.Value as CValue
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (height, src)
import Html.Events exposing (onClick)
import Json.Decode as JDecode exposing (Value, value)
import Natural as N


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> fromWallet WalletMsg
        , view = view
        }


port toWallet : Value -> Cmd msg


port fromWallet : (Value -> msg) -> Sub msg


type Msg
    = WalletMsg Value
    | DiscoverButtonClicked
    | ConnectButtonClicked { id : String, extensions : List Int }
    | GetExtensionsButtonClicked Cip30.Wallet
    | GetNetworkIdButtonClicked Cip30.Wallet
    | GetUtxosButtonClicked Cip30.Wallet
    | GetUtxosPaginateButtonClicked Cip30.Wallet
    | GetUtxosAmountButtonClicked Cip30.Wallet
    | GetCollateralButtonClicked Cip30.Wallet
    | GetBalanceButtonClicked Cip30.Wallet
    | GetUsedAddressesButtonClicked Cip30.Wallet
    | GetUnusedAddressesButtonClicked Cip30.Wallet
    | GetChangeAddressButtonClicked Cip30.Wallet
    | GetRewardAddressesButtonClicked Cip30.Wallet
    | SignDataPaymentKeyButtonClicked Cip30.Wallet
    | SignDataStakeKeyButtonClicked Cip30.Wallet
    | SignTxButtonClicked Cip30.Wallet
    | SubmitTxButtonClicked Cip30.Wallet
    | GetDrepKeyButtonClicked Cip30.Wallet
    | GetRegisteredStakeKeysButtonClicked Cip30.Wallet
    | GetUnregisteredStakeKeysButtonClicked Cip30.Wallet
    | SignDataDrepKeyButtonClicked Cip30.Wallet



-- MODEL


type alias Model =
    { availableWallets : List Cip30.WalletDescriptor
    , connectedWallets : Dict String Cip30.Wallet
    , utxos : List Cip30.Utxo
    , changeAddress : Maybe { walletId : String, address : Address }
    , drepKeyHash : Maybe { walletId : String, drepId : Bytes CredentialHash }
    , signedTx : TxSign
    , lastApiResponse : String
    , lastError : String
    }


type TxSign
    = NoSignRequest
    | WaitingSign Transaction
    | Signed Transaction


init : () -> ( Model, Cmd Msg )
init _ =
    ( { availableWallets = []
      , connectedWallets = Dict.empty
      , utxos = []
      , changeAddress = Nothing
      , drepKeyHash = Nothing
      , signedTx = NoSignRequest
      , lastApiResponse = ""
      , lastError = ""
      }
    , toWallet <| Cip30.encodeRequest Cip30.discoverWallets
    )



-- UPDATE


type ApiResponse
    = Cip30ApiResponse Cip30.ApiResponse
    | Cip95ApiResponse Cip95.ApiResponse


walletResponseDecoder : JDecode.Decoder (Cip30.Response ApiResponse)
walletResponseDecoder =
    Cip30.responseDecoder <|
        Dict.fromList
            [ ( 30, \method -> JDecode.map Cip30ApiResponse (Cip30.apiDecoder method) )
            , ( 95, \method -> JDecode.map Cip95ApiResponse (Cip95.apiDecoder method) )
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WalletMsg value ->
            case JDecode.decodeValue walletResponseDecoder value of
                Ok (Cip30.AvailableWallets wallets) ->
                    ( { model | availableWallets = wallets, lastError = "" }
                    , Cmd.none
                    )

                Ok (Cip30.EnabledWallet wallet) ->
                    ( addEnabledWallet wallet model
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse { walletId } (Cip30ApiResponse (Cip30.Extensions extensions))) ->
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", extensions: [" ++ String.join ", " (List.map String.fromInt extensions) ++ "]"
                        , lastError = ""
                      }
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse { walletId } (Cip30ApiResponse (Cip30.NetworkId networkId))) ->
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", network id: " ++ Debug.toString networkId
                        , lastError = ""
                      }
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse { walletId } (Cip30ApiResponse (Cip30.WalletUtxos utxosList))) ->
                    let
                        ( utxos, utxosStr ) =
                            ( utxosList
                            , List.map Debug.toString utxosList
                                |> String.join "\n"
                            )
                    in
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", utxos:\n" ++ utxosStr
                        , utxos = utxos
                        , lastError = ""
                      }
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse { walletId } (Cip30ApiResponse (Cip30.Collateral utxos))) ->
                    let
                        utxosStr =
                            List.map Debug.toString utxos
                                |> String.join "\n"
                    in
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", collateral:\n" ++ utxosStr
                        , lastError = ""
                      }
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse { walletId } (Cip30ApiResponse (Cip30.WalletBalance balance))) ->
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", balance:\n" ++ Debug.toString balance
                        , lastError = ""
                      }
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse { walletId } (Cip30ApiResponse (Cip30.UsedAddresses usedAddresses))) ->
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", used addresses:\n" ++ String.join "\n" (List.map Debug.toString usedAddresses)
                        , lastError = ""
                      }
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse { walletId } (Cip30ApiResponse (Cip30.UnusedAddresses unusedAddresses))) ->
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", unused addresses:\n" ++ String.join "\n" (List.map Debug.toString unusedAddresses)
                        , lastError = ""
                      }
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse { walletId } (Cip30ApiResponse (Cip30.ChangeAddress changeAddress))) ->
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", change address:\n" ++ Debug.toString changeAddress
                        , changeAddress = Just { walletId = walletId, address = changeAddress }
                        , lastError = ""
                      }
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse { walletId } (Cip30ApiResponse (Cip30.RewardAddresses rewardAddresses))) ->
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", reward addresses:\n" ++ String.join "\n" (List.map Debug.toString rewardAddresses)
                        , lastError = ""
                      }
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse { walletId } (Cip30ApiResponse (Cip30.SignedTx vkeywitnesses))) ->
                    case model.signedTx of
                        WaitingSign tx ->
                            let
                                witnessSet =
                                    tx.witnessSet

                                newWitnessSet =
                                    if List.isEmpty vkeywitnesses then
                                        tx.witnessSet

                                    else
                                        { witnessSet | vkeywitness = Just vkeywitnesses }
                            in
                            ( { model
                                | signedTx = Signed { tx | witnessSet = newWitnessSet }
                                , lastApiResponse = "wallet: " ++ walletId ++ ", Tx signatures:\n" ++ Debug.toString vkeywitnesses
                                , lastError = ""
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                Ok (Cip30.ApiResponse { walletId } (Cip30ApiResponse (Cip30.SubmittedTx txId))) ->
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", Tx submitted: " ++ Bytes.toHex txId
                        , lastError = ""
                      }
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse { walletId } (Cip30ApiResponse (Cip30.SignedData signedData))) ->
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", signed data:\n" ++ Debug.toString signedData
                        , lastError = ""
                      }
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse _ (Cip30ApiResponse (Cip30.UnhandledApiResponse error))) ->
                    ( { model | lastError = Debug.toString error }, Cmd.none )

                Ok (Cip30.ApiResponse { walletId } (Cip95ApiResponse (Cip95.DrepKey key))) ->
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", drep key: " ++ Bytes.toHex key
                        , drepKeyHash = Just { walletId = walletId, drepId = Bytes.blake2b224 key }
                        , lastError = ""
                      }
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse { walletId } (Cip95ApiResponse (Cip95.RegisteredStakeKeys keys))) ->
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", registered stake keys:\n" ++ String.join "\n" (List.map Bytes.toHex keys)
                        , lastError = ""
                      }
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse { walletId } (Cip95ApiResponse (Cip95.UnregisteredStakeKeys keys))) ->
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", unregistered stake keys:\n" ++ String.join "\n" (List.map Bytes.toHex keys)
                        , lastError = ""
                      }
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse { walletId } (Cip95ApiResponse (Cip95.SignedData signedData))) ->
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", signed data:\n" ++ Debug.toString signedData
                        , lastError = ""
                      }
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse _ (Cip95ApiResponse (Cip95.UnhandledApiResponse error))) ->
                    ( { model | lastError = Debug.toString error }, Cmd.none )

                Ok (Cip30.ApiError error) ->
                    ( { model | lastError = Debug.toString error }, Cmd.none )

                Ok (Cip30.UnhandledResponseType error) ->
                    ( { model | lastError = error }, Cmd.none )

                Err error ->
                    ( { model | lastError = JDecode.errorToString error }, Cmd.none )

        DiscoverButtonClicked ->
            ( model, toWallet <| Cip30.encodeRequest Cip30.discoverWallets )

        ConnectButtonClicked { id, extensions } ->
            ( model, toWallet (Cip30.encodeRequest (Cip30.enableWallet { id = id, extensions = extensions })) )

        GetExtensionsButtonClicked wallet ->
            ( model, toWallet (Cip30.encodeRequest (Cip30.getExtensions wallet)) )

        GetNetworkIdButtonClicked wallet ->
            ( model, toWallet (Cip30.encodeRequest (Cip30.getNetworkId wallet)) )

        GetUtxosButtonClicked wallet ->
            ( model, toWallet <| Cip30.encodeRequest <| Cip30.getUtxos wallet { amount = Nothing, paginate = Nothing } )

        GetUtxosPaginateButtonClicked wallet ->
            ( model, toWallet <| Cip30.encodeRequest <| Cip30.getUtxos wallet { amount = Nothing, paginate = Just { page = 0, limit = 2 } } )

        GetUtxosAmountButtonClicked wallet ->
            ( model, toWallet <| Cip30.encodeRequest <| Cip30.getUtxos wallet { amount = Just (CValue.onlyLovelace <| N.fromSafeInt 14000000), paginate = Nothing } )

        GetCollateralButtonClicked wallet ->
            ( model, toWallet <| Cip30.encodeRequest <| Cip30.getCollateral wallet { amount = N.fromSafeInt 3000000 } )

        GetBalanceButtonClicked wallet ->
            ( model, toWallet (Cip30.encodeRequest (Cip30.getBalance wallet)) )

        GetUsedAddressesButtonClicked wallet ->
            ( model, toWallet (Cip30.encodeRequest (Cip30.getUsedAddresses wallet { paginate = Nothing })) )

        GetUnusedAddressesButtonClicked wallet ->
            ( model, toWallet (Cip30.encodeRequest (Cip30.getUnusedAddresses wallet)) )

        GetChangeAddressButtonClicked wallet ->
            ( model, toWallet (Cip30.encodeRequest (Cip30.getChangeAddress wallet)) )

        GetRewardAddressesButtonClicked wallet ->
            ( model, toWallet (Cip30.encodeRequest (Cip30.getRewardAddresses wallet)) )

        SignTxButtonClicked wallet ->
            case ( model.utxos, model.changeAddress ) of
                ( [], _ ) ->
                    ( { model | lastError = "You need to click the 'getUtxos' button first to be aware of some existing utxos" }, Cmd.none )

                ( _, Nothing ) ->
                    ( { model | lastError = "You need to click the 'getChangeAddress' button first to know the wallet address" }, Cmd.none )

                ( utxos, Just { address } ) ->
                    let
                        localStateUtxos =
                            Utxo.refDictFromList utxos

                        txIntents =
                            [ SendTo address CValue.zero ]
                    in
                    case TxIntent.finalize localStateUtxos [] txIntents of
                        Ok { tx } ->
                            ( { model | signedTx = WaitingSign tx }
                            , toWallet (Cip30.encodeRequest (Cip30.signTx wallet { partialSign = False } tx))
                            )

                        Err txBuildingError ->
                            ( { model | lastError = TxIntent.errorToString txBuildingError }, Cmd.none )

        SubmitTxButtonClicked wallet ->
            case model.signedTx of
                Signed tx ->
                    ( model, toWallet (Cip30.encodeRequest (Cip30.submitTx wallet tx)) )

                _ ->
                    ( { model | lastError = "You need to click the 'signTx' button first to sign a Tx before submitting it" }, Cmd.none )

        SignDataPaymentKeyButtonClicked wallet ->
            case model.changeAddress of
                Nothing ->
                    ( { model | lastApiResponse = "Click on getChangeAddress for this wallet first." }, Cmd.none )

                Just { walletId, address } ->
                    if walletId /= (Cip30.walletDescriptor wallet).id then
                        ( { model | lastApiResponse = "Click on getChangeAddress for this wallet first." }, Cmd.none )

                    else
                        case address of
                            Address.Shelley { networkId, paymentCredential } ->
                                case paymentCredential of
                                    Address.ScriptHash _ ->
                                        ( { model | lastApiResponse = "Your change address is a script, not a public key." }, Cmd.none )

                                    Address.VKeyHash keyHash ->
                                        ( model
                                        , toWallet <|
                                            Cip30.encodeRequest <|
                                                Cip30.signData wallet
                                                    { networkId = networkId
                                                    , keyType = Cip30.PaymentKey
                                                    , keyHash = keyHash
                                                    , payload = Bytes.fromBytes <| Bytes.Encode.encode (Bytes.Encode.unsignedInt8 42)
                                                    }
                                        )

                            _ ->
                                ( { model | lastApiResponse = "Your change address is not a Shelley address!?" }, Cmd.none )

        SignDataStakeKeyButtonClicked wallet ->
            case model.changeAddress of
                Nothing ->
                    ( { model | lastApiResponse = "Click on getChangeAddress for this wallet first." }, Cmd.none )

                Just { walletId, address } ->
                    if walletId /= (Cip30.walletDescriptor wallet).id then
                        ( { model | lastApiResponse = "Click on getChangeAddress for this wallet first." }, Cmd.none )

                    else
                        case address of
                            Address.Shelley { networkId, stakeCredential } ->
                                case stakeCredential of
                                    Just (Address.InlineCredential (Address.VKeyHash keyHash)) ->
                                        ( model
                                        , toWallet <|
                                            Cip30.encodeRequest <|
                                                Cip30.signData wallet
                                                    { networkId = networkId
                                                    , keyType = Cip30.StakeKey
                                                    , keyHash = keyHash
                                                    , payload = Bytes.fromBytes <| Bytes.Encode.encode (Bytes.Encode.unsignedInt8 42)
                                                    }
                                        )

                                    _ ->
                                        ( { model | lastApiResponse = "Stake credential is absent or not a public key." }, Cmd.none )

                            _ ->
                                ( { model | lastApiResponse = "Your change address is not a Shelley address!?" }, Cmd.none )

        GetDrepKeyButtonClicked wallet ->
            ( model, toWallet (Cip30.encodeRequest (Cip95.getPubDRepKey wallet)) )

        GetRegisteredStakeKeysButtonClicked wallet ->
            ( model, toWallet (Cip30.encodeRequest (Cip95.getRegisteredPubStakeKeys wallet)) )

        GetUnregisteredStakeKeysButtonClicked wallet ->
            ( model, toWallet (Cip30.encodeRequest (Cip95.getUnregisteredPubStakeKeys wallet)) )

        SignDataDrepKeyButtonClicked wallet ->
            case model.drepKeyHash of
                Nothing ->
                    ( { model | lastApiResponse = "Click on getPubDRepKey for this wallet first." }, Cmd.none )

                Just { walletId, drepId } ->
                    if walletId /= (Cip30.walletDescriptor wallet).id then
                        ( { model | lastApiResponse = "Click on getPubDRepKey for this wallet first." }, Cmd.none )

                    else
                        ( model
                        , toWallet <|
                            Cip30.encodeRequest <|
                                Cip95.signData wallet
                                    { drepId = drepId
                                    , payload = Bytes.fromBytes <| Bytes.Encode.encode (Bytes.Encode.unsignedInt8 42)
                                    }
                        )


addEnabledWallet : Cip30.Wallet -> Model -> Model
addEnabledWallet wallet ({ availableWallets, connectedWallets } as model) =
    -- Modify the available wallets with the potentially new "enabled" status
    let
        { id, isEnabled } =
            Cip30.walletDescriptor wallet

        updatedAvailableWallets : List Cip30.WalletDescriptor
        updatedAvailableWallets =
            availableWallets
                |> List.map
                    (\w ->
                        if w.id == id then
                            { w | isEnabled = isEnabled }

                        else
                            w
                    )
    in
    { model
        | availableWallets = updatedAvailableWallets
        , connectedWallets = Dict.insert id wallet connectedWallets
        , lastApiResponse = ""
        , lastError = ""
    }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text "Hello Cardano!" ]
        , div [] [ Html.button [ onClick DiscoverButtonClicked ] [ text "discover wallets" ] ]
        , div [] [ text "Available wallets:" ]
        , viewAvailableWallets model.availableWallets
        , div [] [ text "Connected wallets:" ]
        , viewConnectedWallets model.connectedWallets
        , div [] [ text "Last API request response:" ]
        , Html.pre [] [ text model.lastApiResponse ]
        , div [] [ text "Last error:" ]
        , Html.pre [] [ text model.lastError ]
        ]


viewAvailableWallets : List Cip30.WalletDescriptor -> Html Msg
viewAvailableWallets wallets =
    let
        walletDescription : Cip30.WalletDescriptor -> String
        walletDescription w =
            "id: "
                ++ w.id
                ++ ", name: "
                ++ w.name
                ++ ", apiVersion: "
                ++ w.apiVersion
                ++ ", isEnabled: "
                ++ Debug.toString w.isEnabled
                ++ ", supportedExtensions: "
                ++ Debug.toString w.supportedExtensions

        walletIcon : Cip30.WalletDescriptor -> Html Msg
        walletIcon { icon } =
            Html.img [ src icon, height 32 ] []

        enableButton : Cip30.WalletDescriptor -> Html Msg
        enableButton { id, supportedExtensions } =
            Html.button [ onClick (ConnectButtonClicked { id = id, extensions = supportedExtensions }) ] [ text "connect" ]
    in
    wallets
        |> List.map (\w -> div [] [ walletIcon w, text (walletDescription w), enableButton w ])
        |> div []


viewConnectedWallets : Dict String Cip30.Wallet -> Html Msg
viewConnectedWallets wallets =
    let
        walletDescription : Cip30.WalletDescriptor -> String
        walletDescription w =
            "id: "
                ++ w.id
                ++ ", name: "
                ++ w.name

        walletIcon : Cip30.WalletDescriptor -> Html Msg
        walletIcon { icon } =
            Html.img [ src icon, height 32 ] []
    in
    Dict.values wallets
        |> List.map (\w -> ( Cip30.walletDescriptor w, w ))
        |> List.map
            (\( d, w ) ->
                div []
                    (walletIcon d
                        :: text (walletDescription d)
                        :: walletActionsCip30 w
                        ++ walletActionsCip95 d.supportedExtensions w
                    )
            )
        |> div []


walletActionsCip30 : Cip30.Wallet -> List (Html Msg)
walletActionsCip30 wallet =
    [ Html.button [ onClick <| GetExtensionsButtonClicked wallet ] [ text "getExtensions" ]
    , Html.button [ onClick <| GetNetworkIdButtonClicked wallet ] [ text "getNetworkId" ]
    , Html.button [ onClick <| GetUtxosButtonClicked wallet ] [ text "getUtxos" ]
    , Html.button [ onClick <| GetUtxosPaginateButtonClicked wallet ] [ text "getUtxos(paginate:2)" ]
    , Html.button [ onClick <| GetUtxosAmountButtonClicked wallet ] [ text "getUtxos(amount:14ada)" ]
    , Html.button [ onClick <| GetCollateralButtonClicked wallet ] [ text "getCollateral(amount:3ada)" ]
    , Html.button [ onClick <| GetBalanceButtonClicked wallet ] [ text "getBalance" ]
    , Html.button [ onClick <| GetUsedAddressesButtonClicked wallet ] [ text "getUsedAddresses" ]
    , Html.button [ onClick <| GetUnusedAddressesButtonClicked wallet ] [ text "getUnusedAddresses" ]
    , Html.button [ onClick <| GetChangeAddressButtonClicked wallet ] [ text "getChangeAddress" ]
    , Html.button [ onClick <| GetRewardAddressesButtonClicked wallet ] [ text "getRewardAddresses" ]
    , Html.button [ onClick <| SignDataPaymentKeyButtonClicked wallet ] [ text "signData with payment key" ]
    , Html.button [ onClick <| SignDataStakeKeyButtonClicked wallet ] [ text "signData with stake key" ]
    , Html.button [ onClick <| SignTxButtonClicked wallet ] [ text "signTx" ]
    , Html.button [ onClick <| SubmitTxButtonClicked wallet ] [ text "submitTx" ]
    ]


walletActionsCip95 : List Int -> Cip30.Wallet -> List (Html Msg)
walletActionsCip95 supportedExtensions wallet =
    if List.member 95 supportedExtensions then
        [ Html.text " | CIP-95 "
        , Html.button [ onClick <| GetDrepKeyButtonClicked wallet ] [ text "getPubDRepKey" ]
        , Html.button [ onClick <| GetRegisteredStakeKeysButtonClicked wallet ] [ text "getRegisteredPubStakeKeys" ]
        , Html.button [ onClick <| GetUnregisteredStakeKeysButtonClicked wallet ] [ text "getUnregisteredPubStakeKeys" ]
        , Html.button [ onClick <| SignDataDrepKeyButtonClicked wallet ] [ text "signData with DRep key" ]
        ]

    else
        []
