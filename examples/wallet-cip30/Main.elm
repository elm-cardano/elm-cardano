port module Main exposing (..)

import Browser
import Bytes.Encode
import Dict exposing (Dict)
import ElmCardano.Cip30 as Cip30
import ElmCardano.Value as ECValue
import Html exposing (Html, div, text)
import Html.Attributes exposing (height, src)
import Html.Events exposing (onClick)
import Json.Decode as JDecode exposing (Value, value)


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
    | GetUtxosPaginateButtonClicked Cip30.Wallet
    | GetUtxosAmountButtonClicked Cip30.Wallet
    | GetCollateralButtonClicked Cip30.Wallet
    | GetBalanceButtonClicked Cip30.Wallet
    | GetUsedAddressesButtonClicked Cip30.Wallet
    | GetUnusedAddressesButtonClicked Cip30.Wallet
    | GetChangeAddressButtonClicked Cip30.Wallet
    | GetRewardAddressesButtonClicked Cip30.Wallet
    | SignDataButtonClicked Cip30.Wallet



-- MODEL


type alias Model =
    { availableWallets : List Cip30.WalletDescriptor
    , connectedWallets : Dict String Cip30.Wallet
    , rewardAddress : Maybe { walletId : String, address : String }
    , lastApiResponse : String
    , lastError : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { availableWallets = [], connectedWallets = Dict.empty, rewardAddress = Nothing, lastApiResponse = "", lastError = "" }
    , toWallet <| Cip30.encodeRequest Cip30.discoverWallets
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WalletMsg value ->
            case JDecode.decodeValue Cip30.responseDecoder value of
                Ok (Cip30.AvailableWallets wallets) ->
                    ( { model | availableWallets = wallets, lastError = "" }
                    , Cmd.none
                    )

                Ok (Cip30.EnabledWallet wallet) ->
                    ( addEnabledWallet wallet model
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse { walletId } (Cip30.Extensions extensions)) ->
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", extensions: [" ++ String.join ", " (List.map String.fromInt extensions) ++ "]"
                        , lastError = ""
                      }
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse { walletId } (Cip30.NetworkId networkId)) ->
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", network id: " ++ String.fromInt networkId
                        , lastError = ""
                      }
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse { walletId } (Cip30.WalletUtxos utxos)) ->
                    let
                        utxosStr =
                            case utxos of
                                Nothing ->
                                    "undefined"

                                Just utxosList ->
                                    List.map Debug.toString utxosList
                                        |> String.join "\n"
                    in
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", utxos:\n" ++ utxosStr
                        , lastError = ""
                      }
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse { walletId } (Cip30.Collateral collateral)) ->
                    let
                        utxosStr =
                            case collateral of
                                Nothing ->
                                    "undefined"

                                Just utxos ->
                                    List.map Debug.toString utxos
                                        |> String.join "\n"
                    in
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", collateral:\n" ++ utxosStr
                        , lastError = ""
                      }
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse { walletId } (Cip30.WalletBalance balance)) ->
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", balance:\n" ++ Debug.toString balance
                        , lastError = ""
                      }
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse { walletId } (Cip30.UsedAddresses usedAddresses)) ->
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", used addresses:\n" ++ String.join "\n" usedAddresses
                        , lastError = ""
                      }
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse { walletId } (Cip30.UnusedAddresses unusedAddresses)) ->
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", unused addresses:\n" ++ String.join "\n" unusedAddresses
                        , lastError = ""
                      }
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse { walletId } (Cip30.ChangeAddress changeAddress)) ->
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", change address:\n" ++ changeAddress
                        , lastError = ""
                      }
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse { walletId } (Cip30.RewardAddresses rewardAddresses)) ->
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", reward addresses:\n" ++ String.join "\n" rewardAddresses
                        , rewardAddress = List.head rewardAddresses |> Maybe.map (\addr -> { walletId = walletId, address = addr })
                        , lastError = ""
                      }
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse { walletId } (Cip30.SignedData signedData)) ->
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", signed data:\n" ++ Debug.toString signedData
                        , lastError = ""
                      }
                    , Cmd.none
                    )

                Ok (Cip30.Error error) ->
                    ( { model | lastError = error }, Cmd.none )

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

        GetUtxosPaginateButtonClicked wallet ->
            -- Gero does not paginate
            -- Flint does not paginate
            -- NuFi does not paginate
            ( model, toWallet <| Cip30.encodeRequest <| Cip30.getUtxos wallet { amount = Nothing, paginate = Just { page = 0, limit = 2 } } )

        GetUtxosAmountButtonClicked wallet ->
            -- Lace picks at random (fun!)
            -- Gero does not handle the amount parameter
            -- NuFi does not handle the amount parameter
            ( model, toWallet <| Cip30.encodeRequest <| Cip30.getUtxos wallet { amount = Just (ECValue.onlyLovelace 14000000), paginate = Nothing } )

        GetCollateralButtonClicked wallet ->
            -- Typhon crashes with the amounts
            -- Nami crashes as the method does not exist
            ( model, toWallet <| Cip30.encodeRequest <| Cip30.getCollateral wallet { amount = ECValue.onlyLovelace 3000000 } )

        GetBalanceButtonClicked wallet ->
            -- Eternl has sometimes? a weird response
            ( model, toWallet (Cip30.encodeRequest (Cip30.getBalance wallet)) )

        GetUsedAddressesButtonClicked wallet ->
            ( model, toWallet (Cip30.encodeRequest (Cip30.getUsedAddresses wallet { paginate = Nothing })) )

        GetUnusedAddressesButtonClicked wallet ->
            -- Lace does not return any unused address
            -- Flint returns the same for unused address as used address
            -- Typhon returns the same for unused address and used address
            -- Eternl returns the same
            -- Eternl and Typhon do not return the same addresses while being on the same wallet?
            -- Nami returns no unused address
            ( model, toWallet (Cip30.encodeRequest (Cip30.getUnusedAddresses wallet)) )

        GetChangeAddressButtonClicked wallet ->
            ( model, toWallet (Cip30.encodeRequest (Cip30.getChangeAddress wallet)) )

        GetRewardAddressesButtonClicked wallet ->
            ( model, toWallet (Cip30.encodeRequest (Cip30.getRewardAddresses wallet)) )

        SignDataButtonClicked wallet ->
            case model.rewardAddress of
                Nothing ->
                    ( { model | lastApiResponse = "Click on getRewardAddresses for this wallet first." }, Cmd.none )

                Just { walletId, address } ->
                    if walletId /= .id (Cip30.walletDescriptor wallet) then
                        ( { model | lastApiResponse = "Click on getRewardAddresses for this wallet first." }, Cmd.none )

                    else
                        ( model
                        , toWallet <|
                            Cip30.encodeRequest <|
                                Cip30.signData wallet
                                    { addr = address
                                    , payload = Bytes.Encode.encode (Bytes.Encode.unsignedInt8 42)
                                    }
                        )


addEnabledWallet : Cip30.Wallet -> Model -> Model
addEnabledWallet wallet { availableWallets, connectedWallets, rewardAddress } =
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
    { availableWallets = updatedAvailableWallets
    , connectedWallets = Dict.insert id wallet connectedWallets
    , rewardAddress = rewardAddress
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
        |> List.map (\( d, w ) -> div [] (walletIcon d :: text (walletDescription d) :: walletActions w))
        |> div []


walletActions : Cip30.Wallet -> List (Html Msg)
walletActions wallet =
    [ Html.button [ onClick <| GetExtensionsButtonClicked wallet ] [ text "getExtensions" ]
    , Html.button [ onClick <| GetNetworkIdButtonClicked wallet ] [ text "getNetworkId" ]
    , Html.button [ onClick <| GetUtxosPaginateButtonClicked wallet ] [ text "getUtxos(paginate:2)" ]
    , Html.button [ onClick <| GetUtxosAmountButtonClicked wallet ] [ text "getUtxos(amount:14ada)" ]
    , Html.button [ onClick <| GetCollateralButtonClicked wallet ] [ text "getCollateral(amount:3ada)" ]
    , Html.button [ onClick <| GetBalanceButtonClicked wallet ] [ text "getBalance" ]
    , Html.button [ onClick <| GetUsedAddressesButtonClicked wallet ] [ text "getUsedAddresses" ]
    , Html.button [ onClick <| GetUnusedAddressesButtonClicked wallet ] [ text "getUnusedAddresses" ]
    , Html.button [ onClick <| GetChangeAddressButtonClicked wallet ] [ text "getChangeAddress" ]
    , Html.button [ onClick <| GetRewardAddressesButtonClicked wallet ] [ text "getRewardAddresses" ]
    , Html.button [ onClick <| SignDataButtonClicked wallet ] [ text "signData" ]
    ]
