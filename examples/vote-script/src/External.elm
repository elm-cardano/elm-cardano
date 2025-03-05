port module External exposing (main)

import Browser
import Bytes.Comparable as Bytes
import Cardano exposing (SpendSource(..), TxIntent(..), WitnessSource(..))
import Cardano.Address as Address exposing (Address, Credential(..), NetworkId(..))
import Cardano.Cip30 as Cip30
import Cardano.Script exposing (PlutusVersion(..))
import Cardano.Transaction as Tx exposing (Transaction)
import Cardano.Utxo as Utxo exposing (DatumOption(..), Output)
import Cbor.Encode
import Dict.Any
import Hex.Convert
import Html exposing (Html, div, text)
import Html.Attributes exposing (height, src)
import Html.Events exposing (onClick)
import Json.Decode as JD exposing (Decoder, Value)
import Json.Encode as JE


main =
    -- The main entry point of our app
    -- More info about that in the Browser package docs:
    -- https://package.elm-lang.org/packages/elm/browser/latest/
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.batch [ fromExternalWallet WalletMsg, fromMainApp MainAppMsg ]
        , view = view
        }


port toExternalWallet : Value -> Cmd msg


port fromExternalWallet : (Value -> msg) -> Sub msg


port toMainApp : Value -> Cmd msg


port fromMainApp : (Value -> msg) -> Sub msg



-- #########################################################
-- MODEL
-- #########################################################


type Model
    = Startup
    | WalletDiscovered (List Cip30.WalletDescriptor)
    | WalletLoading
        { wallet : Cip30.Wallet
        , utxos : List Cip30.Utxo
        }
    | WalletLoaded LoadedWallet { errors : String }


type alias LoadedWallet =
    { wallet : Cip30.Wallet
    , utxos : Utxo.RefDict Output
    , changeAddress : Address
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Startup
    , toExternalWallet <| Cip30.encodeRequest Cip30.discoverWallets
    )



-- #########################################################
-- UPDATE
-- #########################################################


type Msg
    = WalletMsg Value
    | MainAppMsg Value
    | ConnectButtonClicked { id : String }


type MainAppMsg
    = MainAppAskUtxos
    | MainAppAskSignature Transaction


mainAppMsgDecoder : Decoder MainAppMsg
mainAppMsgDecoder =
    JD.field "requestType" JD.string
        |> JD.andThen
            (\requestType ->
                case requestType of
                    "ask-utxos" ->
                        JD.succeed MainAppAskUtxos

                    "ask-signature" ->
                        JD.field "tx" JD.string
                            |> JD.andThen
                                (\txStr ->
                                    case Bytes.fromHex txStr of
                                        Just txBytes ->
                                            case Tx.deserialize txBytes of
                                                Just tx ->
                                                    JD.succeed (MainAppAskSignature tx)

                                                Nothing ->
                                                    JD.fail ("Tx decoding failed for: " ++ txStr)

                                        Nothing ->
                                            JD.fail ("Invalid hex: " ++ txStr)
                                )

                    _ ->
                        JD.fail ("Unrecognized request: " ++ requestType)
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( WalletMsg value, _ ) ->
            case ( JD.decodeValue Cip30.responseDecoder value, model ) of
                -- We just discovered available wallets
                ( Ok (Cip30.AvailableWallets wallets), Startup ) ->
                    ( WalletDiscovered wallets, Cmd.none )

                -- We just connected to the wallet, let’s ask for the available utxos
                ( Ok (Cip30.EnabledWallet wallet), WalletDiscovered _ ) ->
                    ( WalletLoading { wallet = wallet, utxos = [] }
                    , toExternalWallet <| Cip30.encodeRequest <| Cip30.getUtxos wallet { amount = Nothing, paginate = Nothing }
                    )

                -- We just received the utxos, let’s ask for the main change address of the wallet
                ( Ok (Cip30.ApiResponse _ (Cip30.WalletUtxos utxos)), WalletLoading { wallet } ) ->
                    ( WalletLoading { wallet = wallet, utxos = utxos }
                    , toExternalWallet (Cip30.encodeRequest (Cip30.getChangeAddress wallet))
                    )

                ( Ok (Cip30.ApiResponse _ (Cip30.ChangeAddress address)), WalletLoading { wallet, utxos } ) ->
                    ( WalletLoaded { wallet = wallet, utxos = Utxo.refDictFromList utxos, changeAddress = address } { errors = "" }
                    , Cmd.none
                    )

                ( Ok (Cip30.ApiResponse _ (Cip30.SignedTx vkeywitnesses)), WalletLoaded _ _ ) ->
                    let
                        encodedSignatures =
                            JE.object
                                [ ( "responseType", JE.string "signature" )
                                , ( "witnesses"
                                  , JE.list (encodeCborHex << Tx.encodeVKeyWitness) vkeywitnesses
                                  )
                                ]
                    in
                    ( model, toMainApp encodedSignatures )

                _ ->
                    ( model, Cmd.none )

        ( MainAppMsg value, _ ) ->
            case ( JD.decodeValue mainAppMsgDecoder value, model ) of
                ( Ok MainAppAskUtxos, WalletLoaded loadedWallet _ ) ->
                    -- Send available utxos back with toMainApp port
                    let
                        utxoCborEncoder =
                            Cbor.Encode.tuple <|
                                Cbor.Encode.elems
                                    >> Cbor.Encode.elem Utxo.encodeOutputReference Tuple.first
                                    >> Cbor.Encode.elem Utxo.encodeOutput Tuple.second

                        -- Only keep utxos at the change address to simplify fee handling in the Main app
                        utxoSelection =
                            Dict.Any.filter (\_ output -> output.address == loadedWallet.changeAddress) loadedWallet.utxos

                        encodedUtxos =
                            JE.object
                                [ ( "responseType", JE.string "utxos" )
                                , ( "address"
                                  , JE.string <| Bytes.toHex <| Address.toBytes loadedWallet.changeAddress
                                  )
                                , ( "utxos"
                                  , JE.list (encodeCborHex << utxoCborEncoder) (Dict.Any.toList utxoSelection)
                                  )
                                ]
                    in
                    ( model, toMainApp encodedUtxos )

                ( Ok (MainAppAskSignature tx), WalletLoaded loadedWallet _ ) ->
                    -- Ask external wallet for partial signature, then later, send signature back
                    -- TODO: Update the wallet utxos with the content of the Tx
                    let
                        signatureRequest =
                            Cip30.signTx loadedWallet.wallet { partialSign = True } tx
                                |> Cip30.encodeRequest
                    in
                    ( model, toExternalWallet signatureRequest )

                _ ->
                    ( model, Cmd.none )

        ( ConnectButtonClicked { id }, WalletDiscovered _ ) ->
            ( model, toExternalWallet (Cip30.encodeRequest (Cip30.enableWallet { id = id, extensions = [] })) )

        _ ->
            ( model, Cmd.none )



-- Helper


encodeCborHex : Cbor.Encode.Encoder -> Value
encodeCborHex cborEncoder =
    Cbor.Encode.encode cborEncoder
        |> Hex.Convert.toString
        |> JE.string



-- #########################################################
-- VIEW
-- #########################################################


view : Model -> Html Msg
view model =
    case model of
        Startup ->
            div [] [ div [] [ text "This is the external wallet provider app" ] ]

        WalletDiscovered availableWallets ->
            div []
                [ div [] [ text "This is the external wallet provider app" ]
                , div [] [ text "Potential CIP-30 wallets detected:" ]
                , viewAvailableWallets availableWallets
                ]

        WalletLoading _ ->
            div [] [ text "Loading wallet assets ..." ]

        WalletLoaded loadedWallet { errors } ->
            div []
                (viewLoadedWallet loadedWallet
                    ++ [ displayErrors errors
                       ]
                )


displayErrors : String -> Html msg
displayErrors err =
    if err == "" then
        text ""

    else
        div [] [ text <| "ERRORS: " ++ err ]


viewLoadedWallet : LoadedWallet -> List (Html msg)
viewLoadedWallet { wallet, utxos, changeAddress } =
    [ div [] [ text <| "External Wallet: " ++ (Cip30.walletDescriptor wallet).name ]
    , div [] [ text <| "Address: " ++ (Address.toBytes changeAddress |> Bytes.toHex) ]
    , div [] [ text <| "UTxO count: " ++ String.fromInt (Dict.Any.size utxos) ]
    ]


viewAvailableWallets : List Cip30.WalletDescriptor -> Html Msg
viewAvailableWallets wallets =
    let
        walletDescription : Cip30.WalletDescriptor -> String
        walletDescription w =
            "id: " ++ w.id ++ ", name: " ++ w.name

        walletIcon : Cip30.WalletDescriptor -> Html Msg
        walletIcon { icon } =
            Html.img [ src icon, height 32 ] []

        connectButton { id } =
            Html.button [ onClick (ConnectButtonClicked { id = id }) ] [ text "connect" ]

        walletRow w =
            div [] [ walletIcon w, text (walletDescription w), connectButton w ]
    in
    div [] (List.map walletRow wallets)
