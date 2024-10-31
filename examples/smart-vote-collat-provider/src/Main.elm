port module Main exposing (main)

import Browser
import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano exposing (SpendSource(..), TxIntent(..), WitnessSource(..), dummyBytes)
import Cardano.Address as Address exposing (Address, Credential(..), CredentialHash, NetworkId(..))
import Cardano.Cip30 as Cip30
import Cardano.Data as Data
import Cardano.Script exposing (PlutusVersion(..), ScriptCbor)
import Cardano.Transaction as Tx exposing (Transaction)
import Cardano.Utxo as Utxo exposing (DatumOption(..), Output, OutputReference, TransactionId)
import Cardano.Value
import Dict.Any
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (height, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD exposing (Decoder, Value)
import Natural


main =
    -- The main entry point of our app
    -- More info about that in the Browser package docs:
    -- https://package.elm-lang.org/packages/elm/browser/latest/
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> fromWallet WalletMsg
        , view = view
        }


port toWallet : Value -> Cmd msg


port fromWallet : (Value -> msg) -> Sub msg



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
    | BlueprintLoaded LoadedWallet LockScript { errors : String }
    | Submitting AppContext Action { tx : Transaction, errors : String }
    | TxSubmitted AppContext Action { txId : Bytes TransactionId, errors : String }


type alias LoadedWallet =
    { wallet : Cip30.Wallet
    , utxos : Utxo.RefDict Output
    , changeAddress : Address
    }


type alias LockScript =
    { hash : Bytes CredentialHash
    , compiledCode : Bytes ScriptCbor
    }


type alias AppContext =
    { loadedWallet : LoadedWallet
    , myKeyCred : Bytes CredentialHash
    , myStakeKeyHash : Maybe (Bytes CredentialHash)
    , localStateUtxos : Utxo.RefDict Output
    , lockScript : LockScript
    , scriptAddress : Address
    }


type Action
    = Locking
      -- TODO: when unlocking, try using the external wallet to pay the fees!
    | Unlocking


init : () -> ( Model, Cmd Msg )
init _ =
    ( Startup
    , toWallet <| Cip30.encodeRequest Cip30.discoverWallets
    )



-- #########################################################
-- UPDATE
-- #########################################################


type Msg
    = WalletMsg Value
    | ConnectButtonClicked { id : String }
    | LoadBlueprintButtonClicked
    | GotBlueprint (Result Http.Error LockScript)
    | LockAdaButtonClicked
    | UnlockAdaButtonClicked


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
                    , toWallet <| Cip30.encodeRequest <| Cip30.getUtxos wallet { amount = Nothing, paginate = Nothing }
                    )

                -- We just received the utxos, let’s ask for the main change address of the wallet
                ( Ok (Cip30.ApiResponse { walletId } (Cip30.WalletUtxos utxos)), WalletLoading { wallet } ) ->
                    ( WalletLoading { wallet = wallet, utxos = utxos }
                    , toWallet (Cip30.encodeRequest (Cip30.getChangeAddress wallet))
                    )

                ( Ok (Cip30.ApiResponse { walletId } (Cip30.ChangeAddress address)), WalletLoading { wallet, utxos } ) ->
                    ( WalletLoaded { wallet = wallet, utxos = Utxo.refDictFromList utxos, changeAddress = address } { errors = "" }
                    , Cmd.none
                    )

                ( Ok (Cip30.ApiResponse _ (Cip30.SignedTx vkeywitnesses)), Submitting ctx action { tx } ) ->
                    let
                        -- Update the signatures of the Tx with the wallet response
                        signedTx =
                            Tx.updateSignatures (\_ -> Just vkeywitnesses) tx

                        _ =
                            Debug.log "signedTx" (Tx.serialize signedTx)
                    in
                    ( Submitting ctx action { tx = signedTx, errors = "" }
                    , toWallet (Cip30.encodeRequest (Cip30.submitTx ctx.loadedWallet.wallet signedTx))
                    )

                ( Ok (Cip30.ApiResponse { walletId } (Cip30.SubmittedTx txId)), Submitting ({ loadedWallet } as ctx) action { tx } ) ->
                    let
                        -- Update the known UTxOs set after the given Tx is processed
                        { updatedState, spent, created } =
                            Cardano.updateLocalState txId tx ctx.localStateUtxos

                        -- Also update specifically our wallet UTxOs knowledge
                        -- This isn’t purely necessary, but just to keep a consistent wallet state
                        unspentUtxos =
                            List.foldl (\( ref, _ ) state -> Dict.Any.remove ref state) loadedWallet.utxos spent

                        updatedWalletUtxos =
                            List.foldl
                                (\( ref, output ) state ->
                                    if output.address == loadedWallet.changeAddress then
                                        Dict.Any.insert ref output state

                                    else
                                        state
                                )
                                unspentUtxos
                                created

                        updatedContext =
                            { ctx
                                | localStateUtxos = updatedState
                                , loadedWallet = { loadedWallet | utxos = updatedWalletUtxos }
                            }
                    in
                    ( TxSubmitted updatedContext action { txId = txId, errors = "" }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ( ConnectButtonClicked { id }, WalletDiscovered descriptors ) ->
            ( model, toWallet (Cip30.encodeRequest (Cip30.enableWallet { id = id, extensions = [] })) )

        ( LoadBlueprintButtonClicked, WalletLoaded _ _ ) ->
            ( model
            , let
                blueprintDecoder : Decoder LockScript
                blueprintDecoder =
                    JD.at [ "validators" ]
                        (JD.index 0
                            (JD.map2 LockScript
                                (JD.field "hash" JD.string |> JD.map Bytes.fromHexUnchecked)
                                (JD.field "compiledCode" JD.string |> JD.map Bytes.fromHexUnchecked)
                            )
                        )
              in
              Http.get
                { url = "plutus.json"
                , expect = Http.expectJson GotBlueprint blueprintDecoder
                }
            )

        ( GotBlueprint result, WalletLoaded w _ ) ->
            case result of
                Ok lockScript ->
                    ( BlueprintLoaded w lockScript { errors = "" }, Cmd.none )

                Err err ->
                    -- Handle error as needed
                    ( WalletLoaded w { errors = Debug.toString err }, Cmd.none )

        ( LockAdaButtonClicked, BlueprintLoaded w lockScript _ ) ->
            let
                -- Extract both parts (payment/stake) from our wallet address
                ( myKeyCred, myStakeCred ) =
                    ( Address.extractPubKeyHash w.changeAddress
                        |> Maybe.withDefault (dummyBytes 28 "ERROR")
                    , Address.extractStakeCredential w.changeAddress
                    )

                -- Generate the script address while keeping it in our stake
                scriptAddress =
                    Address.Shelley
                        { networkId = Testnet
                        , paymentCredential = ScriptHash lockScript.hash
                        , stakeCredential = myStakeCred
                        }
            in
            lock
                { localStateUtxos = w.utxos
                , myKeyCred = myKeyCred
                , myStakeKeyHash = Address.extractStakeKeyHash w.changeAddress
                , scriptAddress = scriptAddress
                , loadedWallet = w
                , lockScript = lockScript
                }

        ( LockAdaButtonClicked, TxSubmitted ctx _ _ ) ->
            lock ctx

        ( UnlockAdaButtonClicked, TxSubmitted ctx action { txId } ) ->
            let
                twoAda =
                    Cardano.Value.onlyLovelace (Natural.fromSafeString "2000000")

                redeemer =
                    Data.Constr Natural.zero [ Data.Bytes (Bytes.fromText "Hello, World!") ]

                unlockKey =
                    ctx.myStakeKeyHash
                        |> Maybe.withDefault ctx.myKeyCred

                unlockTxAttempt =
                    [ Spend
                        (FromPlutusScript
                            -- The previously sent UTxO was the first output of the locking Tx
                            { spentInput = OutputReference txId 0
                            , datumWitness = Nothing
                            , plutusScriptWitness =
                                { script = ( PlutusV3, WitnessValue ctx.lockScript.compiledCode )
                                , redeemerData = \_ -> redeemer
                                , requiredSigners = [ unlockKey ]
                                }
                            }
                        )
                    , SendTo ctx.loadedWallet.changeAddress twoAda
                    ]
                        |> Cardano.finalize ctx.localStateUtxos []
            in
            case unlockTxAttempt of
                Ok unlockTx ->
                    let
                        cleanTx =
                            Tx.updateSignatures (\_ -> Nothing) unlockTx
                    in
                    ( Submitting ctx Unlocking { tx = cleanTx, errors = "" }
                    , toWallet (Cip30.encodeRequest (Cip30.signTx ctx.loadedWallet.wallet { partialSign = False } cleanTx))
                    )

                Err err ->
                    ( TxSubmitted ctx action { txId = txId, errors = Debug.toString err }
                    , Cmd.none
                    )

        _ ->
            ( model, Cmd.none )


lock : AppContext -> ( Model, Cmd Msg )
lock ({ localStateUtxos, myKeyCred, myStakeKeyHash, scriptAddress, loadedWallet, lockScript } as ctx) =
    let
        -- 1 ada is 1 million lovelaces
        twoAda =
            Cardano.Value.onlyLovelace (Natural.fromSafeString "2000000")

        -- Use my stake key for the unlocking datum (default to spend key if unavailable)
        unlockCred =
            myStakeKeyHash
                |> Maybe.withDefault myKeyCred

        -- Datum as specified by the blueprint of the lock script,
        -- containing our credentials for later verification when spending
        datum =
            Data.Constr Natural.zero [ Data.Bytes (Bytes.toAny unlockCred) ]

        -- Transaction locking 2 ada at the script address
        lockTxAttempt =
            [ Spend (FromWallet loadedWallet.changeAddress twoAda)
            , SendToOutput
                { address = scriptAddress
                , amount = twoAda
                , datumOption = Just (DatumValue datum)
                , referenceScript = Nothing
                }
            ]
                |> Cardano.finalize localStateUtxos []
    in
    case lockTxAttempt of
        Ok lockTx ->
            let
                cleanTx =
                    Tx.updateSignatures (\_ -> Nothing) lockTx
            in
            ( Submitting ctx Locking { tx = cleanTx, errors = "" }
            , toWallet (Cip30.encodeRequest (Cip30.signTx loadedWallet.wallet { partialSign = False } cleanTx))
            )

        Err err ->
            ( BlueprintLoaded loadedWallet lockScript { errors = Debug.toString err }
            , Cmd.none
            )



-- #########################################################
-- VIEW
-- #########################################################


view : Model -> Html Msg
view model =
    case model of
        Startup ->
            div [] [ div [] [ text "Hello Cardano!" ] ]

        WalletDiscovered availableWallets ->
            div []
                [ div [] [ text "Hello Cardano!" ]
                , div [] [ text "CIP-30 wallets detected:" ]
                , viewAvailableWallets availableWallets
                ]

        WalletLoading _ ->
            div [] [ text "Loading wallet assets ..." ]

        WalletLoaded loadedWallet { errors } ->
            div []
                (viewLoadedWallet loadedWallet
                    ++ [ button [ onClick LoadBlueprintButtonClicked ] [ text "Load Blueprint" ]
                       , displayErrors errors
                       ]
                )

        BlueprintLoaded loadedWallet lockScript { errors } ->
            div []
                (viewLoadedWallet loadedWallet
                    ++ [ div [] [ text <| "Script hash: " ++ Bytes.toHex lockScript.hash ]
                       , div [] [ text <| "Script size (bytes): " ++ String.fromInt (Bytes.width lockScript.compiledCode) ]
                       , button [ onClick LockAdaButtonClicked ] [ text "Lock 2 ADA" ]
                       , displayErrors errors
                       ]
                )

        Submitting { loadedWallet, lockScript } action { tx, errors } ->
            div []
                (viewLoadedWallet loadedWallet
                    ++ [ div [] [ text <| "Signing and submitting the " ++ Debug.toString action ++ " transaction ..." ]
                       , displayErrors errors
                       ]
                )

        TxSubmitted { loadedWallet, lockScript } action { txId, errors } ->
            let
                actionButton =
                    case action of
                        Locking ->
                            button [ onClick UnlockAdaButtonClicked ] [ text "Unlock 2 ADA" ]

                        Unlocking ->
                            button [ onClick LockAdaButtonClicked ] [ text "Lock 2 ADA" ]
            in
            div []
                (viewLoadedWallet loadedWallet
                    ++ [ div [] [ text <| "Tx submitted! with ID: " ++ Bytes.toHex txId ]
                       , actionButton
                       , displayErrors errors
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
    [ div [] [ text <| "Wallet: " ++ (Cip30.walletDescriptor wallet).name ]
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
