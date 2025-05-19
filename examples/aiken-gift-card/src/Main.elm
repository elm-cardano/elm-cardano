port module Main exposing (..)

import Browser
import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Map as BytesMap
import Cardano.Address as Address exposing (Address, Credential(..), CredentialHash, NetworkId(..))
import Cardano.Cip30 as Cip30
import Cardano.Data as Data
import Cardano.MultiAsset exposing (AssetName)
import Cardano.Script as Script exposing (PlutusVersion(..), ScriptCbor)
import Cardano.Transaction as Tx exposing (Transaction)
import Cardano.TxIntent as TxIntent exposing (SpendSource(..), TxIntent(..))
import Cardano.Uplc as Uplc
import Cardano.Utxo as Utxo exposing (DatumOption(..), Output, OutputReference, TransactionId, outputReferenceToData)
import Cardano.Value
import Cardano.Witness as Witness
import Dict
import Dict.Any
import Html exposing (Html, button, div, text)
import Html.Attributes as HA exposing (height, src)
import Html.Events as HE exposing (onClick)
import Http
import Integer
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
    | BlueprintLoaded LoadedWallet UnappliedScript { tokenName : String, errors : String }
    | ParametersSet AppContext { errors : String }
    | Submitting AppContext Action { tx : Transaction, errors : String }
    | TxSubmitted AppContext Action { txId : Bytes TransactionId, errors : String }


type alias LoadedWallet =
    { wallet : Cip30.Wallet
    , utxos : Utxo.RefDict Output
    , changeAddress : Address
    }


type alias UnappliedScript =
    { compiledCode : Bytes ScriptCbor }


type alias LockScript =
    { hash : Bytes CredentialHash
    , compiledCode : Bytes ScriptCbor
    }


type alias AppContext =
    { loadedWallet : LoadedWallet
    , pickedUtxo : OutputReference
    , tokenName : Bytes AssetName
    , localStateUtxos : Utxo.RefDict Output
    , lockScript : LockScript
    , scriptAddress : Address
    }


type Action
    = Locking
    | Unlocking


init : () -> ( Model, Cmd Msg )
init _ =
    ( Startup
    , toWallet <| Cip30.encodeRequest Cip30.discoverWallets
    )


getAppContext : Model -> Maybe AppContext
getAppContext model =
    case model of
        ParametersSet appContext _ ->
            Just appContext

        Submitting appContext _ _ ->
            Just appContext

        TxSubmitted appContext _ _ ->
            Just appContext

        _ ->
            Nothing


addError : String -> Model -> Model
addError e model =
    let
        _ =
            Debug.log "ERROR" e
    in
    case model of
        WalletLoaded loadedWallet { errors } ->
            WalletLoaded loadedWallet { errors = errors ++ ", " ++ e }

        BlueprintLoaded loadedWallet unappliedScript { tokenName, errors } ->
            BlueprintLoaded loadedWallet unappliedScript { tokenName = tokenName, errors = errors ++ ", " ++ e }

        ParametersSet appContext { errors } ->
            ParametersSet appContext { errors = errors ++ ", " ++ e }

        Submitting appContext action { tx, errors } ->
            Submitting appContext action { tx = tx, errors = errors ++ ", " ++ e }

        TxSubmitted appContext action { txId, errors } ->
            TxSubmitted appContext action { txId = txId, errors = errors ++ ", " ++ e }

        _ ->
            model



-- #########################################################
-- UPDATE
-- #########################################################


type Msg
    = WalletMsg Value
    | ConnectButtonClicked { id : String }
    | LoadBlueprintButtonClicked
    | GotBlueprint (Result Http.Error UnappliedScript)
    | UpdateTokenName String
    | FinalizeTokenName
    | LockAdaButtonClicked
    | UnlockAdaButtonClicked


walletResponseDecoder : Decoder (Cip30.Response Cip30.ApiResponse)
walletResponseDecoder =
    Cip30.responseDecoder <|
        Dict.singleton 30 Cip30.apiDecoder


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( WalletMsg value, _ ) ->
            case ( JD.decodeValue walletResponseDecoder value, model ) of
                -- We just discovered available wallets
                ( Ok (Cip30.AvailableWallets wallets), Startup ) ->
                    ( WalletDiscovered wallets, Cmd.none )

                -- We just connected to the wallet, let’s ask for the available utxos
                ( Ok (Cip30.EnabledWallet wallet), WalletDiscovered _ ) ->
                    ( WalletLoading { wallet = wallet, utxos = [] }
                    , toWallet <| Cip30.encodeRequest <| Cip30.getUtxos wallet { amount = Nothing, paginate = Nothing }
                    )

                -- We just received the utxos, let’s ask for the main change address of the wallet
                ( Ok (Cip30.ApiResponse _ (Cip30.WalletUtxos utxos)), WalletLoading { wallet } ) ->
                    ( WalletLoading { wallet = wallet, utxos = utxos }
                    , toWallet (Cip30.encodeRequest (Cip30.getChangeAddress wallet))
                    )

                ( Ok (Cip30.ApiResponse _ (Cip30.ChangeAddress address)), WalletLoading { wallet, utxos } ) ->
                    ( WalletLoaded { wallet = wallet, utxos = Utxo.refDictFromList utxos, changeAddress = address } { errors = "" }
                    , Cmd.none
                    )

                ( Ok (Cip30.ApiResponse _ (Cip30.SignedTx vkeywitnesses)), Submitting ctx action { tx } ) ->
                    let
                        -- Update the signatures of the Tx with the wallet response
                        signedTx =
                            Tx.updateSignatures (\_ -> Just vkeywitnesses) tx
                    in
                    ( Submitting ctx action { tx = signedTx, errors = "" }
                    , toWallet (Cip30.encodeRequest (Cip30.submitTx ctx.loadedWallet.wallet signedTx))
                    )

                ( Ok (Cip30.ApiResponse _ (Cip30.SubmittedTx txId)), Submitting ({ loadedWallet } as ctx) action { tx } ) ->
                    let
                        -- Update the known UTxOs set after the given Tx is processed
                        { updatedState, spent, created } =
                            TxIntent.updateLocalState txId tx ctx.localStateUtxos

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

                ( Ok (Cip30.ApiError { info }), m ) ->
                    ( addError info m, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( ConnectButtonClicked { id }, WalletDiscovered _ ) ->
            ( model, toWallet (Cip30.encodeRequest (Cip30.enableWallet { id = id, extensions = [] })) )

        ( LoadBlueprintButtonClicked, WalletLoaded _ _ ) ->
            ( model
            , let
                blueprintDecoder : Decoder UnappliedScript
                blueprintDecoder =
                    JD.at [ "validators" ]
                        (JD.index 0
                            (JD.map UnappliedScript
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
                Ok unappliedScript ->
                    ( BlueprintLoaded w unappliedScript { tokenName = "", errors = "" }, Cmd.none )

                Err err ->
                    -- Handle error as needed
                    ( WalletLoaded w { errors = Debug.toString err }, Cmd.none )

        ( UpdateTokenName newTokenName, BlueprintLoaded w unappliedScript info ) ->
            ( BlueprintLoaded
                w
                unappliedScript
                { info | tokenName = newTokenName }
            , Cmd.none
            )

        ( FinalizeTokenName, BlueprintLoaded w unappliedScript { tokenName, errors } ) ->
            case List.head (Dict.Any.keys w.utxos) of
                Just headUtxo ->
                    let
                        tokenNameBytes =
                            case Bytes.fromHex tokenName of
                                Just hex ->
                                    hex

                                Nothing ->
                                    Bytes.fromText tokenName

                        appliedScriptRes =
                            Uplc.applyParamsToScript
                                [ Data.Bytes tokenNameBytes
                                , outputReferenceToData headUtxo
                                ]
                                (Script.plutusScriptFromBytes PlutusV3 unappliedScript.compiledCode)
                    in
                    case appliedScriptRes of
                        Ok plutusScript ->
                            let
                                hash =
                                    Script.hash (Script.Plutus plutusScript)
                            in
                            ( ParametersSet
                                { loadedWallet = w
                                , pickedUtxo = headUtxo
                                , tokenName = tokenNameBytes
                                , localStateUtxos = w.utxos
                                , lockScript =
                                    { hash = hash
                                    , compiledCode = Script.cborWrappedBytes plutusScript
                                    }
                                , scriptAddress =
                                    Address.Shelley
                                        { networkId = Testnet
                                        , paymentCredential = ScriptHash hash
                                        , stakeCredential = Nothing
                                        }
                                }
                                { errors = errors }
                            , Cmd.none
                            )

                        Err err ->
                            ( BlueprintLoaded w unappliedScript { tokenName = tokenName, errors = Debug.toString err }
                            , Cmd.none
                            )

                Nothing ->
                    ( BlueprintLoaded w unappliedScript { tokenName = tokenName, errors = "Selected wallet has no UTxOs." }
                    , Cmd.none
                    )

        ( LockAdaButtonClicked, ParametersSet ctx _ ) ->
            lock ctx

        ( LockAdaButtonClicked, TxSubmitted ctx _ _ ) ->
            lock ctx

        ( UnlockAdaButtonClicked, TxSubmitted ctx _ { txId } ) ->
            let
                ( burnIntent, burnValue ) =
                    makeMintBurnIntent ctx.lockScript ctx.tokenName False

                intents =
                    -- collect the gift ada
                    [ Spend <|
                        FromPlutusScript
                            -- The gift UTxO was the first output of the locking tx
                            { spentInput = OutputReference txId 0
                            , datumWitness = Nothing
                            , plutusScriptWitness =
                                { script = ( PlutusV3, Witness.ByValue ctx.lockScript.compiledCode )
                                , redeemerData = \_ -> Data.Constr Natural.zero []
                                , requiredSigners = []
                                }
                            }
                    , SendTo ctx.loadedWallet.changeAddress gift

                    -- Burn the NFT gift card
                    , Spend <|
                        FromWallet
                            { address = ctx.loadedWallet.changeAddress
                            , value = burnValue
                            , guaranteedUtxos = []
                            }
                    , burnIntent
                    ]
            in
            intents |> finalizeTx ctx (Just txId)

        _ ->
            ( model, Cmd.none )


gift : Cardano.Value.Value
gift =
    Cardano.Value.onlyLovelace (Natural.fromSafeString "10000000")


lock : AppContext -> ( Model, Cmd Msg )
lock ({ loadedWallet, scriptAddress, lockScript, pickedUtxo, tokenName } as ctx) =
    let
        ( mintIntent, mintValue ) =
            makeMintBurnIntent lockScript tokenName True

        -- Transaction locking a 10 ada gift at the script address,
        -- and minting an NFT at the wallet address.
        intents =
            -- Lock the gift ada in the contract
            [ Spend <|
                FromWallet
                    { address = loadedWallet.changeAddress
                    , value = gift

                    -- Also guarantee that the required UTxO is spent
                    , guaranteedUtxos = [ pickedUtxo ]
                    }

            -- Using "SendToOutput" instead of "SendTo" just to make sure
            -- this will be the first output of the transaction (index 0).
            -- So that we know which UTxO to spend in the unlocking transaction.
            , SendToOutput
                { address = scriptAddress
                , amount = gift
                , datumOption = Nothing
                , referenceScript = Nothing
                }

            -- Mint the NFT gift card
            , mintIntent
            , SendTo loadedWallet.changeAddress mintValue
            ]
    in
    intents |> finalizeTx ctx Nothing


finalizeTx : AppContext -> Maybe (Bytes TransactionId) -> List TxIntent -> ( Model, Cmd Msg )
finalizeTx ctx mPrevTxId intents =
    let
        action =
            case mPrevTxId of
                Just _ ->
                    Unlocking

                Nothing ->
                    Locking

        txAttempt =
            TxIntent.finalize ctx.localStateUtxos [] intents
    in
    case txAttempt of
        Ok { tx } ->
            let
                cleanTx =
                    Tx.updateSignatures (\_ -> Nothing) tx
            in
            ( Submitting ctx action { tx = cleanTx, errors = "" }
            , Cip30.signTx ctx.loadedWallet.wallet { partialSign = False } cleanTx
                |> Cip30.encodeRequest
                |> toWallet
            )

        Err err ->
            ( case mPrevTxId of
                Nothing ->
                    ParametersSet ctx { errors = TxIntent.errorToString err }

                Just txId ->
                    TxSubmitted ctx action { txId = txId, errors = Debug.toString err }
            , Cmd.none
            )


makeMintBurnIntent : LockScript -> Bytes AssetName -> Bool -> ( TxIntent, Cardano.Value.Value )
makeMintBurnIntent lockScript tokenName forMint =
    let
        mintQuantity =
            if forMint then
                Integer.one

            else
                Integer.negativeOne
    in
    ( MintBurn
        { policyId = lockScript.hash
        , assets = BytesMap.singleton tokenName mintQuantity
        , scriptWitness =
            Witness.Plutus
                { script = ( PlutusV3, Witness.ByValue lockScript.compiledCode )
                , redeemerData =
                    \_ ->
                        if forMint then
                            Data.Constr Natural.zero []

                        else
                            Data.Constr Natural.one []
                , requiredSigners = []
                }
        }
    , Cardano.Value.onlyToken lockScript.hash tokenName (Integer.toNatural mintQuantity)
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

        BlueprintLoaded loadedWallet unappliedScript { errors } ->
            div []
                (viewLoadedWallet loadedWallet
                    ++ [ div [] [ text <| "Unapplied script size (bytes): " ++ String.fromInt (Bytes.width unappliedScript.compiledCode) ]
                       , Html.label [ HA.for "token_name" ] [ text "Token name of the gift card auth NFT (auto hex detection): " ]
                       , Html.input [ HA.id "token_name", HE.onInput UpdateTokenName ] []
                       , button [ HE.onClick FinalizeTokenName ] [ text "Finalize Token Name" ]
                       , displayErrors errors
                       ]
                )

        ParametersSet ctx { errors } ->
            div []
                (viewLoadedWallet ctx.loadedWallet
                    ++ [ div [] [ text <| "☑️ Picked UTxO: " ++ (ctx.pickedUtxo |> (\u -> Bytes.toHex u.transactionId ++ "#" ++ String.fromInt u.outputIndex)) ]
                       , div [] [ text <| "☑️ Base16 (hex) formatted NFT token name: " ++ Bytes.toHex ctx.tokenName ]
                       , div [] [ text <| "Applied Script hash: " ++ Bytes.toHex ctx.lockScript.hash ]
                       , div [] [ text <| "Applied Script size (bytes): " ++ String.fromInt (Bytes.width ctx.lockScript.compiledCode) ]
                       , viewScriptCbor ctx.lockScript.compiledCode
                       , button [ onClick LockAdaButtonClicked ] [ text "Lock 10 ADA and mint gift card NFT" ]
                       , displayErrors errors
                       ]
                )

        Submitting ctx _ { errors } ->
            div []
                [ text "⌛ Submitting transaction..."
                , viewScriptCbor ctx.lockScript.compiledCode
                , displayErrors errors
                ]

        TxSubmitted { loadedWallet, lockScript } action { txId, errors } ->
            let
                actionButton =
                    case action of
                        Locking ->
                            button [ onClick UnlockAdaButtonClicked ] [ text "Unlock 10 ADA and burn NFT" ]

                        Unlocking ->
                            div [] []
            in
            div []
                (viewLoadedWallet loadedWallet
                    ++ [ viewScriptCbor lockScript.compiledCode
                       , displayErrors errors
                       , div [] [ Html.b [] [ text <| "🎉 Tx submitted! with ID: " ++ Bytes.toHex txId ] ]
                       , actionButton
                       ]
                )


displayErrors : String -> Html msg
displayErrors err =
    if err == "" then
        text ""

    else
        div [ HA.style "color" "red" ] [ Html.b [] [ text <| "ERRORS: " ], text err ]


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


viewScriptCbor : Bytes ScriptCbor -> Html Msg
viewScriptCbor compiledCode =
    div
        [ HA.style "max-width" "640px", HA.style "word-wrap" "break-word" ]
        [ text <| "Applied Script: "
        , Html.small [] [ text <| Bytes.toHex compiledCode ]
        ]
