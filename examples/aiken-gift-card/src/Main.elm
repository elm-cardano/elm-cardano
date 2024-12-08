port module Main exposing (..)

import Browser
import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Map as BytesMap
import Cardano exposing (ScriptWitness(..), SpendSource(..), TxIntent(..), WitnessSource(..), dummyBytes)
import Cardano.Address as Address exposing (Address, Credential(..), CredentialHash, NetworkId(..))
import Cardano.Cip30 as Cip30
import Cardano.Cip67 exposing (AssetName)
import Cardano.CoinSelection as CoinSelection
import Cardano.Data as Data
import Cardano.Gov exposing (CostModels)
import Cardano.Script as Script exposing (PlutusVersion(..), ScriptCbor)
import Cardano.Transaction as Tx exposing (Transaction)
import Cardano.Uplc as Uplc
import Cardano.Utxo as Utxo exposing (DatumOption(..), Output, OutputReference, TransactionId, outputReferenceToData)
import Cardano.Value
import Dict.Any
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (height, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD exposing (Decoder, Value)
import Json.Encode as JE
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


type CostModelsState
    = FetchingCostModels
    | CostModelsFailed String
    | CostModelsLoaded ProtocolParams


type alias ProtocolParams =
    { costModels : CostModels }


type alias AppContext =
    { loadedWallet : LoadedWallet
    , pickedUtxo : OutputReference
    , tokenName : Bytes AssetName
    , localStateUtxos : Utxo.RefDict Output
    , lockScript : LockScript
    , scriptAddress : Address
    , costModels : CostModelsState
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
    | GotProtocolParams (Result Http.Error ProtocolParams)
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
                                { version = PlutusV3
                                , script = unappliedScript.compiledCode
                                }
                    in
                    case appliedScriptRes of
                        Ok plutusScript ->
                            let
                                scriptHash =
                                    Script.hash (Script.Plutus plutusScript)
                            in
                            ( ParametersSet
                                { loadedWallet = w
                                , pickedUtxo = headUtxo
                                , tokenName = tokenNameBytes
                                , localStateUtxos = w.utxos
                                , lockScript =
                                    { hash = scriptHash
                                    , compiledCode = plutusScript.script
                                    }
                                , scriptAddress =
                                    Address.Shelley
                                        { networkId = Testnet
                                        , paymentCredential = ScriptHash scriptHash
                                        , stakeCredential = Nothing
                                        }
                                , costModels = FetchingCostModels
                                }
                                { errors = errors }
                            , Http.post
                                { url = "https://preview.koios.rest/api/v1/ogmios"
                                , body =
                                    Http.jsonBody
                                        (JE.object
                                            [ ( "jsonrpc", JE.string "2.0" )
                                            , ( "method", JE.string "queryLedgerState/protocolParameters" )
                                            ]
                                        )
                                , expect = Http.expectJson GotProtocolParams protocolParamsDecoder
                                }
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

        ( UnlockAdaButtonClicked, TxSubmitted ctx action { txId } ) ->
            let
                twoAda =
                    Cardano.Value.onlyLovelace (Natural.fromSafeString "2000000")

                redeemer =
                    Data.Constr Natural.zero [ Data.Bytes (Bytes.fromText "Hello, World!") ]

                unlockTxAttempt =
                    [ Spend
                        (FromPlutusScript
                            -- The previously sent UTxO was the first output of the locking Tx
                            { spentInput = OutputReference txId 0
                            , datumWitness = Nothing
                            , plutusScriptWitness =
                                { script = ( PlutusV3, WitnessValue ctx.lockScript.compiledCode )
                                , redeemerData = \_ -> redeemer
                                , requiredSigners = [ ctx.myKeyCred ]
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
lock ({ localStateUtxos, scriptAddress, loadedWallet, lockScript, pickedUtxo, tokenName, costModels } as ctx) =
    let
        -- A valid UTxO must contain exactly one NFT with the script's policy
        -- and specified token name.
        -- twoAdaAndNFT =
        --     Cardano.Value.add
        --         (Cardano.Value.onlyLovelace (Natural.fromSafeString "2000000"))
        --         (Cardano.Value.onlyToken lockScript.hash tokenName Natural.one)
        -- Transaction locking 2 ada, plust the minted NFT at connected wallet's
        -- address, and locking 50 ada at the script address.
        lockTxAttempt =
            [ Spend (FromWalletUtxo pickedUtxo)
            , MintBurn
                { policyId = lockScript.hash
                , assets = BytesMap.singleton tokenName 1
                , scriptWitness =
                    PlutusWitness
                        { script = ( PlutusV3, WitnessValue lockScript.compiledCode )
                        , redeemerData = \_ -> Data.Constr Natural.zero []
                        , requiredSigners = []
                        }
                }

            -- , SendToOutput
            --     { address = loadedWallet.changeAddress
            --     , amount = twoAdaAndNFT
            --     , datumOption = Nothing
            --     , referenceScript = Nothing
            --     }
            , SendToOutput
                { address = scriptAddress
                , amount = Cardano.Value.onlyLovelace (Natural.fromSafeString "50000000")
                , datumOption = Nothing
                , referenceScript = Nothing
                }
            ]
                |> (case costModels of
                        CostModelsLoaded protocolParams ->
                            Cardano.finalizeAdvanced
                                { govState = Cardano.emptyGovernanceState
                                , localStateUtxos = localStateUtxos
                                , coinSelectionAlgo = CoinSelection.largestFirst
                                , evalScriptsCosts = Uplc.evalScriptsCosts Uplc.defaultVmConfig
                                , costModels = protocolParams.costModels
                                }
                                (Cardano.AutoFee { paymentSource = loadedWallet.changeAddress })
                                []

                        _ ->
                            Cardano.finalize localStateUtxos []
                   )
    in
    case lockTxAttempt of
        Ok lockTx ->
            let
                cleanTx =
                    Tx.updateSignatures (\_ -> Nothing) lockTx
            in
            ( Submitting ctx Locking { tx = cleanTx, errors = "" }
            , Cip30.signTx loadedWallet.wallet { partialSign = False } cleanTx
                |> Cip30.encodeRequest
                |> toWallet
            )

        Err err ->
            ( ParametersSet ctx { errors = Debug.toString err }
            , Cmd.none
            )


protocolParamsDecoder : Decoder ProtocolParams
protocolParamsDecoder =
    JD.map3 (\v1 v2 v3 -> { costModels = CostModels (Just v1) (Just v2) (Just v3) })
        (JD.at [ "result", "plutusCostModels", "plutus:v1" ] <| JD.list JD.int)
        (JD.at [ "result", "plutusCostModels", "plutus:v2" ] <| JD.list JD.int)
        (JD.at [ "result", "plutusCostModels", "plutus:v3" ] <| JD.list JD.int)



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
                    ++ [ div [] [ text <| "Unapplied script size (bytes): " ++ String.fromInt (Bytes.width unappliedScript) ]
                       , button [ onClick LockAdaButtonClicked ] [ text "Lock 2 ADA" ]
                       , displayErrors errors
                       ]
                )

        ParametersSet ctx { errors } ->
            div [] []

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
