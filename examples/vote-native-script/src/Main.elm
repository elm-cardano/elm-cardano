port module Main exposing (main)

import Browser
import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano exposing (CertificateIntent(..), CredentialWitness(..), Fee(..), ScriptWitness(..), SpendSource(..), TxIntent(..), VoterWitness(..), WitnessSource(..))
import Cardano.Address as Address exposing (Address, Credential(..), CredentialHash, NetworkId(..), StakeCredential(..))
import Cardano.Cip30 as Cip30
import Cardano.CoinSelection as CoinSelection
import Cardano.Gov exposing (ActionId, CostModels, Drep(..), Vote(..))
import Cardano.Script as Script exposing (NativeScript(..), PlutusVersion(..))
import Cardano.Transaction as Transaction exposing (Transaction)
import Cardano.Uplc as Uplc
import Cardano.Utxo as Utxo exposing (DatumOption(..), Output, OutputReference, TransactionId)
import Cardano.Value
import Cbor.Encode
import Dict.Any
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (height, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD exposing (Decoder, Value)
import Json.Encode as JE
import Natural exposing (Natural)


main =
    -- The main entry point of our app
    -- More info about that in the Browser package docs:
    -- https://package.elm-lang.org/packages/elm/browser/latest/
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.batch [ fromWallet WalletMsg, fromExternalApp ExternalAppMsg ]
        , view = view
        }


port toWallet : Value -> Cmd msg


port fromWallet : (Value -> msg) -> Sub msg


port toExternalApp : Value -> Cmd msg


port fromExternalApp : (Value -> msg) -> Sub msg



-- #########################################################
-- MODEL
-- #########################################################


type Model
    = Startup StartupLoadingState
    | Initialized AppContext { errors : List String }
    | Submitting AppContext Operation { tx : Transaction, errors : String }
    | TxSubmitted AppContext Operation { txId : Bytes TransactionId, errors : String }
    | FeeProviderSigning AppContext { tx : Transaction, errors : String }


{-| Initial loading state
-}
type alias StartupLoadingState =
    { protocolParams : Maybe ProtocolParams
    , proposals : Maybe (List ActiveProposal)
    , walletsDiscovered : List Cip30.WalletDescriptor
    , wallet : Maybe Cip30.Wallet
    , walletUtxos : Maybe (List Cip30.Utxo)
    , walletChangeAddress : Maybe Address
    , feeProvider : Maybe FeeProvider
    , errors : List String
    }


type alias ProtocolParams =
    { costModels : CostModels
    , drepDeposit : Natural
    }


type alias ActiveProposal =
    { id : ActionId
    , actionType : String
    }


type alias LoadedWallet =
    { wallet : Cip30.Wallet
    , utxos : Utxo.RefDict Output
    , changeAddress : Address
    }


type alias FeeProvider =
    { address : Address
    , utxos : Utxo.RefDict Output
    }


completeStartup : StartupLoadingState -> Maybe AppContext
completeStartup s =
    case ( ( s.protocolParams, s.proposals, s.feeProvider ), ( s.wallet, s.walletUtxos, s.walletChangeAddress ), s.errors ) of
        ( ( Just protocolParams, Just proposals, Just feeProvider ), ( Just wallet, Just utxos, Just changeAddress ), [] ) ->
            let
                -- Extract both parts from our wallet address
                -- Remark: to do it properly, we should return an error if the stake cred is not a vkey hash
                ( myKeyCred, myStakeKeyHash ) =
                    ( Address.extractPubKeyHash changeAddress
                        |> Maybe.withDefault (Bytes.dummy 28 "ERROR")
                    , Address.extractStakeKeyHash changeAddress
                        |> Maybe.withDefault (Bytes.dummy 28 "ERROR")
                    )

                -- Generate native script that checks stake credential
                govScript =
                    ScriptPubkey myStakeKeyHash

                walletUtxos =
                    Utxo.refDictFromList utxos
            in
            Just
                { protocolParams = protocolParams
                , feeProvider = feeProvider
                , localStateUtxos = Dict.Any.union walletUtxos feeProvider.utxos
                , loadedWallet = LoadedWallet wallet walletUtxos changeAddress
                , myKeyCred = myKeyCred
                , myStakeKeyHash = myStakeKeyHash
                , govNativeScript = { hash = Script.hash (Script.Native govScript), script = govScript }
                , proposals = List.map (\p -> { selected = False, proposal = p, vote = VoteAbstain }) proposals
                }

        _ ->
            Nothing


type alias AppContext =
    { protocolParams : ProtocolParams
    , feeProvider : FeeProvider
    , localStateUtxos : Utxo.RefDict Output
    , loadedWallet : LoadedWallet
    , myKeyCred : Bytes CredentialHash
    , myStakeKeyHash : Bytes CredentialHash
    , govNativeScript : { hash : Bytes CredentialHash, script : NativeScript }
    , proposals : List { selected : Bool, proposal : ActiveProposal, vote : Vote }
    }


type Operation
    = RegisteringDRep
    | Voting
    | UnregisteringDRep


init : () -> ( Model, Cmd Msg )
init _ =
    ( Startup
        { protocolParams = Nothing
        , proposals = Nothing
        , walletsDiscovered = []
        , wallet = Nothing
        , walletUtxos = Nothing
        , walletChangeAddress = Nothing
        , feeProvider = Nothing
        , errors = []
        }
    , Cmd.batch
        [ toWallet (Cip30.encodeRequest Cip30.discoverWallets)
        , loadProtocolParams
        , loadGovernanceProposals
        ]
    )


loadProtocolParams : Cmd Msg
loadProtocolParams =
    Http.post
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


loadGovernanceProposals : Cmd Msg
loadGovernanceProposals =
    Http.post
        { url = "https://preview.koios.rest/api/v1/ogmios"
        , body =
            Http.jsonBody
                (JE.object
                    [ ( "jsonrpc", JE.string "2.0" )
                    , ( "method", JE.string "queryLedgerState/governanceProposals" )
                    ]
                )
        , expect = Http.expectJson GotProposals proposalsDecoder
        }


proposalsDecoder : Decoder (List ActiveProposal)
proposalsDecoder =
    JD.field "result" <|
        JD.list <|
            JD.map2 ActiveProposal
                (JD.map2
                    (\id index ->
                        { transactionId = Bytes.fromHexUnchecked id
                        , govActionIndex = index
                        }
                    )
                    (JD.at [ "proposal", "transaction", "id" ] JD.string)
                    (JD.at [ "proposal", "index" ] JD.int)
                )
                (JD.at [ "action", "type" ] JD.string)



-- #########################################################
-- UPDATE
-- #########################################################


type Msg
    = WalletMsg Value
    | ExternalAppMsg Value
    | ConnectButtonClicked { id : String }
    | RequestExternalUtxosClicked
    | GotProtocolParams (Result Http.Error ProtocolParams)
    | GotProposals (Result Http.Error (List ActiveProposal))
    | RegisterDRepButtonClicked
    | SkipRegisterButtonClicked
    | ProposalSelectionChanged ActionId Bool
    | ProposalVoteChanged ActionId String
    | VoteButtonClicked
    | UnregisterDRepButtonClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotProtocolParams result, Startup state ) ->
            case result of
                Ok params ->
                    let
                        newState =
                            { state | protocolParams = Just params }
                    in
                    completeStartup newState
                        |> Maybe.map (\appContext -> ( Initialized appContext { errors = [] }, Cmd.none ))
                        |> Maybe.withDefault ( Startup newState, Cmd.none )

                Err err ->
                    ( Startup { state | errors = Debug.toString err :: state.errors }, Cmd.none )

        ( GotProposals result, Startup state ) ->
            case result of
                Ok activeProposals ->
                    let
                        newState =
                            { state | proposals = Just activeProposals }
                    in
                    completeStartup newState
                        |> Maybe.map (\appContext -> ( Initialized appContext { errors = [] }, Cmd.none ))
                        |> Maybe.withDefault ( Startup newState, Cmd.none )

                Err err ->
                    ( Startup { state | errors = Debug.toString err :: state.errors }, Cmd.none )

        ( ConnectButtonClicked { id }, _ ) ->
            ( model, toWallet (Cip30.encodeRequest (Cip30.enableWallet { id = id, extensions = [] })) )

        ( WalletMsg value, Startup state ) ->
            case JD.decodeValue Cip30.responseDecoder value of
                -- We just discovered available wallets
                Ok (Cip30.AvailableWallets wallets) ->
                    ( Startup { state | walletsDiscovered = wallets }
                    , Cmd.none
                    )

                -- We just connected to the wallet, let’s ask for all that is still missing
                Ok (Cip30.EnabledWallet wallet) ->
                    ( Startup { state | wallet = Just wallet }
                    , Cmd.batch
                        -- Retrieve UTXOs from the main wallet
                        [ Cip30.getUtxos wallet { amount = Nothing, paginate = Nothing }
                            |> Cip30.encodeRequest
                            |> toWallet

                        -- Retrieve the wallet change address
                        , toWallet (Cip30.encodeRequest (Cip30.getChangeAddress wallet))

                        -- Request UTXOs from the fee provider
                        , toExternalApp (JE.object [ ( "requestType", JE.string "ask-utxos" ) ])
                        ]
                    )

                -- We just received the utxos
                Ok (Cip30.ApiResponse _ (Cip30.WalletUtxos utxos)) ->
                    let
                        newState =
                            { state | walletUtxos = Just utxos }
                    in
                    completeStartup newState
                        |> Maybe.map (\appContext -> ( Initialized appContext { errors = [] }, Cmd.none ))
                        |> Maybe.withDefault ( Startup newState, Cmd.none )

                -- Received the wallet change address
                Ok (Cip30.ApiResponse _ (Cip30.ChangeAddress address)) ->
                    let
                        newState =
                            { state | walletChangeAddress = Just address }
                    in
                    completeStartup newState
                        |> Maybe.map (\appContext -> ( Initialized appContext { errors = [] }, Cmd.none ))
                        |> Maybe.withDefault ( Startup newState, Cmd.none )

                -- No other wallet message is allowed at startup
                _ ->
                    ( model, Cmd.none )

        ( ExternalAppMsg value, Startup state ) ->
            case JD.decodeValue externalAppResponseDecoder value of
                -- Received the fee provider info
                Ok (GotUtxos ( addr, utxos )) ->
                    let
                        newState =
                            { state | feeProvider = Just { address = addr, utxos = Utxo.refDictFromList utxos } }
                    in
                    completeStartup newState
                        |> Maybe.map (\appContext -> ( Initialized appContext { errors = [] }, Cmd.none ))
                        |> Maybe.withDefault ( Startup newState, Cmd.none )

                Err err ->
                    let
                        _ =
                            Debug.log "Error decoding external app response:" (Debug.toString err)
                    in
                    ( model, Cmd.none )

                -- No other fee provider message is allowed at startup
                _ ->
                    ( model, Cmd.none )

        -- Dealing with Tx signature and submission
        ( WalletMsg value, Submitting ({ loadedWallet, feeProvider } as ctx) action { tx } ) ->
            case JD.decodeValue Cip30.responseDecoder value of
                Ok (Cip30.ApiResponse _ (Cip30.SignedTx vkeywitnesses)) ->
                    let
                        -- Update the signatures of the Tx with the wallet response
                        signedTx =
                            Transaction.updateSignatures (Just << List.append vkeywitnesses << Maybe.withDefault []) tx

                        _ =
                            Debug.log "signedTx" (Transaction.serialize signedTx)
                    in
                    ( Submitting ctx action { tx = signedTx, errors = "" }
                    , toWallet (Cip30.encodeRequest (Cip30.submitTx loadedWallet.wallet signedTx))
                    )

                Ok (Cip30.ApiResponse _ (Cip30.SubmittedTx txId)) ->
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

                        -- Also update specifically our fee provider wallet UTxOs knowledge
                        -- This isn’t purely necessary, but just to keep a consistent wallet state
                        unspentProviderUtxos =
                            List.foldl (\( ref, _ ) state -> Dict.Any.remove ref state) feeProvider.utxos spent

                        updatedFeeProviderUtxos =
                            List.foldl
                                (\( ref, output ) state ->
                                    if output.address == feeProvider.address then
                                        Dict.Any.insert ref output state

                                    else
                                        state
                                )
                                unspentProviderUtxos
                                created

                        updatedContext =
                            { ctx
                                | localStateUtxos = updatedState
                                , loadedWallet = { loadedWallet | utxos = updatedWalletUtxos }
                                , feeProvider = { feeProvider | utxos = updatedFeeProviderUtxos }
                            }
                    in
                    ( TxSubmitted updatedContext action { txId = txId, errors = "" }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        -- Fee provider signatures
        ( ExternalAppMsg value, FeeProviderSigning ctx { tx } ) ->
            case JD.decodeValue externalAppResponseDecoder value of
                Ok (GotSignature witnesses) ->
                    let
                        txWithFeeWitness =
                            Transaction.updateSignatures (\_ -> Just witnesses) tx
                    in
                    ( Submitting ctx Voting { tx = txWithFeeWitness, errors = "" }
                      -- False partial sign
                    , toWallet (Cip30.encodeRequest (Cip30.signTx ctx.loadedWallet.wallet { partialSign = True } txWithFeeWitness))
                    )

                Err err ->
                    let
                        _ =
                            Debug.log "Error decoding external app response:" (Debug.toString err)
                    in
                    ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( SkipRegisterButtonClicked, Initialized context _ ) ->
            ( TxSubmitted context RegisteringDRep { txId = Bytes.dummy 32 "", errors = "" }
            , Cmd.none
            )

        ( RegisterDRepButtonClicked, Initialized ctx _ ) ->
            let
                -- Create registration certificate for the script address as DRep
                regDRepTxAttempt =
                    [ Spend <|
                        FromWallet
                            { address = ctx.loadedWallet.changeAddress
                            , value = Cardano.Value.onlyLovelace ctx.protocolParams.drepDeposit
                            , guaranteedUtxos = []
                            }
                    , IssueCertificate <|
                        RegisterDrep
                            { drep =
                                WithScript ctx.govNativeScript.hash <|
                                    NativeWitness
                                        { script = WitnessValue ctx.govNativeScript.script
                                        , expectedSigners = [ ctx.myStakeKeyHash ]
                                        }
                            , deposit = ctx.protocolParams.drepDeposit
                            , info = Nothing
                            }
                    ]
                        |> Cardano.finalizeAdvanced
                            { govState = Cardano.emptyGovernanceState
                            , localStateUtxos = ctx.localStateUtxos
                            , coinSelectionAlgo = CoinSelection.largestFirst
                            , evalScriptsCosts = Uplc.evalScriptsCosts Uplc.defaultVmConfig
                            , costModels = ctx.protocolParams.costModels
                            }
                            (AutoFee { paymentSource = ctx.loadedWallet.changeAddress })
                            []
            in
            case regDRepTxAttempt of
                Ok { tx } ->
                    ( Submitting ctx RegisteringDRep { tx = tx, errors = "" }
                      -- partialSign = False -- Full sign failure with Eternl
                    , toWallet (Cip30.encodeRequest (Cip30.signTx ctx.loadedWallet.wallet { partialSign = True } tx))
                    )

                Err err ->
                    ( Initialized ctx { errors = [ Debug.toString err ] }, Cmd.none )

        -- Recording the selected votes
        ( ProposalSelectionChanged actionId isSelected, TxSubmitted ctx action rest ) ->
            let
                updatedProposals =
                    ctx.proposals
                        |> List.map
                            (\p ->
                                if p.proposal.id == actionId then
                                    { p | selected = isSelected }

                                else
                                    p
                            )
            in
            ( TxSubmitted { ctx | proposals = updatedProposals } action rest
            , Cmd.none
            )

        -- Recording the vote choices
        ( ProposalVoteChanged actionId voteStr, TxSubmitted ctx action rest ) ->
            let
                updatedProposals =
                    ctx.proposals
                        |> List.map
                            (\p ->
                                if p.proposal.id == actionId then
                                    { p | vote = stringToVote voteStr }

                                else
                                    p
                            )
            in
            ( TxSubmitted { ctx | proposals = updatedProposals } action rest
            , Cmd.none
            )

        ( VoteButtonClicked, TxSubmitted ctx action { txId } ) ->
            let
                -- Create voting transaction using the script
                voteTxAttempt =
                    [ Vote
                        (WithDrepCred <|
                            WithScript ctx.govNativeScript.hash <|
                                NativeWitness
                                    { script = WitnessValue ctx.govNativeScript.script
                                    , expectedSigners = [ ctx.myStakeKeyHash ]
                                    }
                        )
                        (ctx.proposals
                            |> List.filterMap
                                (\{ selected, proposal, vote } ->
                                    if selected then
                                        Just { actionId = proposal.id, vote = vote, rationale = Nothing }

                                    else
                                        Nothing
                                )
                        )
                    ]
                        |> Cardano.finalizeAdvanced
                            { govState = Cardano.emptyGovernanceState
                            , localStateUtxos = ctx.localStateUtxos
                            , coinSelectionAlgo = CoinSelection.largestFirst
                            , evalScriptsCosts = Uplc.evalScriptsCosts Uplc.defaultVmConfig
                            , costModels = ctx.protocolParams.costModels
                            }
                            (AutoFee { paymentSource = ctx.feeProvider.address })
                            []
            in
            case voteTxAttempt of
                Ok { tx } ->
                    ( FeeProviderSigning ctx { tx = tx, errors = "" }
                    , toExternalApp
                        (JE.object
                            [ ( "requestType", JE.string "ask-signature" )
                            , ( "tx", JE.string <| Bytes.toHex <| Transaction.serialize tx )
                            ]
                        )
                    )

                Err err ->
                    ( TxSubmitted ctx action { txId = txId, errors = Debug.toString err }
                    , Cmd.none
                    )

        ( UnregisterDRepButtonClicked, TxSubmitted ctx action { txId } ) ->
            let
                refund =
                    -- MVP. In theory, we should check the amount in ledger
                    ctx.protocolParams.drepDeposit

                -- Create unregistration transaction
                unregDRepTxAttempt =
                    [ IssueCertificate <|
                        UnregisterDrep
                            { drep =
                                WithScript ctx.govNativeScript.hash <|
                                    NativeWitness
                                        { script = WitnessValue ctx.govNativeScript.script
                                        , expectedSigners = [ ctx.myStakeKeyHash ]
                                        }
                            , refund = refund
                            }
                    , SendTo ctx.loadedWallet.changeAddress <|
                        Cardano.Value.onlyLovelace refund
                    ]
                        |> Cardano.finalizeAdvanced
                            { govState = Cardano.emptyGovernanceState
                            , localStateUtxos = ctx.localStateUtxos
                            , coinSelectionAlgo = CoinSelection.largestFirst
                            , evalScriptsCosts = Uplc.evalScriptsCosts Uplc.defaultVmConfig
                            , costModels = ctx.protocolParams.costModels
                            }
                            (AutoFee { paymentSource = ctx.loadedWallet.changeAddress })
                            []
            in
            case unregDRepTxAttempt of
                Ok { tx } ->
                    ( Submitting ctx UnregisteringDRep { tx = tx, errors = "" }
                      -- partialSign = False -- Full sign error with eternl
                    , toWallet (Cip30.encodeRequest (Cip30.signTx ctx.loadedWallet.wallet { partialSign = True } tx))
                    )

                Err err ->
                    ( TxSubmitted ctx action { txId = txId, errors = Debug.toString err }
                    , Cmd.none
                    )

        _ ->
            ( model, Cmd.none )


type ExternalAppResponse
    = GotUtxos ( Address, List ( OutputReference, Output ) )
    | GotSignature (List Transaction.VKeyWitness)


externalAppResponseDecoder : Decoder ExternalAppResponse
externalAppResponseDecoder =
    JD.field "responseType" JD.string
        |> JD.andThen
            (\responseType ->
                case responseType of
                    "utxos" ->
                        JD.map2 (\addr utxos -> GotUtxos ( addr, utxos ))
                            (JD.field "address" Cip30.addressDecoder)
                            (JD.field "utxos" (JD.list Cip30.utxoDecoder))

                    "signature" ->
                        JD.field "witnesses" (JD.list (Cip30.hexCborDecoder Transaction.decodeVKeyWitness))
                            |> JD.map GotSignature

                    _ ->
                        JD.fail "Unknown response type"
            )


protocolParamsDecoder : Decoder ProtocolParams
protocolParamsDecoder =
    JD.map4
        (\v1 v2 v3 drepDeposit ->
            { costModels = CostModels (Just v1) (Just v2) (Just v3)
            , drepDeposit = drepDeposit
            }
        )
        (JD.at [ "result", "plutusCostModels", "plutus:v1" ] <| JD.list JD.int)
        (JD.at [ "result", "plutusCostModels", "plutus:v2" ] <| JD.list JD.int)
        (JD.at [ "result", "plutusCostModels", "plutus:v3" ] <| JD.list JD.int)
        (JD.at [ "result", "delegateRepresentativeDeposit", "ada", "lovelace" ] <| JD.map Natural.fromSafeInt JD.int)


stringToVote : String -> Vote
stringToVote str =
    -- Helper function to convert String to Vote
    case str of
        "yes" ->
            VoteYes

        "no" ->
            VoteNo

        _ ->
            VoteAbstain



-- #########################################################
-- VIEW
-- #########################################################


view : Model -> Html Msg
view model =
    case model of
        Startup state ->
            div []
                [ viewMaybeWallet state
                , viewProtocolParams state.protocolParams
                , viewProposalLoading state.proposals
                , div [ Html.Attributes.style "height" "2em" ] []
                , displayErrors <| String.join "\n" state.errors
                ]

        Initialized ctx { errors } ->
            div []
                [ viewLoadedWallet ctx.loadedWallet
                , button [ onClick RegisterDRepButtonClicked ] [ text "Register Script as DRep" ]
                , button [ onClick SkipRegisterButtonClicked ] [ text "Skip registration" ]
                , div [ Html.Attributes.style "height" "2em" ] []
                , displayErrors <| String.join "\n" errors
                ]

        Submitting { loadedWallet } action { errors } ->
            div []
                [ viewLoadedWallet loadedWallet
                , div [] [ text <| "Signing and submitting the " ++ Debug.toString action ++ " transaction ..." ]
                , div [ Html.Attributes.style "height" "2em" ] []
                , displayErrors errors
                ]

        TxSubmitted { loadedWallet, proposals } previousAction { txId, errors } ->
            let
                actionArea =
                    case previousAction of
                        RegisteringDRep ->
                            let
                                viewProposals =
                                    List.map viewProposal proposals
                            in
                            div []
                                ([ div [] [ text "Active proposals (oldest to newest):" ] ]
                                    ++ viewProposals
                                    ++ [ button [ onClick VoteButtonClicked ] [ text "Vote" ] ]
                                )

                        Voting ->
                            button [ onClick UnregisterDRepButtonClicked ] [ text "Unregister the script DRep" ]

                        UnregisteringDRep ->
                            div [] [ text "All done!" ]
            in
            div []
                [ viewLoadedWallet loadedWallet
                , div [] [ text <| "Tx submitted! with ID: " ++ Bytes.toHex txId ]
                , div [ Html.Attributes.style "height" "2em" ] []
                , actionArea
                , div [ Html.Attributes.style "height" "2em" ] []
                , displayErrors errors
                ]

        FeeProviderSigning { loadedWallet } { errors } ->
            div []
                [ viewLoadedWallet loadedWallet
                , div [] [ text <| "Signing transaction with the fee provider ..." ]
                , div [ Html.Attributes.style "height" "2em" ] []
                , displayErrors errors
                ]


viewMaybeWallet : StartupLoadingState -> Html Msg
viewMaybeWallet { walletsDiscovered, wallet, walletUtxos, walletChangeAddress, feeProvider } =
    case ( walletsDiscovered, wallet ) of
        ( [], Nothing ) ->
            div [] [ text "Discovering available wallets ..." ]

        ( _, Nothing ) ->
            div []
                [ div [] [ text "Select Main wallet in the detected CIP-30 wallets:" ]
                , viewAvailableWallets walletsDiscovered
                ]

        ( _, Just cip30Wallet ) ->
            let
                viewUtxoCount =
                    case walletUtxos of
                        Nothing ->
                            div [] [ text "Loading wallet UTxOs ..." ]

                        Just utxos ->
                            div [] [ text <| "UTxO count: " ++ String.fromInt (List.length utxos) ]

                viewChangeAddress =
                    case walletChangeAddress of
                        Nothing ->
                            div [] [ text "Loading wallet change address ..." ]

                        Just changeAddress ->
                            div [] [ text <| "Address: " ++ (Address.toBytes changeAddress |> Bytes.toHex) ]

                viewFeeProvider =
                    case feeProvider of
                        Nothing ->
                            div [] [ text "Loading fee provider ..." ]

                        Just { address } ->
                            div [] [ text <| "Fee provider address: " ++ (Address.toBytes address |> Bytes.toHex) ]
            in
            div []
                [ div [] [ text <| "Main Wallet: " ++ (Cip30.walletDescriptor cip30Wallet).name ]
                , viewChangeAddress
                , viewUtxoCount
                , viewFeeProvider
                ]


viewProtocolParams : Maybe ProtocolParams -> Html Msg
viewProtocolParams maybeParams =
    case maybeParams of
        Nothing ->
            div [] [ text "Loading protocol parameters ..." ]

        Just _ ->
            div [] [ text "Protocol params loaded!" ]


viewProposalLoading : Maybe (List ActiveProposal) -> Html Msg
viewProposalLoading maybeProposals =
    case maybeProposals of
        Nothing ->
            div [] [ text "Loading active proposals ..." ]

        Just proposals ->
            div [] [ text <| "Loaded " ++ String.fromInt (List.length proposals) ++ " active proposals" ]


displayErrors : String -> Html msg
displayErrors err =
    if err == "" then
        text ""

    else
        div [] [ text <| "ERRORS: " ++ err ]


viewLoadedWallet : LoadedWallet -> Html msg
viewLoadedWallet { wallet, utxos, changeAddress } =
    div []
        [ div [] [ text <| "Main Wallet: " ++ (Cip30.walletDescriptor wallet).name ]
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


viewProposal : { selected : Bool, proposal : ActiveProposal, vote : Vote } -> Html Msg
viewProposal { selected, proposal, vote } =
    div []
        [ Html.input
            [ Html.Attributes.type_ "checkbox"
            , Html.Attributes.checked selected
            , Html.Events.onCheck (ProposalSelectionChanged proposal.id)
            ]
            []
        , text <| "Action: " ++ proposal.actionType ++ ", "
        , Html.a
            [ Html.Attributes.href <|
                "https://preview.cardanoscan.io/govAction/"
                    ++ (proposal.id.transactionId |> Bytes.toHex)
                    ++ (Bytes.toHex <| Bytes.fromBytes <| Cbor.Encode.encode (Cbor.Encode.int proposal.id.govActionIndex))
            , Html.Attributes.target "_blank"
            , Html.Attributes.rel "noopener noreferrer"
            ]
            [ text <| "ID: " ++ (proposal.id.transactionId |> Bytes.toHex)
            , text <| " #" ++ String.fromInt proposal.id.govActionIndex
            ]
        , text ", Vote: "
        , Html.select
            [ Html.Attributes.value (voteToString vote)
            , Html.Events.onInput (ProposalVoteChanged proposal.id)
            ]
            [ Html.option [ Html.Attributes.value (voteToString VoteYes), Html.Attributes.selected (vote == VoteYes) ] [ text "Yes" ]
            , Html.option [ Html.Attributes.value (voteToString VoteNo), Html.Attributes.selected (vote == VoteNo) ] [ text "No" ]
            , Html.option [ Html.Attributes.value (voteToString VoteAbstain), Html.Attributes.selected (vote == VoteAbstain) ] [ text "Abstain" ]
            ]
        ]


voteToString : Vote -> String
voteToString vote =
    case vote of
        VoteYes ->
            "yes"

        VoteNo ->
            "no"

        VoteAbstain ->
            "abstain"
