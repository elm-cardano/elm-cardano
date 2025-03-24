port module Main exposing (..)

import Browser
import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano exposing (SpendSource(..), TxIntent(..), WitnessSource(..))
import Cardano.Address as Address exposing (Address, Credential(..), CredentialHash, NetworkId(..))
import Cardano.Cip30 as Cip30
import Cardano.Data as Data
import Cardano.MultiAsset exposing (AssetName)
import Cardano.Script exposing (PlutusVersion(..), ScriptCbor)
import Cardano.Transaction as Tx exposing (Transaction)
import Cardano.Utxo as Utxo exposing (DatumOption(..), Output, OutputReference, TransactionId)
import Cardano.Value as Value
import Dict.Any
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (height, src)
import Html.Events exposing (onClick)
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


tokenPolicyId : Bytes CredentialHash
tokenPolicyId =
    Bytes.fromHexUnchecked "2fe3c3364b443194b10954771c95819b8d6ed464033c21f03f8facb5"


tokenName : Bytes AssetName
tokenName =
    Bytes.fromText "iUSD"



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
    | Submitting AppContext { tx : Transaction, errors : String }
    | TxSubmitted AppContext { txId : Bytes TransactionId, errors : String }


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
    , myStakeCred : Maybe Address.StakeCredential
    , localStateUtxos : Utxo.RefDict Output
    , lockScript : LockScript
    , scriptAddress : Address
    }


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
    | CreateBucketButtonClicked
    | ReuseBucketButtonClicked


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
                ( Ok (Cip30.ApiResponse _ (Cip30.WalletUtxos utxos)), WalletLoading { wallet } ) ->
                    ( WalletLoading { wallet = wallet, utxos = utxos }
                    , toWallet (Cip30.encodeRequest (Cip30.getChangeAddress wallet))
                    )

                ( Ok (Cip30.ApiResponse _ (Cip30.ChangeAddress address)), WalletLoading { wallet, utxos } ) ->
                    ( WalletLoaded { wallet = wallet, utxos = Utxo.refDictFromList utxos, changeAddress = address } { errors = "" }
                    , Cmd.none
                    )

                ( Ok (Cip30.ApiResponse _ (Cip30.SignedTx vkeywitnesses)), Submitting ctx { tx } ) ->
                    let
                        -- Update the signatures of the Tx with the wallet response
                        signedTx =
                            Tx.updateSignatures (\_ -> Just vkeywitnesses) tx
                    in
                    ( Submitting ctx { tx = signedTx, errors = "" }
                    , toWallet (Cip30.encodeRequest (Cip30.submitTx ctx.loadedWallet.wallet signedTx))
                    )

                ( Ok (Cip30.ApiResponse _ (Cip30.SubmittedTx txId)), Submitting ({ loadedWallet } as ctx) { tx } ) ->
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
                    ( TxSubmitted updatedContext { txId = txId, errors = "" }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ( ConnectButtonClicked { id }, WalletDiscovered _ ) ->
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

        ( CreateBucketButtonClicked, BlueprintLoaded w lockScript _ ) ->
            let
                -- Extract both parts (payment/stake) from our wallet address
                ( myKeyCred, myStakeCred ) =
                    ( Address.extractPubKeyHash w.changeAddress
                        |> Maybe.withDefault (Bytes.dummy 28 "ERROR")
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
            createBucket
                { localStateUtxos = w.utxos
                , myKeyCred = myKeyCred
                , myStakeCred = myStakeCred
                , scriptAddress = scriptAddress
                , loadedWallet = w
                , lockScript = lockScript
                }

        ( CreateBucketButtonClicked, TxSubmitted ctx _ ) ->
            createBucket ctx

        ( ReuseBucketButtonClicked, TxSubmitted ctx { txId } ) ->
            let
                -- The previously sent UTxO was the first output of the bucket creation Tx
                bucketRef =
                    OutputReference txId 0

                spentBucket =
                    -- The output was the first one of the "create bucket" Tx
                    case Dict.Any.get bucketRef ctx.localStateUtxos of
                        Just b ->
                            b

                        -- The "case ... of" is necessary to make this Debug.todo lazy
                        -- otherwise Maybe.withDefault (Debug.todo "") will make the app crash
                        Nothing ->
                            Debug.todo "should be there in the updated state"

                -- Extract the bucket owner from its datum
                extractOwner bucket =
                    case bucket.datumOption of
                        -- Just (DatumValue (Data.Constr _ (owner :: _))) ->
                        Just (DatumValue { rawBytes }) ->
                            case Data.fromBytes rawBytes of
                                Just (Data.Constr _ (owner :: _)) ->
                                    Just owner

                                _ ->
                                    Nothing

                        _ ->
                            Nothing

                bucketOwner =
                    extractOwner spentBucket

                -- Let’s add 1 token to that bucket
                bucketValueIncrease =
                    Value.onlyToken tokenPolicyId tokenName Natural.one

                -- The datum of the new output bucket should contain the same owner,
                -- and the index of the input utxo of the corresponding bucket that is spent.
                outputBucket { inputs } =
                    { address = ctx.scriptAddress
                    , amount = Value.add spentBucket.amount bucketValueIncrease
                    , datumOption =
                        let
                            -- The input index is easy to find, just look for the correct ref in inputs
                            input_bucket_index =
                                findIndex (\ref -> ref == bucketRef) inputs
                                    -- Put a huge default, just to make sure it’s found
                                    |> Maybe.withDefault 7000
                                    |> Integer.fromSafeInt
                                    |> Data.Int
                        in
                        bucketOwner
                            |> Maybe.map (\owner -> Utxo.datumValueFromData <| Data.Constr Natural.zero [ owner, input_bucket_index ])
                    , referenceScript = Nothing
                    }

                -- The redeemer needs to provide the index of that bucket we spend
                -- in the list of the Tx inputs,
                -- and the index of that new bucket we create in the Tx outputs.
                redeemerData { inputs, outputs } =
                    let
                        -- The input index is easy to find, just look for the correct ref
                        input_bucket_index =
                            findIndex (\ref -> ref == bucketRef) inputs
                                -- Put a huge default, just to make sure it’s found
                                |> Maybe.withDefault 8000

                        -- For the output index, we will make the assumption that
                        -- it is the only one at the script address,
                        -- that contains a datum with the same owner as the input
                        output_bucket_index =
                            findIndex (\o -> extractOwner o == bucketOwner) outputs
                                -- Put a huge default, just to make sure it’s found
                                |> Maybe.withDefault 9000
                    in
                    Data.Constr Natural.zero
                        [ Data.Int <| Integer.fromSafeInt input_bucket_index
                        , Data.Int <| Integer.fromSafeInt output_bucket_index
                        ]

                reuseBucketTxAttempt =
                    [ Spend
                        (FromPlutusScript
                            { spentInput = bucketRef
                            , datumWitness = Nothing
                            , plutusScriptWitness =
                                { script = ( PlutusV3, WitnessByValue ctx.lockScript.compiledCode )
                                , redeemerData = redeemerData

                                -- NO SIGNATURE: it’s the actual purpose of these "buckets"
                                , requiredSigners = []
                                }
                            }
                        )
                    , Spend <| FromWallet { address = ctx.loadedWallet.changeAddress, value = bucketValueIncrease, guaranteedUtxos = [] }
                    , SendToOutputAdvanced outputBucket
                    ]
                        |> Cardano.finalize ctx.localStateUtxos []
            in
            case reuseBucketTxAttempt of
                Ok { tx } ->
                    ( Submitting ctx { tx = tx, errors = "" }
                    , toWallet (Cip30.encodeRequest (Cip30.signTx ctx.loadedWallet.wallet { partialSign = False } tx))
                    )

                Err err ->
                    ( TxSubmitted ctx { txId = txId, errors = Debug.toString err }
                    , Cmd.none
                    )

        _ ->
            ( model, Cmd.none )


createBucket : AppContext -> ( Model, Cmd Msg )
createBucket ({ localStateUtxos, myKeyCred, scriptAddress, loadedWallet, lockScript } as ctx) =
    let
        -- 1 ada is 1 million lovelaces
        twoAda =
            Value.onlyLovelace (Natural.fromSafeString "2000000")

        -- Datum as specified by the blueprint of the lock script,
        -- containing our credentials for later verification when spending
        datum =
            Data.Constr Natural.zero
                [ Data.Bytes <| Bytes.toAny myKeyCred -- owner
                , Data.Int <| Integer.zero -- input_bucket_index
                ]

        -- Transaction locking 2 Ada in a "bucket" UTxO at the script address
        createBucketTxAttempt =
            [ Spend <| FromWallet { address = ctx.loadedWallet.changeAddress, value = twoAda, guaranteedUtxos = [] }
            , SendToOutput
                { address = scriptAddress
                , amount = twoAda
                , datumOption = Just (Utxo.datumValueFromData datum)
                , referenceScript = Nothing
                }
            ]
                |> Cardano.finalize localStateUtxos []
    in
    case createBucketTxAttempt of
        Ok { tx } ->
            ( Submitting ctx { tx = tx, errors = "" }
            , toWallet (Cip30.encodeRequest (Cip30.signTx loadedWallet.wallet { partialSign = False } tx))
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
                       , button [ onClick CreateBucketButtonClicked ] [ text "Create bucket" ]
                       , displayErrors errors
                       ]
                )

        Submitting { loadedWallet } { errors } ->
            div []
                (viewLoadedWallet loadedWallet
                    ++ [ div [] [ text <| "Signing and submitting the transaction ..." ]
                       , displayErrors errors
                       ]
                )

        TxSubmitted { loadedWallet } { txId, errors } ->
            div []
                (viewLoadedWallet loadedWallet
                    ++ [ div [] [ text <| "Tx submitted! with ID: " ++ Bytes.toHex txId ]
                       , div [] [ text <| "Now we can reuse that bucket without adding the owner credential in 'required_signers' array! " ]
                       , button [ onClick ReuseBucketButtonClicked ] [ text "Reuse the bucket" ]
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



-- #########################################################
-- Helpers
-- #########################################################


findIndex : (a -> Bool) -> List a -> Maybe Int
findIndex =
    findIndexHelp 0


findIndexHelp : Int -> (a -> Bool) -> List a -> Maybe Int
findIndexHelp index predicate list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if predicate x then
                Just index

            else
                findIndexHelp (index + 1) predicate xs
