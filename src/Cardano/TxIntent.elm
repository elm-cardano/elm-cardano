module Cardano.TxIntent exposing
    ( balance, finalize, finalizeAdvanced, TxFinalized, TxFinalizationError(..)
    , TxIntent(..), SpendSource(..)
    , CertificateIntent(..)
    , VoteIntent, ProposalIntent, ActionProposal(..)
    , TxOtherInfo(..)
    , Fee(..)
    , GovernanceState, emptyGovernanceState
    , updateLocalState
    )

{-| Building Cardano Transactions with Intents.


# Transaction Building Overview

This framework aims to provide intuitive and correct building blocks
for transaction building, based on the following aspects of transactions.

1.  Intent: what we want to achieve with this transaction
      - Transfer: send some tokens from somewhere to somewhere else
      - Mint and burn: create and destroy tokens
      - Use a script: provide/spend tokens and data to/from a script
      - Stake management: collect rewards, manage delegations and pool registrations
      - Voting: vote on proposals
      - Propose: make your own proposals
2.  Metadata: additional information
3.  Constraints: what additional constraints do we want to set
      - Temporal validity range: first/last slots when the Tx is valid
4.  Requirements: what is imposed by the protocol
      - Tx fee: depends on size/mem/cpu
      - Hashes: for metadata and script data
      - Collateral: for plutus scripts
      - Signatures: for consuming inputs and scripts requirements

This API revolves around composing intents, then adding metadata and constraints,
and finally trying to validate it and auto-populate all requirements.


# Code Documentation

@docs balance, finalize, finalizeAdvanced, TxFinalized, TxFinalizationError
@docs TxIntent, SpendSource
@docs CertificateIntent
@docs VoteIntent, ProposalIntent, ActionProposal
@docs TxOtherInfo
@docs Fee
@docs GovernanceState, emptyGovernanceState
@docs updateLocalState

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Map as Map exposing (BytesMap)
import Cardano.Address as Address exposing (Address(..), Credential(..), CredentialHash, NetworkId(..), StakeAddress)
import Cardano.AuxiliaryData as AuxiliaryData exposing (AuxiliaryData)
import Cardano.CoinSelection as CoinSelection
import Cardano.Data as Data exposing (Data)
import Cardano.Gov as Gov exposing (Action, ActionId, Anchor, Constitution, CostModels, Drep(..), ProposalProcedure, ProtocolParamUpdate, ProtocolVersion, Vote, Voter(..))
import Cardano.Metadatum exposing (Metadatum)
import Cardano.MultiAsset as MultiAsset exposing (AssetName, MultiAsset, PolicyId)
import Cardano.Pool as Pool
import Cardano.Redeemer as Redeemer exposing (Redeemer, RedeemerTag)
import Cardano.Script as Script exposing (NativeScript, PlutusVersion(..), ScriptCbor)
import Cardano.Transaction as Transaction exposing (Certificate(..), Transaction, TransactionBody, VKeyWitness, WitnessSet)
import Cardano.TxContext as TxContext exposing (TxContext)
import Cardano.Uplc as Uplc
import Cardano.Utils exposing (UnitInterval)
import Cardano.Utxo as Utxo exposing (Output, OutputReference, TransactionId)
import Cardano.Value as Value exposing (Value)
import Cardano.Witness as Witness
import Dict.Any exposing (AnyDict)
import Integer exposing (Integer)
import List.Extra
import Natural exposing (Natural)
import Result.Extra
import Set


{-| Represents different types of transaction intents.
-}
type TxIntent
    = SendTo Address Value
    | SendToOutput Output
    | SendToOutputAdvanced (TxContext -> Output)
      -- Spending assets from somewhere
    | Spend SpendSource
      -- Minting / burning assets
    | MintBurn
        { policyId : Bytes CredentialHash
        , assets : BytesMap AssetName Integer
        , scriptWitness : Witness.Script
        }
      -- Issuing certificates
    | IssueCertificate CertificateIntent
      -- Withdrawing rewards
    | WithdrawRewards
        -- TODO: check that the addres type match the scriptWitness field
        { stakeCredential : StakeAddress
        , amount : Natural
        , scriptWitness : Maybe Witness.Script
        }
    | Vote Witness.Voter (List VoteIntent)
    | Propose ProposalIntent


{-| Represents different sources for spending assets.
-}
type SpendSource
    = FromWallet
        { address : Address
        , value : Value
        , guaranteedUtxos : List OutputReference
        }
    | FromNativeScript
        { spentInput : OutputReference
        , nativeScriptWitness : Witness.NativeScript
        }
    | FromPlutusScript
        { spentInput : OutputReference
        , datumWitness : Maybe (Witness.Source Data)
        , plutusScriptWitness : Witness.PlutusScript
        }


{-| All intents requiring the on-chain publication of a certificate.

These include stake registration and delegation,
stake pool management, and voting or delegating your voting power.

-}
type CertificateIntent
    = RegisterStake { delegator : Witness.Credential, deposit : Natural }
    | UnregisterStake { delegator : Witness.Credential, refund : Natural }
    | DelegateStake { delegator : Witness.Credential, poolId : Bytes Pool.Id }
      -- Pool management
    | RegisterPool { deposit : Natural } Pool.Params
    | RetirePool { poolId : Bytes Pool.Id, epoch : Natural }
      -- Vote management
    | RegisterDrep { drep : Witness.Credential, deposit : Natural, info : Maybe Anchor }
    | UnregisterDrep { drep : Witness.Credential, refund : Natural }
    | VoteAlwaysAbstain { delegator : Witness.Credential }
    | VoteAlwaysNoConfidence { delegator : Witness.Credential }
    | DelegateVotes { delegator : Witness.Credential, drep : Credential }


{-| Governance vote.
-}
type alias VoteIntent =
    { actionId : ActionId
    , vote : Vote
    , rationale : Maybe Anchor
    }


{-| Governance action proposal.
-}
type alias ProposalIntent =
    { govAction : ActionProposal
    , offchainInfo : Anchor
    , deposit : Natural
    , depositReturnAccount : StakeAddress
    }


{-| The different kinds of proposals available for governance.
-}
type ActionProposal
    = ParameterChange ProtocolParamUpdate
    | HardForkInitiation ProtocolVersion
    | TreasuryWithdrawals (List { destination : StakeAddress, amount : Natural })
    | NoConfidence
    | UpdateCommittee
        { removeMembers : List Credential
        , addMembers : List { newMember : Credential, expirationEpoch : Natural }
        , quorumThreshold : UnitInterval
        }
    | NewConstitution Constitution
    | Info


{-| Represents additional information for a transaction.
-}
type TxOtherInfo
    = TxReferenceInput OutputReference
    | TxMetadata { tag : Natural, metadata : Metadatum }
    | TxTimeValidityRange { start : Int, end : Natural }


{-| Configure fees manually or automatically for a transaction.
-}
type Fee
    = ManualFee (List { paymentSource : Address, exactFeeAmount : Natural })
    | AutoFee { paymentSource : Address }


{-| Initialize fee estimation by setting the fee field to ₳0.5
This is represented as 500K lovelace, which is encoded as a 32bit uint.
32bit uint can represent a range from ₳0.065 to ₳4200 so it most likely won’t change.
-}
defaultAutoFee : Natural
defaultAutoFee =
    Natural.fromSafeInt 500000


{-| Result of the Tx finalization.

The hashes of the credentials expected to provide a signature
are provided as an additional artifact of Tx finalization.

-}
type alias TxFinalized =
    { tx : Transaction
    , expectedSignatures : List (Bytes CredentialHash)
    }


{-| Errors that may happen during Tx finalization.
-}
type TxFinalizationError
    = UnableToGuessFeeSource
    | UnbalancedIntents { inputTotal : Value, outputTotal : Value, extraneousInput : Value, extraneousOutput : Value } String
    | InsufficientManualFee { declared : Natural, computed : Natural }
    | NotEnoughMinAda String
    | InvalidAddress Address String
    | InvalidStakeAddress StakeAddress String
    | DuplicateVoters (List { voter : String })
    | EmptyVotes { voter : String }
    | WitnessError Witness.Error
    | FailedToPerformCoinSelection CoinSelection.Error
    | CollateralSelectionError CoinSelection.Error
    | DuplicatedMetadataTags Int
    | IncorrectTimeValidityRange String
    | UplcVmError String
    | GovProposalsNotSupportedInSimpleFinalize
    | FailurePleaseReportToElmCardano String


{-| Attempt to balance a transaction with a provided address.

All the missing value from inputs and/or outputs is compensated
by adding new intents with the provided address as source and/or destination.

The function may fail while verifying that all references are present in the local state UTxOs.

-}
balance : Utxo.RefDict Output -> Address -> List TxIntent -> Result TxFinalizationError (List TxIntent)
balance localStateUtxos address txIntents =
    case checkBalance localStateUtxos txIntents of
        Err (UnbalancedIntents { extraneousInput, extraneousOutput } _) ->
            Ok <|
                SendTo address extraneousInput
                    :: Spend (FromWallet { address = address, value = extraneousOutput, guaranteedUtxos = [] })
                    :: txIntents

        Err error ->
            Err error

        Ok _ ->
            Ok txIntents


{-| Type to represent the successful output after the Tx intents balance has been checked.
-}
type alias Balanced =
    { preProcessedIntents : PreProcessedIntents
    , preSelected : { sum : Value, inputs : Utxo.RefDict (Maybe (TxContext -> Data)) }
    , preCreated : TxContext -> { sum : Value, outputs : List Output }
    }


{-| Check that Tx intents are balanced, and transitively that no reference is missing from the local state UTxOs.
-}
checkBalance : Utxo.RefDict Output -> List TxIntent -> Result TxFinalizationError Balanced
checkBalance localStateUtxos txIntents =
    let
        preProcessedIntents =
            -- A bit too much work, but let’s reuse and not optimize
            preProcessIntents txIntents

        -- Accumulate all output references from inputs and witnesses.
        allOutputReferencesInIntents : Utxo.RefDict ()
        allOutputReferencesInIntents =
            List.concat
                [ List.map .input preProcessedIntents.preSelected
                , preProcessedIntents.guaranteedUtxos
                , List.filterMap Witness.extractRef preProcessedIntents.nativeScriptSources
                , List.map (\( _, source ) -> source) preProcessedIntents.plutusScriptSources
                    |> List.filterMap Witness.extractRef
                , List.filterMap Witness.extractRef preProcessedIntents.datumSources
                ]
                |> List.map (\ref -> ( ref, () ))
                |> Utxo.refDictFromList

        -- Check that all referenced inputs are present in the local state
        absentOutputReferencesInLocalState : Utxo.RefDict ()
        absentOutputReferencesInLocalState =
            Dict.Any.diff allOutputReferencesInIntents
                (Dict.Any.map (\_ _ -> ()) localStateUtxos)

        -- Extract total minted value and total burned value
        splitMintsBurns =
            List.map (\m -> ( m.policyId, MultiAsset.balance m.assets )) preProcessedIntents.mints

        totalMintedValue =
            List.foldl (\( p, { minted } ) -> Value.addTokens (Map.singleton p minted)) Value.zero splitMintsBurns

        totalBurnedValue =
            List.foldl (\( p, { burned } ) -> Value.addTokens (Map.singleton p burned)) Value.zero splitMintsBurns

        -- Extract total ada amount withdrawn
        totalWithdrawalAmount =
            List.foldl (\w acc -> Natural.add w.amount acc) Natural.zero preProcessedIntents.withdrawals

        -- Retrieve the ada and tokens amount at a given output reference
        getValueFromRef : OutputReference -> Value
        getValueFromRef ref =
            Dict.Any.get ref localStateUtxos
                |> Maybe.map .amount
                |> Maybe.withDefault Value.zero

        -- Extract value thanks to input refs
        -- Also add minted tokens and withdrawals to preSelected
        preSelected =
            preProcessedIntents.preSelected
                |> List.foldl (\s -> addPreSelectedInput s.input (getValueFromRef s.input) s.redeemer)
                    { sum = Value.add totalMintedValue (Value.onlyLovelace totalWithdrawalAmount)
                    , inputs = Utxo.emptyRefDict
                    }

        -- Add burned tokens to preCreated
        preCreated =
            \txContext ->
                let
                    { sum, outputs } =
                        preProcessedIntents.preCreated txContext
                in
                { sum = Value.add sum totalBurnedValue, outputs = outputs }

        preCreatedOutputs =
            preCreated TxContext.new

        -- Compute total inputs and outputs to check the Tx balance
        totalInput =
            Dict.Any.foldl (\_ -> Value.add) preSelected.sum preProcessedIntents.freeInputs
                |> Value.add (Value.onlyLovelace preProcessedIntents.totalRefund)

        totalOutput =
            Dict.Any.foldl (\_ -> Value.add) preCreatedOutputs.sum preProcessedIntents.freeOutputs
                |> Value.add (Value.onlyLovelace preProcessedIntents.totalDeposit)
    in
    if not <| Dict.Any.isEmpty absentOutputReferencesInLocalState then
        Err <| WitnessError <| Witness.ReferenceOutputsMissingFromLocalState (Dict.Any.keys absentOutputReferencesInLocalState)

    else if totalInput /= totalOutput then
        let
            extraneousInput =
                Value.subtract totalInput totalOutput

            extraneousOutput =
                Value.subtract totalOutput totalInput

            errorMessage =
                let
                    indent spaces str =
                        String.repeat spaces " " ++ str

                    missingOutputMsg =
                        case Value.toMultilineString extraneousInput of
                            [ adaStr ] ->
                                "Missing lovelace output: " ++ adaStr

                            multilineValue ->
                                "Missing value output:\n"
                                    ++ (String.join "\n" <| List.map (indent 3) multilineValue)

                    missingInputMsg =
                        case Value.toMultilineString extraneousOutput of
                            [ adaStr ] ->
                                "Missing lovelace input: " ++ adaStr

                            multilineValue ->
                                "Missing value input:\n"
                                    ++ (String.join "\n" <| List.map (indent 3) multilineValue)
                in
                if extraneousOutput == Value.zero then
                    missingOutputMsg

                else if extraneousInput == Value.zero then
                    missingInputMsg

                else
                    String.join "\n" [ missingInputMsg, missingOutputMsg ]
        in
        Err <|
            UnbalancedIntents
                { inputTotal = totalInput
                , outputTotal = totalOutput
                , extraneousInput = extraneousInput
                , extraneousOutput = extraneousOutput
                }
                errorMessage

    else
        Ok
            { preProcessedIntents = preProcessedIntents
            , preSelected = preSelected
            , preCreated = preCreated
            }


{-| Finalize a transaction before signing and submitting it.

Analyze all intents and perform the following actions:

  - Check the Tx balance
  - Select the input UTxOs with a default coin selection algorithm
  - Evaluate script execution costs with default mainnet parameters
  - Try to find fee payment source automatically and compute automatic Tx fee

The network parameters will be automatically chosen to be:

  - default Mainnet parameters if the guessed fee address is from Mainnet
  - default Preview parameters if the guessed fee address is from a testnet.

Preprod is not supported for this simplified [finalize] function.
In case you want more customization, please use [finalizeAdvanced].

-}
finalize :
    Utxo.RefDict Output
    -> List TxOtherInfo
    -> List TxIntent
    -> Result TxFinalizationError TxFinalized
finalize localStateUtxos txOtherInfo txIntents =
    assertNoGovProposals txIntents
        |> Result.andThen (\_ -> guessFeeSource txIntents)
        |> Result.andThen
            (\feeSource ->
                let
                    defaultEvalScriptsCosts =
                        if containPlutusScripts txIntents then
                            let
                                network =
                                    case feeSource of
                                        Byron _ ->
                                            Debug.todo "Byron addresses are not unsupported"

                                        Shelley { networkId } ->
                                            networkId

                                        Reward { networkId } ->
                                            networkId

                                slotConfig =
                                    case network of
                                        Mainnet ->
                                            Uplc.slotConfigMainnet

                                        Testnet ->
                                            Uplc.slotConfigPreview
                            in
                            Uplc.evalScriptsCosts
                                { budget = Uplc.conwayDefaultBudget
                                , slotConfig = slotConfig
                                , costModels = Uplc.conwayDefaultCostModels
                                }

                        else
                            \_ _ -> Ok []
                in
                finalizeAdvanced
                    { govState = emptyGovernanceState -- proposals are forbidden in simple finalize anyway
                    , localStateUtxos = localStateUtxos
                    , coinSelectionAlgo = CoinSelection.largestFirst
                    , evalScriptsCosts = defaultEvalScriptsCosts
                    , costModels = Uplc.conwayDefaultCostModels
                    }
                    (AutoFee { paymentSource = feeSource })
                    txOtherInfo
                    txIntents
            )


{-| Simple helper function needed to check that there isn’t any proposal
in the Tx intents when using the simple [finalize] function.
This is because finalization requires some governance state, not provided here,
such as guardrails script hash, last enacted proposals, etc.
-}
assertNoGovProposals : List TxIntent -> Result TxFinalizationError ()
assertNoGovProposals intents =
    case intents of
        [] ->
            Ok ()

        (Propose _) :: _ ->
            Err GovProposalsNotSupportedInSimpleFinalize

        _ :: otherIntents ->
            assertNoGovProposals otherIntents


{-| Attempt to guess the [Address] used to pay the fees from the list of intents.

It will use an address coming from either of these below options,
in the preference order of that list:

  - an address coming from a `From address value` spend source
  - an address coming from a `SendTo address value` destination

If none of these are present, this will return an `UnableToGuessFeeSource` error.
If a wallet UTxO reference is found but not present in the local state UTxOs,
this will return a `ReferenceOutputsMissingFromLocalState` error.

-}
guessFeeSource : List TxIntent -> Result TxFinalizationError Address
guessFeeSource txIntents =
    let
        findFromAddress intents =
            case intents of
                [] ->
                    Nothing

                (Spend (FromWallet { address })) :: _ ->
                    Just address

                _ :: rest ->
                    findFromAddress rest

        findSendTo intents =
            case intents of
                [] ->
                    Nothing

                (SendTo address _) :: _ ->
                    Just address

                _ :: rest ->
                    findSendTo rest
    in
    case findFromAddress txIntents of
        Just address ->
            Ok address

        Nothing ->
            case findSendTo txIntents of
                Just address ->
                    Ok address

                Nothing ->
                    Err UnableToGuessFeeSource


{-| Helper function to detect the presence of Plutus scripts in the transaction.
-}
containPlutusScripts : List TxIntent -> Bool
containPlutusScripts txIntents =
    case txIntents of
        [] ->
            False

        (SendTo _ _) :: otherIntents ->
            containPlutusScripts otherIntents

        (SendToOutput _) :: otherIntents ->
            containPlutusScripts otherIntents

        (SendToOutputAdvanced _) :: otherIntents ->
            containPlutusScripts otherIntents

        (Spend (FromWallet _)) :: otherIntents ->
            containPlutusScripts otherIntents

        (Spend (FromNativeScript _)) :: otherIntents ->
            containPlutusScripts otherIntents

        (Spend (FromPlutusScript _)) :: _ ->
            True

        (MintBurn { scriptWitness }) :: otherIntents ->
            case scriptWitness of
                Witness.Native _ ->
                    containPlutusScripts otherIntents

                Witness.Plutus _ ->
                    True

        (IssueCertificate (RegisterStake { delegator })) :: otherIntents ->
            if Witness.credentialIsPlutusScript delegator then
                True

            else
                containPlutusScripts otherIntents

        (IssueCertificate (UnregisterStake { delegator })) :: otherIntents ->
            if Witness.credentialIsPlutusScript delegator then
                True

            else
                containPlutusScripts otherIntents

        (IssueCertificate (DelegateStake { delegator })) :: otherIntents ->
            if Witness.credentialIsPlutusScript delegator then
                True

            else
                containPlutusScripts otherIntents

        (IssueCertificate (RegisterPool _ _)) :: otherIntents ->
            containPlutusScripts otherIntents

        (IssueCertificate (RetirePool _)) :: otherIntents ->
            containPlutusScripts otherIntents

        (IssueCertificate (RegisterDrep { drep })) :: otherIntents ->
            if Witness.credentialIsPlutusScript drep then
                True

            else
                containPlutusScripts otherIntents

        (IssueCertificate (UnregisterDrep { drep })) :: otherIntents ->
            if Witness.credentialIsPlutusScript drep then
                True

            else
                containPlutusScripts otherIntents

        (IssueCertificate (VoteAlwaysAbstain { delegator })) :: otherIntents ->
            if Witness.credentialIsPlutusScript delegator then
                True

            else
                containPlutusScripts otherIntents

        (IssueCertificate (VoteAlwaysNoConfidence { delegator })) :: otherIntents ->
            if Witness.credentialIsPlutusScript delegator then
                True

            else
                containPlutusScripts otherIntents

        (IssueCertificate (DelegateVotes { delegator })) :: otherIntents ->
            if Witness.credentialIsPlutusScript delegator then
                True

            else
                containPlutusScripts otherIntents

        (WithdrawRewards { scriptWitness }) :: otherIntents ->
            case scriptWitness of
                Just (Witness.Plutus _) ->
                    True

                _ ->
                    containPlutusScripts otherIntents

        (Vote voter _) :: otherIntents ->
            case voter of
                Witness.WithCommitteeHotCred (Witness.WithScript _ (Witness.Plutus _)) ->
                    True

                Witness.WithDrepCred (Witness.WithScript _ (Witness.Plutus _)) ->
                    True

                _ ->
                    containPlutusScripts otherIntents

        (Propose { govAction }) :: otherIntents ->
            case govAction of
                ParameterChange _ ->
                    True

                TreasuryWithdrawals _ ->
                    True

                _ ->
                    containPlutusScripts otherIntents


{-| Contains pointers to the latest enacted governance actions and to the constitution.
-}
type alias GovernanceState =
    { guardrailsScript :
        Maybe
            { policyId : Bytes PolicyId
            , plutusVersion : PlutusVersion
            , scriptWitness : Witness.Source (Bytes ScriptCbor)
            }
    , lastEnactedCommitteeAction : Maybe ActionId
    , lastEnactedConstitutionAction : Maybe ActionId
    , lastEnactedHardForkAction : Maybe ActionId
    , lastEnactedProtocolParamUpdateAction : Maybe ActionId
    }


{-| Just a helper initialization for when we don’t care about governance proposals.
-}
emptyGovernanceState : GovernanceState
emptyGovernanceState =
    { guardrailsScript = Nothing
    , lastEnactedCommitteeAction = Nothing
    , lastEnactedConstitutionAction = Nothing
    , lastEnactedHardForkAction = Nothing
    , lastEnactedProtocolParamUpdateAction = Nothing
    }


{-| Finalize a transaction before signing and submitting it.

Analyze all intents and perform the following actions:

  - Check the Tx balance
  - Select the input UTxOs with the provided coin selection algorithm
  - Evaluate script execution costs with the provided function
  - Compute Tx fee if set to auto

-}
finalizeAdvanced :
    { govState : GovernanceState
    , localStateUtxos : Utxo.RefDict Output
    , coinSelectionAlgo : CoinSelection.Algorithm
    , evalScriptsCosts : Utxo.RefDict Output -> Transaction -> Result String (List Redeemer)
    , costModels : CostModels
    }
    -> Fee
    -> List TxOtherInfo
    -> List TxIntent
    -> Result TxFinalizationError TxFinalized
finalizeAdvanced { govState, localStateUtxos, coinSelectionAlgo, evalScriptsCosts, costModels } fee txOtherInfo txIntents =
    case ( processIntents govState localStateUtxos txIntents, processOtherInfo txOtherInfo ) of
        ( Err err, _ ) ->
            Err err

        ( _, Err err ) ->
            Err err

        ( Ok processedIntents, Ok processedOtherInfo ) ->
            let
                buildTxRound : TxContext -> Fee -> Result TxFinalizationError TxFinalized
                buildTxRound txContext roundFees =
                    let
                        ( feeAmount, feeAddresses ) =
                            case roundFees of
                                ManualFee perAddressFee ->
                                    ( List.foldl (\{ exactFeeAmount } -> Natural.add exactFeeAmount) Natural.zero perAddressFee
                                    , List.map .paymentSource perAddressFee
                                    )

                                AutoFee { paymentSource } ->
                                    ( defaultAutoFee, [ paymentSource ] )

                        ( collateralAmount, collateralSources ) =
                            if List.isEmpty processedIntents.plutusScriptSources then
                                ( Natural.zero, Address.emptyDict )

                            else
                                -- collateral = 1.5 * fee
                                -- It’s an euclidean division, so if there is a non-zero rest,
                                -- we add 1 to make sure we aren’t short 1 lovelace.
                                ( feeAmount
                                    |> Natural.mul (Natural.fromSafeInt 15)
                                    |> Natural.divModBy (Natural.fromSafeInt 10)
                                    |> Maybe.withDefault ( Natural.zero, Natural.zero )
                                    |> (\( q, r ) -> Natural.add q <| Natural.min r Natural.one)
                                  -- Identify automatically collateral sources
                                  -- from fee addresses, free inputs addresses or spent inputs addresses.
                                , [ feeAddresses
                                  , Dict.Any.keys processedIntents.freeInputs
                                  , Dict.Any.keys processedIntents.preSelected.inputs
                                        |> List.filterMap (\addr -> Dict.Any.get addr localStateUtxos |> Maybe.map .address)
                                  ]
                                    |> List.concat
                                    |> List.filter Address.isShelleyWallet
                                    -- make the list unique
                                    |> List.map (\addr -> ( addr, () ))
                                    |> Address.dictFromList
                                )

                        updateTxContext : Address.Dict { selectedUtxos : List ( OutputReference, Output ), changeOutputs : List Output } -> TxContext
                        updateTxContext coinSelections =
                            TxContext.updateInputsOutputs
                                { preSelectedInputs =
                                    Dict.Any.filterMap (\ref _ -> Dict.Any.get ref localStateUtxos) processedIntents.preSelected.inputs
                                , preCreatedOutputs = processedIntents.preCreated
                                }
                                -- aggregate per-address coin selections into one
                                (Dict.Any.foldl insertOneSelection { selectedUtxos = Utxo.emptyRefDict, changeOutputs = [] } coinSelections)
                                txContext

                        insertOneSelection _ { selectedUtxos, changeOutputs } acc =
                            { selectedUtxos = List.foldl (\( ref, output ) -> Dict.Any.insert ref output) acc.selectedUtxos selectedUtxos
                            , changeOutputs = changeOutputs ++ acc.changeOutputs
                            }
                    in
                    -- UTxO selection
                    Result.map2
                        (\coinSelection collateralSelection ->
                            -- coinSelection : Address.Dict { selectedUtxos : List ( OutputReference, Output ), changeOutputs : List Output }
                            -- Aggregate with pre-selected inputs and pre-created outputs
                            updateTxContext coinSelection
                                --> TransactionBody
                                |> buildTx feeAmount collateralSelection processedIntents processedOtherInfo
                        )
                        (computeCoinSelection localStateUtxos roundFees processedIntents coinSelectionAlgo)
                        (CoinSelection.collateral (CoinSelection.CollateralContext (Dict.Any.toList localStateUtxos) collateralSources collateralAmount)
                            |> Result.mapError CollateralSelectionError
                        )

                computeRefScriptBytesForTx tx =
                    computeRefScriptBytes localStateUtxos (tx.body.referenceInputs ++ tx.body.inputs)

                adjustFees tx =
                    case fee of
                        ManualFee _ ->
                            fee

                        AutoFee { paymentSource } ->
                            let
                                refScriptBytes =
                                    computeRefScriptBytesForTx tx
                            in
                            Transaction.computeFees Transaction.defaultTxFeeParams { refScriptBytes = refScriptBytes } tx
                                |> (\{ txSizeFee, scriptExecFee, refScriptSizeFee } -> Natural.add txSizeFee scriptExecFee |> Natural.add refScriptSizeFee)
                                |> (\computedFee -> ManualFee [ { paymentSource = paymentSource, exactFeeAmount = computedFee } ])
            in
            -- Without estimating cost of plutus script exec, do couple loops of:
            --   - estimate Tx fees
            --   - adjust coin selection
            --   - adjust redeemers
            buildTxRound TxContext.new fee
                --> Result String Transaction
                |> Result.andThen (\{ tx } -> buildTxRound (TxContext.fromTx localStateUtxos tx) (adjustFees tx))
                -- Evaluate plutus script cost
                |> Result.andThen (\{ tx } -> (adjustExecutionCosts <| evalScriptsCosts localStateUtxos) tx)
                -- Redo a final round of above
                |> Result.andThen (\tx -> buildTxRound (TxContext.fromTx localStateUtxos tx) (adjustFees tx))
                |> Result.andThen (\{ tx } -> (adjustExecutionCosts <| evalScriptsCosts localStateUtxos) tx)
                -- Redo a final round of above
                |> Result.andThen (\tx -> buildTxRound (TxContext.fromTx localStateUtxos tx) (adjustFees tx))
                |> Result.andThen
                    (\{ tx, expectedSignatures } ->
                        (adjustExecutionCosts <| evalScriptsCosts localStateUtxos) tx
                            -- Potentially replace the dummy auxiliary data hash and script data hash
                            |> Result.map replaceDummyAuxiliaryDataHash
                            |> Result.map (replaceDummyScriptDataHash costModels processedIntents)
                            -- Finally, check if final fees are correct
                            |> Result.andThen (\finalTx -> checkInsufficientFee { refScriptBytes = computeRefScriptBytesForTx finalTx } fee finalTx)
                            -- Very finally, clean the placeholder vkey witnesses and append the expected vkey hashes
                            |> Result.map
                                (\finalTx ->
                                    { tx = Transaction.updateSignatures (always Nothing) finalTx
                                    , expectedSignatures = expectedSignatures
                                    }
                                )
                    )


{-| Helper function to update the auxiliary data hash.
-}
replaceDummyAuxiliaryDataHash : Transaction -> Transaction
replaceDummyAuxiliaryDataHash ({ body, auxiliaryData } as tx) =
    { tx | body = { body | auxiliaryDataHash = Maybe.map AuxiliaryData.hash auxiliaryData } }


{-| Helper function to update the script data hash.
-}
replaceDummyScriptDataHash : CostModels -> ProcessedIntents -> Transaction -> Transaction
replaceDummyScriptDataHash costModels intents ({ body } as tx) =
    let
        activeCostModels =
            { plutusV1 =
                if List.any (\( v, _ ) -> v == PlutusV1) intents.plutusScriptSources then
                    costModels.plutusV1

                else
                    Nothing
            , plutusV2 =
                if List.any (\( v, _ ) -> v == PlutusV2) intents.plutusScriptSources then
                    costModels.plutusV2

                else
                    Nothing
            , plutusV3 =
                if List.any (\( v, _ ) -> v == PlutusV3) intents.plutusScriptSources then
                    costModels.plutusV3

                else
                    Nothing
            }
    in
    { tx | body = { body | scriptDataHash = Maybe.map (\_ -> Transaction.hashScriptData activeCostModels tx) body.scriptDataHash } }


{-| Helper function to compute the total size of reference scripts.

Inputs are only counted once (even if present in both regular and reference inputs).
But scripts duplicates in different inputs are counted multiple times.
Both native and Plutus scripts are counted.

The rule is detailed in that document:
<https://github.com/IntersectMBO/cardano-ledger/blob/master/docs/adr/2024-08-14_009-refscripts-fee-change.md#reference-scripts-total-size>

-}
computeRefScriptBytes : Utxo.RefDict Output -> List OutputReference -> Int
computeRefScriptBytes localStateUtxos references =
    -- merge all inputs uniquely
    Utxo.refDictFromList (List.map (\r -> ( r, () )) references)
        |> Dict.Any.keys
        -- retrieve outputs reference scripts for all inputs
        |> List.filterMap
            (\ref ->
                Dict.Any.get ref localStateUtxos
                    |> Maybe.andThen .referenceScript
            )
        -- extract reference script bytes size
        |> List.map (\scriptRef -> Bytes.width (Script.refBytes scriptRef))
        |> List.sum


type alias PreProcessedIntents =
    { freeInputs : Address.Dict Value
    , freeOutputs : Address.Dict Value
    , guaranteedUtxos : List OutputReference
    , preSelected : List { input : OutputReference, redeemer : Maybe (TxContext -> Data) }
    , preCreated : TxContext -> { sum : Value, outputs : List Output }
    , nativeScriptSources : List (Witness.Source NativeScript)
    , plutusScriptSources : List ( PlutusVersion, Witness.Source (Bytes ScriptCbor) )
    , datumSources : List (Witness.Source Data)
    , expectedSigners : List (List (Bytes CredentialHash)) -- like requiredSigners, but not to put in the required_signers field of the Tx
    , requiredSigners : List (List (Bytes CredentialHash))
    , mints : List { policyId : Bytes CredentialHash, assets : BytesMap AssetName Integer, redeemer : Maybe (TxContext -> Data) }
    , withdrawals : List { stakeAddress : StakeAddress, amount : Natural, redeemer : Maybe (TxContext -> Data) }
    , certificates : List ( Certificate, Maybe (TxContext -> Data) )
    , proposalIntents : List ProposalIntent
    , votes : List { voter : Voter, votes : List VoteIntent, redeemer : Maybe (TxContext -> Data) }
    , totalDeposit : Natural
    , totalRefund : Natural
    }


noIntent : PreProcessedIntents
noIntent =
    { freeInputs = Address.emptyDict
    , freeOutputs = Address.emptyDict
    , guaranteedUtxos = []
    , preSelected = []
    , preCreated = \_ -> { sum = Value.zero, outputs = [] }
    , nativeScriptSources = []
    , plutusScriptSources = []
    , datumSources = []
    , expectedSigners = []
    , requiredSigners = []
    , mints = []
    , withdrawals = []
    , certificates = []
    , proposalIntents = []
    , votes = []
    , totalDeposit = Natural.zero
    , totalRefund = Natural.zero
    }


{-| Initial processing step in order to categorize all intents.

This pre-processing step does not need the local utxo state.
It only aggregates all intents into relevant fields
to make following processing steps easier.

-}
preProcessIntents : List TxIntent -> PreProcessedIntents
preProcessIntents txIntents =
    let
        freeValueAdd : Address -> Value -> Address.Dict Value -> Address.Dict Value
        freeValueAdd addr v freeValue =
            Dict.Any.update addr (Just << Value.add v << Maybe.withDefault Value.zero) freeValue

        -- Step function that pre-processes each TxIntent
        stepIntent : TxIntent -> PreProcessedIntents -> PreProcessedIntents
        stepIntent txIntent preProcessedIntents =
            case txIntent of
                SendTo addr v ->
                    { preProcessedIntents
                        | freeOutputs = freeValueAdd addr v preProcessedIntents.freeOutputs
                    }

                SendToOutput newOutput ->
                    let
                        newPreCreated txContext =
                            let
                                { sum, outputs } =
                                    preProcessedIntents.preCreated txContext
                            in
                            { sum = Value.add sum newOutput.amount
                            , outputs = newOutput :: outputs
                            }
                    in
                    { preProcessedIntents | preCreated = newPreCreated }

                SendToOutputAdvanced f ->
                    let
                        newPreCreated txContext =
                            let
                                { sum, outputs } =
                                    preProcessedIntents.preCreated txContext

                                newOutput =
                                    f txContext
                            in
                            { sum = Value.add sum newOutput.amount
                            , outputs = newOutput :: outputs
                            }
                    in
                    { preProcessedIntents | preCreated = newPreCreated }

                Spend (FromWallet { address, value, guaranteedUtxos }) ->
                    { preProcessedIntents
                        | freeInputs = freeValueAdd address value preProcessedIntents.freeInputs
                        , guaranteedUtxos = guaranteedUtxos ++ preProcessedIntents.guaranteedUtxos
                    }

                Spend (FromNativeScript { spentInput, nativeScriptWitness }) ->
                    { preProcessedIntents
                        | preSelected = { input = spentInput, redeemer = Nothing } :: preProcessedIntents.preSelected
                        , nativeScriptSources = nativeScriptWitness.script :: preProcessedIntents.nativeScriptSources
                        , expectedSigners = nativeScriptWitness.expectedSigners :: preProcessedIntents.expectedSigners
                    }

                Spend (FromPlutusScript { spentInput, datumWitness, plutusScriptWitness }) ->
                    let
                        newDatumSources =
                            case datumWitness of
                                Nothing ->
                                    preProcessedIntents.datumSources

                                Just datumSource ->
                                    datumSource :: preProcessedIntents.datumSources
                    in
                    { preProcessedIntents
                        | preSelected = { input = spentInput, redeemer = Just plutusScriptWitness.redeemerData } :: preProcessedIntents.preSelected
                        , datumSources = newDatumSources
                        , requiredSigners = plutusScriptWitness.requiredSigners :: preProcessedIntents.requiredSigners
                        , plutusScriptSources = plutusScriptWitness.script :: preProcessedIntents.plutusScriptSources
                    }

                MintBurn { policyId, assets, scriptWitness } ->
                    case scriptWitness of
                        Witness.Native { script, expectedSigners } ->
                            { preProcessedIntents
                                | nativeScriptSources = script :: preProcessedIntents.nativeScriptSources
                                , expectedSigners = expectedSigners :: preProcessedIntents.expectedSigners
                                , mints = { policyId = policyId, assets = assets, redeemer = Nothing } :: preProcessedIntents.mints
                            }

                        Witness.Plutus { script, redeemerData, requiredSigners } ->
                            { preProcessedIntents
                                | plutusScriptSources = script :: preProcessedIntents.plutusScriptSources
                                , requiredSigners = requiredSigners :: preProcessedIntents.requiredSigners
                                , mints = { policyId = policyId, assets = assets, redeemer = Just redeemerData } :: preProcessedIntents.mints
                            }

                WithdrawRewards { stakeCredential, amount, scriptWitness } ->
                    case scriptWitness of
                        Nothing ->
                            { preProcessedIntents
                                | withdrawals = { stakeAddress = stakeCredential, amount = amount, redeemer = Nothing } :: preProcessedIntents.withdrawals
                            }

                        Just (Witness.Native { script, expectedSigners }) ->
                            { preProcessedIntents
                                | withdrawals = { stakeAddress = stakeCredential, amount = amount, redeemer = Nothing } :: preProcessedIntents.withdrawals
                                , nativeScriptSources = script :: preProcessedIntents.nativeScriptSources
                                , expectedSigners = expectedSigners :: preProcessedIntents.expectedSigners
                            }

                        Just (Witness.Plutus { script, redeemerData, requiredSigners }) ->
                            { preProcessedIntents
                                | withdrawals = { stakeAddress = stakeCredential, amount = amount, redeemer = Just redeemerData } :: preProcessedIntents.withdrawals
                                , plutusScriptSources = script :: preProcessedIntents.plutusScriptSources
                                , requiredSigners = requiredSigners :: preProcessedIntents.requiredSigners
                            }

                IssueCertificate (RegisterStake { delegator, deposit }) ->
                    preprocessCert
                        (\keyCred -> RegCert { delegator = VKeyHash keyCred, deposit = deposit })
                        (\scriptHash -> RegCert { delegator = ScriptHash scriptHash, deposit = deposit })
                        { deposit = deposit, refund = Natural.zero }
                        delegator
                        preProcessedIntents

                IssueCertificate (UnregisterStake { delegator, refund }) ->
                    preprocessCert
                        (\keyCred -> UnregCert { delegator = VKeyHash keyCred, refund = refund })
                        (\scriptHash -> UnregCert { delegator = ScriptHash scriptHash, refund = refund })
                        { deposit = Natural.zero, refund = refund }
                        delegator
                        preProcessedIntents

                IssueCertificate (DelegateStake { delegator, poolId }) ->
                    preprocessCert
                        (\keyCred -> StakeDelegationCert { delegator = VKeyHash keyCred, poolId = poolId })
                        (\scriptHash -> StakeDelegationCert { delegator = ScriptHash scriptHash, poolId = poolId })
                        { deposit = Natural.zero, refund = Natural.zero }
                        delegator
                        preProcessedIntents

                IssueCertificate (RegisterDrep { drep, deposit, info }) ->
                    preprocessCert
                        (\keyCred -> RegDrepCert { drepCredential = VKeyHash keyCred, deposit = deposit, anchor = info })
                        (\scriptHash -> RegDrepCert { drepCredential = ScriptHash scriptHash, deposit = deposit, anchor = info })
                        { deposit = deposit, refund = Natural.zero }
                        drep
                        preProcessedIntents

                IssueCertificate (UnregisterDrep { drep, refund }) ->
                    preprocessCert
                        (\keyCred -> UnregDrepCert { drepCredential = VKeyHash keyCred, refund = refund })
                        (\scriptHash -> UnregDrepCert { drepCredential = ScriptHash scriptHash, refund = refund })
                        { deposit = Natural.zero, refund = refund }
                        drep
                        preProcessedIntents

                IssueCertificate (VoteAlwaysAbstain { delegator }) ->
                    preprocessCert
                        (\keyCred -> VoteDelegCert { delegator = VKeyHash keyCred, drep = AlwaysAbstain })
                        (\scriptHash -> VoteDelegCert { delegator = ScriptHash scriptHash, drep = AlwaysAbstain })
                        { deposit = Natural.zero, refund = Natural.zero }
                        delegator
                        preProcessedIntents

                IssueCertificate (VoteAlwaysNoConfidence { delegator }) ->
                    preprocessCert
                        (\keyCred -> VoteDelegCert { delegator = VKeyHash keyCred, drep = AlwaysNoConfidence })
                        (\scriptHash -> VoteDelegCert { delegator = ScriptHash scriptHash, drep = AlwaysNoConfidence })
                        { deposit = Natural.zero, refund = Natural.zero }
                        delegator
                        preProcessedIntents

                IssueCertificate (DelegateVotes { delegator, drep }) ->
                    preprocessCert
                        (\keyCred -> VoteDelegCert { delegator = VKeyHash keyCred, drep = DrepCredential drep })
                        (\scriptHash -> VoteDelegCert { delegator = ScriptHash scriptHash, drep = DrepCredential drep })
                        { deposit = Natural.zero, refund = Natural.zero }
                        delegator
                        preProcessedIntents

                IssueCertificate (RegisterPool { deposit } poolParams) ->
                    { preProcessedIntents
                        | certificates = ( PoolRegistrationCert poolParams, Nothing ) :: preProcessedIntents.certificates
                        , totalDeposit = Natural.add deposit preProcessedIntents.totalDeposit
                    }

                IssueCertificate (RetirePool { poolId, epoch }) ->
                    { preProcessedIntents
                        | certificates = ( PoolRetirementCert { poolId = poolId, epoch = epoch }, Nothing ) :: preProcessedIntents.certificates
                    }

                Vote (Witness.WithCommitteeHotCred (Witness.WithKey cred)) votes ->
                    { preProcessedIntents
                        | votes = { voter = VoterCommitteeHotCred (VKeyHash cred), votes = votes, redeemer = Nothing } :: preProcessedIntents.votes
                    }

                Vote (Witness.WithCommitteeHotCred (Witness.WithScript cred (Witness.Native { script, expectedSigners }))) votes ->
                    { preProcessedIntents
                        | votes = { voter = VoterCommitteeHotCred (ScriptHash cred), votes = votes, redeemer = Nothing } :: preProcessedIntents.votes
                        , nativeScriptSources = script :: preProcessedIntents.nativeScriptSources
                        , expectedSigners = expectedSigners :: preProcessedIntents.expectedSigners
                    }

                Vote (Witness.WithCommitteeHotCred (Witness.WithScript cred (Witness.Plutus { script, redeemerData, requiredSigners }))) votes ->
                    { preProcessedIntents
                        | votes = { voter = VoterCommitteeHotCred (ScriptHash cred), votes = votes, redeemer = Just redeemerData } :: preProcessedIntents.votes
                        , plutusScriptSources = script :: preProcessedIntents.plutusScriptSources
                        , requiredSigners = requiredSigners :: preProcessedIntents.requiredSigners
                    }

                Vote (Witness.WithDrepCred (Witness.WithKey cred)) votes ->
                    { preProcessedIntents
                        | votes = { voter = VoterDrepCred (VKeyHash cred), votes = votes, redeemer = Nothing } :: preProcessedIntents.votes
                    }

                Vote (Witness.WithDrepCred (Witness.WithScript cred (Witness.Native { script, expectedSigners }))) votes ->
                    { preProcessedIntents
                        | votes = { voter = VoterDrepCred (ScriptHash cred), votes = votes, redeemer = Nothing } :: preProcessedIntents.votes
                        , nativeScriptSources = script :: preProcessedIntents.nativeScriptSources
                        , expectedSigners = expectedSigners :: preProcessedIntents.expectedSigners
                    }

                Vote (Witness.WithDrepCred (Witness.WithScript cred (Witness.Plutus { script, redeemerData, requiredSigners }))) votes ->
                    { preProcessedIntents
                        | votes = { voter = VoterDrepCred (ScriptHash cred), votes = votes, redeemer = Just redeemerData } :: preProcessedIntents.votes
                        , plutusScriptSources = script :: preProcessedIntents.plutusScriptSources
                        , requiredSigners = requiredSigners :: preProcessedIntents.requiredSigners
                    }

                Vote (Witness.WithPoolCred cred) votes ->
                    { preProcessedIntents
                        | votes = { voter = VoterPoolId cred, votes = votes, redeemer = Nothing } :: preProcessedIntents.votes
                    }

                -- For proposals, we accumulate the deposit,
                -- then we keep intents as is, because to actually convert the action type,
                -- we will need the GovernanceState, which isn’t available at the pre-processing step.
                Propose ({ deposit } as proposal) ->
                    { preProcessedIntents
                        | proposalIntents = proposal :: preProcessedIntents.proposalIntents
                        , totalDeposit = Natural.add deposit preProcessedIntents.totalDeposit
                    }
    in
    -- Use fold right so that the outputs list is in the correct order
    List.foldr stepIntent noIntent txIntents


{-| Helper function to update preprocessed state with a new certificate.
It also accumulates the total amount of deposits and refunds.
-}
preprocessCert :
    (Bytes CredentialHash -> Certificate)
    -> (Bytes CredentialHash -> Certificate)
    -> { deposit : Natural, refund : Natural }
    -> Witness.Credential
    -> PreProcessedIntents
    -> PreProcessedIntents
preprocessCert certWithKeyF certWithScriptF { deposit, refund } cred preProcessedIntents =
    case cred of
        Witness.WithKey keyCred ->
            { preProcessedIntents
                | certificates = ( certWithKeyF keyCred, Nothing ) :: preProcessedIntents.certificates
                , totalDeposit = Natural.add deposit preProcessedIntents.totalDeposit
                , totalRefund = Natural.add refund preProcessedIntents.totalRefund
            }

        Witness.WithScript scriptHash (Witness.Native { script, expectedSigners }) ->
            { preProcessedIntents
                | certificates = ( certWithScriptF scriptHash, Nothing ) :: preProcessedIntents.certificates
                , nativeScriptSources = script :: preProcessedIntents.nativeScriptSources
                , expectedSigners = expectedSigners :: preProcessedIntents.expectedSigners
                , totalDeposit = Natural.add deposit preProcessedIntents.totalDeposit
                , totalRefund = Natural.add refund preProcessedIntents.totalRefund
            }

        Witness.WithScript scriptHash (Witness.Plutus { script, redeemerData, requiredSigners }) ->
            { preProcessedIntents
                | certificates = ( certWithScriptF scriptHash, Just redeemerData ) :: preProcessedIntents.certificates
                , plutusScriptSources = script :: preProcessedIntents.plutusScriptSources
                , requiredSigners = requiredSigners :: preProcessedIntents.requiredSigners
                , totalDeposit = Natural.add deposit preProcessedIntents.totalDeposit
                , totalRefund = Natural.add refund preProcessedIntents.totalRefund
            }


type alias ProcessedIntents =
    { freeInputs : Address.Dict Value
    , freeOutputs : Address.Dict Value
    , guaranteedUtxos : Address.Dict (List ( OutputReference, Output ))
    , preSelected : { sum : Value, inputs : Utxo.RefDict (Maybe (TxContext -> Data)) }
    , preCreated : TxContext -> { sum : Value, outputs : List Output }
    , nativeScriptSources : List (Witness.Source NativeScript)
    , plutusScriptSources : List ( PlutusVersion, Witness.Source (Bytes ScriptCbor) )
    , datumSources : List (Witness.Source Data)
    , expectedSigners : List (Bytes CredentialHash)
    , requiredSigners : List (Bytes CredentialHash)
    , totalMinted : MultiAsset Integer
    , mintRedeemers : BytesMap PolicyId (Maybe (TxContext -> Data))
    , withdrawals : Address.StakeDict { amount : Natural, redeemer : Maybe (TxContext -> Data) }
    , certificates : List ( Certificate, Maybe (TxContext -> Data) )
    , proposals : List ( ProposalProcedure, Maybe Data )
    , votes : Gov.VoterDict { votes : List VoteIntent, redeemer : Maybe (TxContext -> Data) }
    }


{-| Process already pre-processed intents and validate them all.
-}
processIntents : GovernanceState -> Utxo.RefDict Output -> List TxIntent -> Result TxFinalizationError ProcessedIntents
processIntents govState localStateUtxos txIntents =
    checkBalance localStateUtxos txIntents
        |> Result.andThen (processBalanced govState localStateUtxos txIntents)


{-| Continue processing intents after the balance has already been checked.
-}
processBalanced : GovernanceState -> Utxo.RefDict Output -> List TxIntent -> Balanced -> Result TxFinalizationError ProcessedIntents
processBalanced govState localStateUtxos txIntents { preProcessedIntents, preSelected, preCreated } =
    let
        ( ( spendings, mints ), ( withdrawals, allVotes ) ) =
            List.foldr distributeIntent ( ( [], [] ), ( [], [] ) ) txIntents

        distributeIntent txIntent ( ( spendingsAcc, mintsAcc ), ( withdrawalsAcc, votesAcc ) ) =
            case txIntent of
                Spend source ->
                    ( ( source :: spendingsAcc, mintsAcc ), ( withdrawalsAcc, votesAcc ) )

                MintBurn mint ->
                    ( ( spendingsAcc, mint :: mintsAcc ), ( withdrawalsAcc, votesAcc ) )

                WithdrawRewards { stakeCredential, scriptWitness } ->
                    ( ( spendingsAcc, mintsAcc ), ( ( stakeCredential, scriptWitness ) :: withdrawalsAcc, votesAcc ) )

                Vote voter votes ->
                    ( ( spendingsAcc, mintsAcc ), ( withdrawalsAcc, ( voter, votes ) :: votesAcc ) )

                _ ->
                    ( ( spendingsAcc, mintsAcc ), ( withdrawalsAcc, votesAcc ) )

        totalMintedAndBurned : MultiAsset Integer
        totalMintedAndBurned =
            List.map (\m -> Map.singleton m.policyId m.assets) preProcessedIntents.mints
                |> List.foldl MultiAsset.mintAdd MultiAsset.empty
                |> MultiAsset.normalize Integer.isZero

        guaranteedUtxos : Address.Dict (List ( OutputReference, Output ))
        guaranteedUtxos =
            preProcessedIntents.guaranteedUtxos
                |> List.foldl
                    (\ref acc ->
                        Dict.Any.get ref localStateUtxos
                            |> Maybe.map
                                (\output ->
                                    case Dict.Any.get output.address acc of
                                        Nothing ->
                                            Dict.Any.insert output.address [ ( ref, output ) ] acc

                                        Just utxos ->
                                            Dict.Any.insert output.address (( ref, output ) :: utxos) acc
                                )
                            |> Maybe.withDefault acc
                    )
                    Address.emptyDict
    in
    validMinAdaPerOutput (preCreated TxContext.new).outputs
        |> Result.mapError NotEnoughMinAda
        |> Result.andThen (\_ -> validateSpentOutputs localStateUtxos spendings)
        |> Result.andThen (\_ -> validateWithdrawals localStateUtxos withdrawals)
        |> Result.andThen (\_ -> validateVotes localStateUtxos allVotes)
        |> Result.andThen (\_ -> validateGuardrails localStateUtxos govState preProcessedIntents)
        |> Result.map
            (\allPlutusScriptSources ->
                let
                    -- Dedup required signers
                    requiredSigners =
                        List.concat preProcessedIntents.requiredSigners
                            |> List.map (\signer -> ( signer, () ))
                            |> Map.fromList
                            |> Map.keys

                    -- Dedup expected signers
                    expectedSigners =
                        List.concat preProcessedIntents.expectedSigners
                            |> List.map (\signer -> ( signer, () ))
                            |> Map.fromList
                            |> Map.keys
                in
                { freeInputs = preProcessedIntents.freeInputs
                , freeOutputs = preProcessedIntents.freeOutputs
                , guaranteedUtxos = guaranteedUtxos
                , preSelected = preSelected
                , preCreated = preCreated

                -- TODO: an improvement would consist in fetching the referenced from the local state utxos,
                -- and extract the script values, to even remove duplicates both in ref and values.
                , nativeScriptSources = List.Extra.uniqueBy (Witness.toHex Script.encodeNativeScript) preProcessedIntents.nativeScriptSources
                , plutusScriptSources = List.Extra.uniqueBy (Tuple.second >> Witness.toHex Bytes.toCbor) allPlutusScriptSources
                , datumSources = List.Extra.uniqueBy (Witness.toHex Data.toCbor) preProcessedIntents.datumSources
                , expectedSigners = expectedSigners
                , requiredSigners = requiredSigners
                , totalMinted = totalMintedAndBurned
                , mintRedeemers =
                    List.map (\m -> ( m.policyId, m.redeemer )) preProcessedIntents.mints
                        |> Map.fromList
                , withdrawals =
                    List.map (\w -> ( w.stakeAddress, { amount = w.amount, redeemer = w.redeemer } )) preProcessedIntents.withdrawals
                        |> Address.stakeDictFromList
                , certificates = preProcessedIntents.certificates
                , proposals =
                    preProcessedIntents.proposalIntents
                        |> List.map
                            (\{ govAction, offchainInfo, deposit, depositReturnAccount } ->
                                ( { deposit = deposit
                                  , depositReturnAccount = depositReturnAccount
                                  , anchor = offchainInfo
                                  , govAction = actionFromIntent govState govAction
                                  }
                                , proposalRedeemer govAction
                                )
                            )
                , votes =
                    preProcessedIntents.votes
                        |> List.map (\{ voter, votes, redeemer } -> ( voter, { votes = votes, redeemer = redeemer } ))
                        |> Gov.voterDictFromList
                }
            )


{-| Helper function to convert an action proposal intent into an actual one.
-}
actionFromIntent : GovernanceState -> ActionProposal -> Action
actionFromIntent govState actionIntent =
    case actionIntent of
        ParameterChange protocolParamUpdate ->
            Gov.ParameterChange
                { latestEnacted = govState.lastEnactedProtocolParamUpdateAction
                , protocolParamUpdate = protocolParamUpdate
                , guardrailsPolicy = Maybe.map .policyId govState.guardrailsScript
                }

        HardForkInitiation protocolVersion ->
            Gov.HardForkInitiation
                { latestEnacted = govState.lastEnactedHardForkAction
                , protocolVersion = protocolVersion
                }

        TreasuryWithdrawals withdrawals ->
            Gov.TreasuryWithdrawals
                { withdrawals = List.map (\w -> ( w.destination, w.amount )) withdrawals
                , guardrailsPolicy = Maybe.map .policyId govState.guardrailsScript
                }

        NoConfidence ->
            Gov.NoConfidence
                { latestEnacted = govState.lastEnactedCommitteeAction
                }

        UpdateCommittee updateInfo ->
            Gov.UpdateCommittee
                { latestEnacted = govState.lastEnactedCommitteeAction
                , removedMembers = updateInfo.removeMembers
                , addedMembers = updateInfo.addMembers
                , quorumThreshold = updateInfo.quorumThreshold
                }

        NewConstitution constitution ->
            Gov.NewConstitution
                { latestEnacted = govState.lastEnactedConstitutionAction
                , constitution = constitution
                }

        Info ->
            Gov.Info


{-| Helper function to generate the redeemers for potential guardrails script execution.
-}
proposalRedeemer : ActionProposal -> Maybe Data
proposalRedeemer govAction =
    case govAction of
        ParameterChange _ ->
            Just (Data.Int Integer.zero)

        TreasuryWithdrawals _ ->
            Just (Data.Int Integer.zero)

        _ ->
            Nothing


{-| Helper function
-}
addPreSelectedInput :
    OutputReference
    -> Value
    -> Maybe (TxContext -> Data)
    -> { sum : Value, inputs : Utxo.RefDict (Maybe (TxContext -> Data)) }
    -> { sum : Value, inputs : Utxo.RefDict (Maybe (TxContext -> Data)) }
addPreSelectedInput ref value maybeRedeemer { sum, inputs } =
    { sum = Value.add value sum
    , inputs = Dict.Any.insert ref maybeRedeemer inputs
    }


validMinAdaPerOutput : List Output -> Result String ()
validMinAdaPerOutput outputs =
    case outputs of
        [] ->
            Ok ()

        output :: rest ->
            case Utxo.checkMinAda output of
                Ok _ ->
                    validMinAdaPerOutput rest

                Err err ->
                    Err err


{-| Do as many checks as we can on user-provided votes.

In particular, check that:

  - there is no duplicate voter
  - there is no empty list of votes
  - all voter witnesses are valid

-}
validateVotes : Utxo.RefDict Output -> List ( Witness.Voter, List VoteIntent ) -> Result TxFinalizationError ()
validateVotes localStateUtxos votes =
    let
        voterIds =
            List.map (Tuple.first >> voterWitnessToBech32) votes

        duplicateVoters =
            List.Extra.frequencies voterIds
                |> List.filter (\( _, freq ) -> freq > 1)
                |> List.map Tuple.first
                |> List.map (\v -> { voter = v })
    in
    if not (List.isEmpty duplicateVoters) then
        Err <| DuplicateVoters duplicateVoters

    else
        List.map (checkVoter localStateUtxos) votes
            |> Result.Extra.combine
            |> Result.map (\_ -> ())


checkVoter : Utxo.RefDict Output -> ( Witness.Voter, List VoteIntent ) -> Result TxFinalizationError ()
checkVoter localStateUtxos ( voterWitness, votes ) =
    if List.isEmpty votes then
        Err <| EmptyVotes { voter = voterWitnessToBech32 voterWitness }

    else
        case voterWitness of
            Witness.WithCommitteeHotCred (Witness.WithScript hash scriptWitness) ->
                Witness.checkScript localStateUtxos hash scriptWitness
                    |> Result.mapError WitnessError

            Witness.WithDrepCred (Witness.WithScript hash scriptWitness) ->
                Witness.checkScript localStateUtxos hash scriptWitness
                    |> Result.mapError WitnessError

            _ ->
                Ok ()


voterWitnessToBech32 : Witness.Voter -> String
voterWitnessToBech32 voterWitness =
    Witness.toVoter voterWitness
        |> Gov.voterToId
        |> Gov.idToBech32


{-| Do as many checks as we can on user-provided withdrawals.
-}
validateWithdrawals : Utxo.RefDict Output -> List ( StakeAddress, Maybe Witness.Script ) -> Result TxFinalizationError ()
validateWithdrawals localStateUtxos withdrawals =
    List.map (checkWithdrawal localStateUtxos) withdrawals
        |> Result.Extra.combine
        |> Result.map (\_ -> ())


{-| Do as many checks as we can on user-provided withdrawals.

We can check things like:

  - check address is of correct type (key/script)
  - check script witness (same hash, ...)
  - check datum witness (same hash, ...)

-}
checkWithdrawal : Utxo.RefDict Output -> ( StakeAddress, Maybe Witness.Script ) -> Result TxFinalizationError ()
checkWithdrawal localStateUtxos ( stakeAddress, maybeWitness ) =
    case ( stakeAddress.stakeCredential, maybeWitness ) of
        ( VKeyHash _, Nothing ) ->
            Ok ()

        ( ScriptHash scriptHash, Just (Witness.Native nativeScriptWitness) ) ->
            Witness.checkNativeScript localStateUtxos scriptHash nativeScriptWitness
                |> Result.mapError WitnessError

        ( ScriptHash scriptHash, Just (Witness.Plutus plutusScriptWitness) ) ->
            Witness.checkPlutusScript localStateUtxos scriptHash plutusScriptWitness
                |> Result.mapError WitnessError

        ( VKeyHash _, Just _ ) ->
            Err <| InvalidStakeAddress stakeAddress "You are providing a script witness, but this stake address is for a public key withdrawal, not a script"

        ( ScriptHash _, Nothing ) ->
            Err <| InvalidStakeAddress stakeAddress "This stake address is for a script, but you didn’t provide a script witness for the withdrawal"


{-| Do as many checks as we can on user-provided spendings.
-}
validateSpentOutputs : Utxo.RefDict Output -> List SpendSource -> Result TxFinalizationError ()
validateSpentOutputs localStateUtxos spendings =
    List.map (checkSpentSource localStateUtxos) spendings
        |> Result.Extra.combine
        |> Result.map (\_ -> ())


{-| Do as many checks as we can on user-provided spendings.

We can check things like:

  - check address is of correct type (key/script)
  - check script witness (same hash, ...)
  - check datum witness (same hash, ...)

-}
checkSpentSource : Utxo.RefDict Output -> SpendSource -> Result TxFinalizationError ()
checkSpentSource localStateUtxos spending =
    case spending of
        FromWallet walletSpending ->
            checkWalletSpending localStateUtxos walletSpending

        FromNativeScript { spentInput, nativeScriptWitness } ->
            getUtxo localStateUtxos spentInput
                |> Result.andThen (\output -> checkNativeScriptSpending localStateUtxos output nativeScriptWitness)

        FromPlutusScript { spentInput, datumWitness, plutusScriptWitness } ->
            getUtxo localStateUtxos spentInput
                |> Result.andThen
                    (\output ->
                        Witness.checkDatum localStateUtxos output.datumOption datumWitness
                            |> Result.mapError WitnessError
                            |> Result.andThen (\_ -> checkPlutusScriptSpending localStateUtxos output plutusScriptWitness)
                    )


checkWalletSpending : Utxo.RefDict Output -> { a | address : Address, guaranteedUtxos : List OutputReference } -> Result TxFinalizationError ()
checkWalletSpending localStateUtxos { address, guaranteedUtxos } =
    let
        checkRefAddressIsKey ref =
            getUtxo localStateUtxos ref
                |> Result.andThen (\output -> checkKeyAddressSpending output.address)
    in
    checkKeyAddressSpending address
        |> Result.andThen
            (\_ ->
                List.map checkRefAddressIsKey guaranteedUtxos
                    |> Result.Extra.combine
                    |> Result.map (\_ -> ())
            )


checkNativeScriptSpending : Utxo.RefDict Output -> Output -> Witness.NativeScript -> Result TxFinalizationError ()
checkNativeScriptSpending localStateUtxos spentInput nativeScriptWitness =
    checkScriptAddressSpending spentInput.address
        |> Result.andThen
            (\scriptHash ->
                Witness.checkNativeScript localStateUtxos scriptHash nativeScriptWitness
                    |> Result.mapError WitnessError
            )


checkPlutusScriptSpending : Utxo.RefDict Output -> Output -> Witness.PlutusScript -> Result TxFinalizationError ()
checkPlutusScriptSpending localStateUtxos spentInput plutusScriptWitness =
    checkScriptAddressSpending spentInput.address
        |> Result.andThen
            (\expectedHash ->
                Witness.checkPlutusScript localStateUtxos expectedHash plutusScriptWitness
                    |> Result.mapError WitnessError
            )


getUtxo : Utxo.RefDict Output -> OutputReference -> Result TxFinalizationError Output
getUtxo utxos ref =
    Dict.Any.get ref utxos
        |> Result.fromMaybe (WitnessError <| Witness.ReferenceOutputsMissingFromLocalState [ ref ])


checkAddressSpending : Address -> (Credential -> Result TxFinalizationError a) -> Result TxFinalizationError a
checkAddressSpending address credentialValidator =
    case address of
        Byron _ ->
            Err <| InvalidAddress address "Byron addresses not supported"

        Reward _ ->
            Err <| InvalidAddress address "Reward address cannot be spent"

        Shelley { paymentCredential } ->
            credentialValidator paymentCredential


checkKeyAddressSpending : Address -> Result TxFinalizationError ()
checkKeyAddressSpending address =
    checkAddressSpending address <|
        \credential ->
            case credential of
                VKeyHash _ ->
                    Ok ()

                ScriptHash _ ->
                    Err <| InvalidAddress address "This is a script address, not a regular wallet key address"


checkScriptAddressSpending : Address -> Result TxFinalizationError (Bytes CredentialHash)
checkScriptAddressSpending address =
    checkAddressSpending address <|
        \credential ->
            case credential of
                VKeyHash _ ->
                    Err <| InvalidAddress address "This is a regular wallet key address, not a script address"

                ScriptHash scriptHash ->
                    Ok scriptHash


{-| Helper function that check the witness of the guardrails script and update the list of Plutus scripts if needed.
-}
validateGuardrails : Utxo.RefDict Output -> GovernanceState -> PreProcessedIntents -> Result TxFinalizationError (List ( PlutusVersion, Witness.Source (Bytes ScriptCbor) ))
validateGuardrails localStateUtxos govState preProcessedIntents =
    -- If there is any proposal requiring the guardrails script, update the plutus script sources
    let
        -- Helper to check if a given proposal requires the guardrails script execution
        requiresGuardrails proposalIntent =
            case proposalIntent.govAction of
                ParameterChange _ ->
                    True

                TreasuryWithdrawals _ ->
                    True

                _ ->
                    False
    in
    if List.any requiresGuardrails preProcessedIntents.proposalIntents then
        case govState.guardrailsScript of
            Just { policyId, plutusVersion, scriptWitness } ->
                let
                    guardrailsDummyPlutusScriptWitness =
                        { script = ( plutusVersion, scriptWitness )
                        , redeemerData = \_ -> Data.List []
                        , requiredSigners = []
                        }
                in
                Witness.checkPlutusScript localStateUtxos policyId guardrailsDummyPlutusScriptWitness
                    |> Result.mapError WitnessError
                    |> Result.map (\_ -> ( plutusVersion, scriptWitness ) :: preProcessedIntents.plutusScriptSources)

            Nothing ->
                Ok preProcessedIntents.plutusScriptSources

    else
        Ok preProcessedIntents.plutusScriptSources


type alias ProcessedOtherInfo =
    { referenceInputs : List OutputReference
    , metadata : List { tag : Natural, metadata : Metadatum }
    , timeValidityRange : Maybe { start : Int, end : Natural }
    }


noInfo : ProcessedOtherInfo
noInfo =
    { referenceInputs = []
    , metadata = []
    , timeValidityRange = Nothing
    }


processOtherInfo : List TxOtherInfo -> Result TxFinalizationError ProcessedOtherInfo
processOtherInfo otherInfo =
    let
        processedOtherInfo =
            List.foldl
                (\info acc ->
                    case info of
                        TxReferenceInput ref ->
                            { acc | referenceInputs = ref :: acc.referenceInputs }

                        TxMetadata m ->
                            { acc | metadata = m :: acc.metadata }

                        TxTimeValidityRange ({ start, end } as newVR) ->
                            { acc
                                | timeValidityRange =
                                    case acc.timeValidityRange of
                                        Nothing ->
                                            Just newVR

                                        Just vr ->
                                            Just { start = max start vr.start, end = Natural.min end vr.end }
                            }
                )
                noInfo
                otherInfo

        -- Check if there are duplicate metadata tags.
        -- (use Int instead of Natural for this purpose)
        metadataTags =
            List.map (.tag >> Natural.toInt) processedOtherInfo.metadata

        hasDuplicatedMetadataTags =
            List.length metadataTags /= Set.size (Set.fromList metadataTags)

        -- Check that the time range intersection is still valid
        validTimeRange =
            case processedOtherInfo.timeValidityRange of
                Nothing ->
                    True

                Just range ->
                    Natural.fromSafeInt range.start |> Natural.isLessThan range.end
    in
    if hasDuplicatedMetadataTags then
        let
            findDuplicate current tags =
                case tags of
                    [] ->
                        Nothing

                    t :: biggerTags ->
                        if t == current then
                            Just t

                        else
                            findDuplicate t biggerTags

            dupTag =
                findDuplicate -1 (List.sort metadataTags)
                    |> Maybe.withDefault -1
        in
        Err <| DuplicatedMetadataTags dupTag

    else if not validTimeRange then
        Err <| IncorrectTimeValidityRange <| "Invalid time range (or intersection of multiple time ranges). The time range end must be > than the start." ++ Debug.toString processedOtherInfo.timeValidityRange

    else
        Ok processedOtherInfo


{-| Perform coin selection for the required input per address.

For each address, create outputs with the change.
The output must satisfy minAda.

-}
computeCoinSelection :
    Utxo.RefDict Output
    -> Fee
    -> ProcessedIntents
    -> CoinSelection.Algorithm
    -> Result TxFinalizationError (Address.Dict { selectedUtxos : List ( OutputReference, Output ), changeOutputs : List Output })
computeCoinSelection localStateUtxos fee processedIntents coinSelectionAlgo =
    let
        -- Inputs not available for selection because already manually preselected
        -- Available inputs are the ones which are not ... unavailable! (logic)
        -- Group them per address
        insertInUtxoListDict ref output =
            Dict.Any.update output.address
                (Just << (::) ( ref, output ) << Maybe.withDefault [])

        -- Add the fee to free inputs
        addFee : Address -> Natural -> Address.Dict Value -> Address.Dict Value
        addFee addr amount dict =
            Dict.Any.update addr (Just << Value.add (Value.onlyLovelace amount) << Maybe.withDefault Value.zero) dict

        freeInputsWithFee : Address.Dict Value
        freeInputsWithFee =
            case fee of
                ManualFee perAddressFee ->
                    List.foldl
                        (\{ paymentSource, exactFeeAmount } -> addFee paymentSource exactFeeAmount)
                        processedIntents.freeInputs
                        perAddressFee

                AutoFee { paymentSource } ->
                    addFee paymentSource defaultAutoFee processedIntents.freeInputs

        -- These are the free outputs that are unrelated to any address with fees or free input.
        -- Keys only contain addresses that do not appear in freeInputsWithFee.
        independentFreeOutputValues : Address.Dict Value
        independentFreeOutputValues =
            Dict.Any.diff processedIntents.freeOutputs freeInputsWithFee

        -- These will require they have enough minAda to make their own independent outputs.
        validIndependentFreeOutputs : Result TxFinalizationError (Address.Dict Output)
        validIndependentFreeOutputs =
            independentFreeOutputValues
                |> Dict.Any.map (\addr output -> Utxo.checkMinAda <| Utxo.simpleOutput addr output)
                |> resultDictJoin
                |> Result.mapError NotEnoughMinAda

        -- These are the free outputs that are related to any address with fees or free input.
        -- Keys are a subset of the address in freeInputsWithFee
        -- Build the per-address coin selection context.
        -- Merge the two dicts:
        --   - freeInputsWithFee (that will become the coin selection target value)
        --   - relatedFreeOutputValues (that will be added to the coin selection change)
        perAddressContext : Address.Dict CoinSelection.PerAddressContext
        perAddressContext =
            let
                dummyOutput =
                    Output (Byron Bytes.empty) Value.zero Nothing Nothing

                notAvailableUtxos =
                    -- Using dummyOutput to have the same type as localStateUtxos
                    Dict.Any.map (\_ _ -> dummyOutput) processedIntents.preSelected.inputs

                availableUtxos : Address.Dict (List ( OutputReference, Output ))
                availableUtxos =
                    Dict.Any.diff localStateUtxos notAvailableUtxos
                        --> Utxo.RefDict Output
                        |> Dict.Any.foldl insertInUtxoListDict Address.emptyDict

                relatedFreeOutputValues : Address.Dict Value
                relatedFreeOutputValues =
                    Dict.Any.diff processedIntents.freeOutputs independentFreeOutputValues

                context addr targetValue alreadyOwed =
                    { targetValue = targetValue
                    , alreadyOwed = alreadyOwed
                    , availableUtxos = Dict.Any.get addr availableUtxos |> Maybe.withDefault []
                    , alreadySelectedUtxos = []
                    }

                whenInput addr v =
                    Dict.Any.insert addr (context addr v Value.zero)

                whenOutput addr v =
                    Dict.Any.insert addr (context addr Value.zero v)

                whenBoth addr input output =
                    Dict.Any.insert addr (context addr input output)
            in
            Dict.Any.merge
                whenInput
                whenBoth
                whenOutput
                freeInputsWithFee
                relatedFreeOutputValues
                Address.emptyDict

        perAddressConfig : Address -> CoinSelection.PerAddressConfig
        perAddressConfig _ =
            -- Default to the same algorithm on all addresses
            { selectionAlgo = coinSelectionAlgo

            -- Default to no normalization
            -- TODO: simplify by default?
            , normalizationAlgo = \{ target, owed } -> { normalizedTarget = target, normalizedOwed = owed }

            -- Default to single change output
            -- TODO: If there is more than 5 ada free in the change (after minAda),
            -- also create a pure-ada output so that we don’t deplete all outputs viable for collateral.
            , changeAlgo = \value -> [ value ]
            }

        coinSelectionAndChangeOutputs =
            CoinSelection.perAddress perAddressConfig perAddressContext
                |> Result.mapError FailedToPerformCoinSelection
    in
    Result.map2
        (Dict.Any.foldl (\addr output -> Dict.Any.insert addr { selectedUtxos = [], changeOutputs = [ output ] }))
        coinSelectionAndChangeOutputs
        validIndependentFreeOutputs


{-| Helper function to join Dict Result into Result Dict.
-}
resultDictJoin : AnyDict comparable key (Result err value) -> Result err (AnyDict comparable key value)
resultDictJoin dict =
    Dict.Any.foldl (\key -> Result.map2 (Dict.Any.insert key)) (Ok <| Dict.Any.removeAll dict) dict


{-| Build the Transaction from the processed intents and the latest inputs/outputs.
-}
buildTx :
    Natural
    -> CoinSelection.Selection
    -> ProcessedIntents
    -> ProcessedOtherInfo
    -> TxContext
    -> TxFinalized
buildTx feeAmount collateralSelection processedIntents otherInfo txContext =
    let
        -- WitnessSet ######################################
        --
        ( nativeScripts, nativeScriptRefs ) =
            split Witness.sourceToResult processedIntents.nativeScriptSources

        ( plutusScripts, plutusScriptRefs ) =
            splitScripts processedIntents.plutusScriptSources

        ( datumWitnessValues, datumWitnessRefs ) =
            split Witness.sourceToResult processedIntents.datumSources

        -- Compute datums for pre-selected inputs.
        preSelected : Utxo.RefDict (Maybe Data)
        preSelected =
            processedIntents.preSelected.inputs
                |> Dict.Any.map (\_ -> Maybe.map (\f -> f txContext))

        -- Add a default Nothing to all inputs picked by the selection algorithm.
        algoSelected : Utxo.RefDict (Maybe Data)
        algoSelected =
            List.map (\( ref, _ ) -> ( ref, Nothing )) txContext.inputs
                |> Utxo.refDictFromList
                |> (\allSpent -> Dict.Any.diff allSpent preSelected)

        -- Helper
        makeRedeemer : RedeemerTag -> Int -> Data -> Redeemer
        makeRedeemer tag id data =
            { tag = tag
            , index = id
            , data = data
            , exUnits = { mem = 0, steps = 0 }
            }

        -- Build the spend redeemers while keeping the index of the sorted inputs.
        sortedSpendRedeemers : List Redeemer
        sortedSpendRedeemers =
            Dict.Any.union preSelected algoSelected
                |> Dict.Any.toList
                |> List.indexedMap
                    (\id ( _, maybeDatum ) ->
                        Maybe.map (makeRedeemer Redeemer.Spend id) maybeDatum
                    )
                |> List.filterMap identity

        -- Build the mint redeemers while keeping the index of the sorted order of policy IDs.
        sortedMintRedeemers : List Redeemer
        sortedMintRedeemers =
            Map.values processedIntents.mintRedeemers
                |> List.indexedMap
                    (\id maybeRedeemerF ->
                        Maybe.map
                            (\redeemerF -> makeRedeemer Redeemer.Mint id (redeemerF txContext))
                            maybeRedeemerF
                    )
                |> List.filterMap identity

        -- The StakeDict used for the withdrawals field
        -- uses the same ordering as Haskell (with script credentials first)
        sortedWithdrawals : List ( StakeAddress, Natural, Maybe Data )
        sortedWithdrawals =
            Dict.Any.toList processedIntents.withdrawals
                |> List.map (\( addr, w ) -> ( addr, w.amount, Maybe.map (\f -> f txContext) w.redeemer ))

        -- Build the withdrawals redeemers while keeping the index in the sorted list.
        sortedWithdrawalsRedeemers : List Redeemer
        sortedWithdrawalsRedeemers =
            sortedWithdrawals
                |> List.indexedMap
                    (\id ( _, _, maybeDatum ) ->
                        Maybe.map (makeRedeemer Redeemer.Reward id) maybeDatum
                    )
                |> List.filterMap identity

        -- No need to sort certificates redeemers
        certRedeemers : List Redeemer
        certRedeemers =
            processedIntents.certificates
                |> List.indexedMap
                    (\id ( _, maybeRedeemerF ) ->
                        Maybe.map
                            (\redeemerF -> makeRedeemer Redeemer.Cert id (redeemerF txContext))
                            maybeRedeemerF
                    )
                |> List.filterMap identity

        -- No need to sort proposals redeemers
        proposalRedeemers : List Redeemer
        proposalRedeemers =
            processedIntents.proposals
                |> List.indexedMap
                    (\id ( _, maybeData ) ->
                        Maybe.map (makeRedeemer Redeemer.Propose id) maybeData
                    )
                |> List.filterMap identity

        -- Sort votes with the Voter order
        sortedVotes : List ( Voter, { votes : List VoteIntent, redeemer : Maybe (TxContext -> Data) } )
        sortedVotes =
            Dict.Any.toList processedIntents.votes

        -- Build the Vote redeemer with the same order as txVotes
        voteRedeemers : List Redeemer
        voteRedeemers =
            sortedVotes
                |> List.indexedMap
                    (\id ( _, { redeemer } ) ->
                        Maybe.map
                            (\redeemerF -> makeRedeemer Redeemer.Vote id (redeemerF txContext))
                            redeemer
                    )
                |> List.filterMap identity

        -- Look for inputs at addresses that will need signatures
        walletCredsInInputs : List (Bytes CredentialHash)
        walletCredsInInputs =
            txContext.inputs
                |> List.filterMap (\( _, output ) -> Address.extractPubKeyHash output.address)

        -- Look for stake credentials needed for withdrawals
        withdrawalsStakeCreds : List (Bytes CredentialHash)
        withdrawalsStakeCreds =
            Dict.Any.keys processedIntents.withdrawals
                |> List.filterMap (\stakeAddress -> Address.extractCredentialKeyHash stakeAddress.stakeCredential)

        -- Look for stake credentials needed for certificates
        certificatesCreds : List (Bytes CredentialHash)
        certificatesCreds =
            List.map Tuple.first processedIntents.certificates
                |> List.concatMap extractCertificateCred

        -- Look for credentials needed for votes
        votesCreds : List (Bytes CredentialHash)
        votesCreds =
            List.filterMap (Tuple.first >> Gov.voterKeyCred) sortedVotes

        -- Find all the hashes of credentials expected to provide a signature
        allExpectedSignatures : List (Bytes CredentialHash)
        allExpectedSignatures =
            [ processedIntents.requiredSigners
            , processedIntents.expectedSigners
            , walletCredsInInputs
            , withdrawalsStakeCreds
            , certificatesCreds
            , votesCreds
            ]
                |> List.concat
                |> List.map (\cred -> ( cred, {} ))
                |> Map.fromList
                |> Map.keys

        -- Create a dummy VKey Witness for each input wallet address or required signer
        -- so that fees are correctly estimated.
        placeholderVKeyWitness : List VKeyWitness
        placeholderVKeyWitness =
            allExpectedSignatures
                |> List.map
                    (\cred ->
                        -- Try keeping the 28 bytes of the credential hash at the start if it’s an actual cred
                        -- or prefix with VKEY and SIGNATURE for fake creds in textual shape (used in tests).
                        let
                            credStr =
                                Bytes.pretty cred
                        in
                        if credStr == Bytes.toHex cred then
                            { vkey = Bytes.dummyWithPrefix 32 cred
                            , signature = Bytes.dummyWithPrefix 64 cred
                            }

                        else
                            { vkey = Bytes.dummy 32 <| "VKEY" ++ credStr
                            , signature = Bytes.dummy 64 <| "SIGNATURE" ++ credStr
                            }
                    )

        txWitnessSet : WitnessSet
        txWitnessSet =
            { vkeywitness = nothingIfEmptyList placeholderVKeyWitness
            , bootstrapWitness = Nothing
            , plutusData = nothingIfEmptyList datumWitnessValues
            , nativeScripts = nothingIfEmptyList nativeScripts
            , plutusV1Script = nothingIfEmptyList <| filterScriptVersion PlutusV1 plutusScripts
            , plutusV2Script = nothingIfEmptyList <| filterScriptVersion PlutusV2 plutusScripts
            , plutusV3Script = nothingIfEmptyList <| filterScriptVersion PlutusV3 plutusScripts
            , redeemer =
                nothingIfEmptyList <|
                    List.concat
                        -- Order is the same as in the Plutus script context.
                        -- See test "where the redeemers are correctly sorted".
                        [ sortedSpendRedeemers
                        , sortedMintRedeemers
                        , certRedeemers
                        , sortedWithdrawalsRedeemers
                        , voteRedeemers
                        , proposalRedeemers
                        ]
            }

        -- AuxiliaryData ###################################
        --
        txAuxData : Maybe AuxiliaryData
        txAuxData =
            if List.isEmpty otherInfo.metadata then
                Nothing

            else
                List.map (\{ tag, metadata } -> ( tag, metadata )) otherInfo.metadata
                    |> AuxiliaryData.fromJustLabels
                    |> Just

        -- TransactionBody #################################
        --
        -- Regroup all OutputReferences from witnesses
        -- Ref inputs are sorted (important).
        allReferenceInputs =
            List.concat
                [ List.map Tuple.first txContext.referenceInputs
                , otherInfo.referenceInputs
                , nativeScriptRefs
                , plutusScriptRefs
                , datumWitnessRefs
                ]
                |> List.map (\ref -> ( ref, () ))
                |> Utxo.refDictFromList
                |> Dict.Any.keys

        collateralReturnAmount =
            (Maybe.withDefault Value.zero collateralSelection.change).lovelace

        collateralReturn : Maybe Output
        collateralReturn =
            List.head collateralSelection.selectedUtxos
                |> Maybe.map (\( _, output ) -> Utxo.fromLovelace output.address collateralReturnAmount)

        totalCollateral : Maybe Int
        totalCollateral =
            if List.isEmpty collateralSelection.selectedUtxos then
                Nothing

            else
                collateralSelection.selectedUtxos
                    |> List.foldl (\( _, o ) -> Natural.add o.amount.lovelace) Natural.zero
                    |> (\sumCollateralInputs -> Natural.sub sumCollateralInputs collateralReturnAmount)
                    |> Natural.toInt
                    |> Just

        updatedTxBody : TransactionBody
        updatedTxBody =
            { inputs = List.map Tuple.first txContext.inputs
            , outputs = txContext.outputs
            , fee = feeAmount
            , ttl = Maybe.map .end otherInfo.timeValidityRange
            , certificates = List.map Tuple.first processedIntents.certificates
            , withdrawals = List.map (\( addr, amount, _ ) -> ( addr, amount )) sortedWithdrawals
            , update = Nothing
            , auxiliaryDataHash =
                if List.isEmpty otherInfo.metadata then
                    Nothing

                else
                    Just (Bytes.dummy 32 "AuxDataHash")
            , validityIntervalStart = Maybe.map .start otherInfo.timeValidityRange
            , mint = processedIntents.totalMinted
            , scriptDataHash =
                if txWitnessSet.redeemer == Nothing && txWitnessSet.plutusData == Nothing then
                    Nothing

                else
                    Just (Bytes.dummy 32 "ScriptDataHash")
            , collateral = List.map Tuple.first collateralSelection.selectedUtxos
            , requiredSigners = processedIntents.requiredSigners
            , networkId = Nothing -- not mandatory
            , collateralReturn = collateralReturn
            , totalCollateral = totalCollateral
            , referenceInputs = allReferenceInputs
            , votingProcedures =
                sortedVotes
                    |> List.map (Tuple.mapSecond (\{ votes } -> List.map (\{ actionId, vote, rationale } -> ( actionId, Gov.VotingProcedure vote rationale )) votes))
            , proposalProcedures = List.map Tuple.first processedIntents.proposals
            , currentTreasuryValue = Nothing -- TODO currentTreasuryValue
            , treasuryDonation = Nothing -- TODO treasuryDonation
            }
    in
    { tx =
        { body = updatedTxBody
        , witnessSet = txWitnessSet
        , isValid = True
        , auxiliaryData = txAuxData
        }
    , expectedSignatures = allExpectedSignatures
    }


{-| Helper to extract the credential associated with a certificate.
-}
extractCertificateCred : Certificate -> List (Bytes CredentialHash)
extractCertificateCred cert =
    case cert of
        StakeRegistrationCert _ ->
            -- not needed, but this will be deprecated anyway
            []

        StakeDeregistrationCert { delegator } ->
            List.filterMap identity [ Address.extractCredentialKeyHash delegator ]

        StakeDelegationCert { delegator } ->
            List.filterMap identity [ Address.extractCredentialKeyHash delegator ]

        PoolRegistrationCert { operator, poolOwners } ->
            operator :: poolOwners

        PoolRetirementCert { poolId } ->
            [ poolId ]

        -- Not handled, deprecated
        GenesisKeyDelegationCert _ ->
            []

        -- Not handled, deprecated
        MoveInstantaneousRewardsCert _ ->
            []

        RegCert { delegator } ->
            List.filterMap identity [ Address.extractCredentialKeyHash delegator ]

        UnregCert { delegator } ->
            List.filterMap identity [ Address.extractCredentialKeyHash delegator ]

        VoteDelegCert { delegator } ->
            List.filterMap identity [ Address.extractCredentialKeyHash delegator ]

        StakeVoteDelegCert { delegator } ->
            List.filterMap identity [ Address.extractCredentialKeyHash delegator ]

        StakeRegDelegCert { delegator } ->
            List.filterMap identity [ Address.extractCredentialKeyHash delegator ]

        VoteRegDelegCert { delegator } ->
            List.filterMap identity [ Address.extractCredentialKeyHash delegator ]

        StakeVoteRegDelegCert { delegator } ->
            List.filterMap identity [ Address.extractCredentialKeyHash delegator ]

        AuthCommitteeHotCert _ ->
            Debug.todo "How many signatures for AuthCommitteeHotCert?"

        ResignCommitteeColdCert _ ->
            Debug.todo "How many signatures for ResignCommitteeColdCert?"

        RegDrepCert { drepCredential } ->
            List.filterMap identity [ Address.extractCredentialKeyHash drepCredential ]

        UnregDrepCert { drepCredential } ->
            List.filterMap identity [ Address.extractCredentialKeyHash drepCredential ]

        UpdateDrepCert { drepCredential } ->
            List.filterMap identity [ Address.extractCredentialKeyHash drepCredential ]


{-| Update the known local state with the spent and created UTxOs of a given transaction.
-}
updateLocalState :
    Bytes TransactionId
    -> Transaction
    -> Utxo.RefDict Output
    ->
        { updatedState : Utxo.RefDict Output
        , spent : List ( OutputReference, Output )
        , created : List ( OutputReference, Output )
        }
updateLocalState txId tx oldState =
    let
        unspent =
            List.foldl Dict.Any.remove oldState tx.body.inputs

        createdUtxos =
            List.indexedMap (\index output -> ( OutputReference txId index, output )) tx.body.outputs
    in
    { updatedState =
        List.foldl (\( ref, output ) state -> Dict.Any.insert ref output state) unspent createdUtxos
    , spent = List.filterMap (\ref -> Dict.Any.get ref oldState |> Maybe.map (Tuple.pair ref)) tx.body.inputs
    , created = createdUtxos
    }


splitScripts : List ( PlutusVersion, Witness.Source (Bytes ScriptCbor) ) -> ( List ( PlutusVersion, Bytes ScriptCbor ), List OutputReference )
splitScripts scripts =
    split (\( v, source ) -> Result.mapError (Tuple.pair v) <| Witness.sourceToResult source) scripts


split : (a -> Result err ok) -> List a -> ( List err, List ok )
split f items =
    List.foldr
        (\a ( accErr, accOk ) ->
            case f a of
                Err err ->
                    ( err :: accErr, accOk )

                Ok ok ->
                    ( accErr, ok :: accOk )
        )
        ( [], [] )
        items


{-| Helper
-}
nothingIfEmptyList : List a -> Maybe (List a)
nothingIfEmptyList list =
    if List.isEmpty list then
        Nothing

    else
        Just list


{-| Helper
-}
filterScriptVersion : Script.PlutusVersion -> List ( PlutusVersion, Bytes ScriptCbor ) -> List (Bytes ScriptCbor)
filterScriptVersion v =
    List.filterMap
        (\( version, script ) ->
            if version == v then
                Just script

            else
                Nothing
        )


{-| Adjust the steps/mem scripts execution costs with UPLC phase 2 evaluation of the transaction.
-}
adjustExecutionCosts : (Transaction -> Result String (List Redeemer)) -> Transaction -> Result TxFinalizationError Transaction
adjustExecutionCosts evalScriptsCosts tx =
    evalScriptsCosts tx
        |> Result.mapError UplcVmError
        |> Result.map
            (\redeemers ->
                if List.isEmpty redeemers then
                    tx

                else
                    let
                        witnessSet =
                            tx.witnessSet
                    in
                    { tx | witnessSet = { witnessSet | redeemer = Just redeemers } }
            )


{-| Final check for the Tx fees.
-}
checkInsufficientFee : { refScriptBytes : Int } -> Fee -> Transaction -> Result TxFinalizationError Transaction
checkInsufficientFee refSize fee tx =
    let
        declaredFee =
            tx.body.fee

        computedFee =
            Transaction.computeFees Transaction.defaultTxFeeParams refSize tx
                |> (\{ txSizeFee, scriptExecFee, refScriptSizeFee } -> Natural.add txSizeFee scriptExecFee |> Natural.add refScriptSizeFee)
    in
    if declaredFee |> Natural.isLessThan computedFee then
        case fee of
            ManualFee _ ->
                Err <| InsufficientManualFee { declared = declaredFee, computed = computedFee }

            AutoFee _ ->
                Err <| FailurePleaseReportToElmCardano "Insufficient AutoFee. Maybe we need another buildTx round?"

    else
        Ok tx
