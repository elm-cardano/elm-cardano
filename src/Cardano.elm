module Cardano exposing
    ( TxIntent(..), SpendSource(..), ScriptWitness(..), NativeScriptWitness, PlutusScriptWitness, WitnessSource(..)
    , CertificateIntent(..), CredentialWitness(..), VoterWitness(..)
    , VoteIntent, ProposalIntent, ActionProposal(..)
    , TxOtherInfo(..)
    , Fee(..)
    , finalize, finalizeAdvanced, TxFinalized, TxFinalizationError(..)
    , GovernanceState, emptyGovernanceState
    , updateLocalState
    )

{-| Cardano stuff


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
That’s enough theory, let’s get more concrete.

Let’s first define some addresses we are going to be using.

    addressFromHex addressHexString =
        Cardano.Address.fromBytes addressHexString
            |> Maybe.withDefault shouldNotErrorIfIsACorrectAddress

    ( me, you, someone ) =
        ( addressFromHex "01..."
        , addressFromHex "01..."
        , addressFromHex "01..."
        )

Here is a simple way to send 1 Ada to someone else.

    -- 1 Ada is 1000000 Lovelaces
    -- Asset amounts are typed with unbounded Natural numbers
    oneAda =
        Value.onlyLovelace (Natural.fromSafeString "1000000")

    -- We need to provide available UTxOs for Tx finalization.
    -- For this simple Tx, it only needs to know of our own UTxOs,
    -- that we would typically retrieve via an API provider.
    localStateUtxos =
        Utxo.refDictFromList myUtxos

    sendOneAdaToSomeoneTx =
        [ Spend (FromWallet { address = me, value = oneAda, guaranteedUtxos = [] })
        , SendTo someone oneAda
        ]
            |> finalize localStateUtxos []

The finalization step validates the Tx, compute the fees and add other required fields.

More control on the transfer is possible if we want to have multiple senders and receivers.
Here is an example where me and you both contribute 1 Ada.

    twoAda =
        Value.add oneAda oneAda

    localStateUtxos =
        Utxo.refDictFromList (myUtxos ++ yourUtxos)

    bothSendOneAdaToSomeoneTx =
        [ Spend (FromWallet { address = me, value = oneAda, guaranteedUtxos = [] })
        , Spend (FromWallet { address = you, value = oneAda, guaranteedUtxos = [] })
        , SendTo someone twoAda
        ]
            |> finalize localStateUtxos []

To mint or burn via a native script, here is what we can do.

    ( dogScriptRef, dogPolicyId, dogAssetName ) =
        Debug.todo "dog info is provided"

    ( catScriptRef, catPolicyId, catAssetName ) =
        Debug.todo "cat info is provided"

    localStateUtxos =
        Utxo.refDictFromList (myUtxos ++ scriptsRefsUtxos)

    mintAndBurnTx =
        -- minting 1 dog
        -- Mint amounts are of type Integer: unbounded positive or negative integers
        [ MintBurn
            { policyId = dogPolicyId
            , assets = Map.singleton dogAssetName Integer.one
            , scriptWitness = NativeWitness (WitnessReference dogScriptRef)
            }
        , SendTo me (Value.onlyToken dogPolicyId dogAssetName Natural.one)

        -- burning 1 cat
        , Spend <|
            FromWallet
                { address = me
                , value = Value.onlyToken catPolicyId catAssetName Natural.one
                , guaranteedUtxos = []
                }
        , MintBurn
            { policyId = catPolicyId
            , assets = Map.singleton catAssetName Integer.negativeOne
            , scriptWitness = NativeWitness (WitnessReference catScriptRef)
            }
        ]
            |> finalize localStateUtxos []

Let’s show how to use a native script to lock some tokens,
that can only be retrieved with our signature.

    -- Retrieve my public key credential from my address
    myKeyCred =
        Address.extractPubKeyHash me
            |> Maybe.withDefault dummyCredential

    -- Native script to lock funds with our public key
    lockScript =
        ScriptPubkey myKeyCred

    lockScriptHash =
        -- TODO: Script.hash
        Script.hash (Script.Native lockScript)

    -- Deriving the script address from the script hash
    scriptAddress =
        Address.Shelley
            { networkId = Mainnet
            , paymentCredential = ScriptHash lockScriptHash

            -- Adding our stake credential while we are at it
            -- so that our ada stays staked and yields staking rewards
            , stakeCredential = Address.extractStakeCredential me
            }

    localStateUtxos =
        Utxo.refDictFromList myUtxos

    nativeLockTx =
        [ Spend (FromWallet { address = me, value = twoAda, guaranteedUtxos = [] })
        , SendTo scriptAddress twoAda
        ]
            |> finalize localStateUtxos []

As you can see, we could even keep our stake credential
while locking our ada into the script address.
It means the locked ada will still be counted in our stake for the rewards.
This is thanks to Cardano addresses which have two parts.
The script logic only determines the first part of the address.

Let’s show an example how to spend utxos from this native script.
We want to retrieve 1 ada from it, and keep the other ada locked.
We need to do that in two actions.

1.  Spend the whole UTxO, with its 2 ada in it.
2.  Send 1 ada back to the same address.

We cannot partially spend UTxOs.
UTxOs are like bills, you spend them whole and get change for overspending.

    ( lockedUtxoRef, lockedOutput ) =
        -- TODO: Transaction.findOutputUtxosAt
        Transaction.findOutputUtxosAt scriptAddress nativeLockTx
            |> List.head

    { updatedState } =
        Cardano.updateLocalState txId nativeLockTx localStateUtxos

    nativeUnlockTx =
        [ Spend <|
            FromNativeScript
                -- spend the whole UTxO
                { spentInput = lockedUtxoRef

                -- This native script is so small,
                -- the easiest way to provide it is directly by value instead of by reference
                , nativeScriptWitness = WitnessValue lockScript
                }

        -- Retrieve 1 ada and send 1 ada back to the contract
        , SendTo me oneAda
        , SendTo scriptAddress oneAda
        ]
            |> finalize updatedState []

Alright, how about doing all those things with Plutus scripts now?
Plutus scripts can be used for many purposes such as minting,
spending funds or withdrawing staking rewards.

All script executions need to provide a "redeemer".
This is some mandatory piece of data provided as argument to the script function.
Transaction signatures required by the script must also be specified in a dedicated field.
This enables very efficient script executions since they can just check
that a public key is present in that `requiredSigners` field.

Let’s start with a simple minting and burning example.
For this example, we suppose the Plutus script was already written,
in some onchain language, like [Aiken](https://aiken-lang.org/).
This Plutus script will accept any mint or burn
as long as we present our signature in the transaction.
The redeemer is not used at all so we can define a dummy one,
of the smallest size possible since a redeemer is mandatory.

    ( dogScriptOutputRef, dogPolicyId, dogAssetName ) =
        Debug.todo "dog info is provided"

    ( catScriptOutputRef, catPolicyId, catAssetName ) =
        Debug.todo "cat info is provided"

    myKeyCred =
        Address.extractPubKeyHash me
            |> Maybe.withDefault dummyCredential

    -- Dummy redeemer of the smallest size possible.
    -- A redeemer is mandatory, but unchecked by this contract anyway.
    dummyRedeemer =
        Data.Int Integer.zero

    localStateUtxos =
        Utxo.refDictFromList (myUtxos ++ scriptsRefsUtxos)

    mintAndBurnTx =
        -- minting 1 dog
        [ MintBurn
            { policyId = dogPolicyId
            , assets = Map.singleton dogAssetName Integer.one
            , scriptWitness =
                PlutusWitness
                    { script = WitnessReference dogScriptOutputRef
                    , redeemerData = \_ -> dummyRedeemer
                    , requiredSigners = [ myKeyCred ]
                    }
            }
        , SendTo me (Value.onlyToken dogPolicyId dogAssetName Natural.one)

        -- burning 1 cat
        , Spend <|
            FromWallet
                { address = me
                , value = Value.onlyToken catPolicyId catAssetName Natural.one
                , guaranteedUtxos = []
                }
        , MintBurn
            { policyId = catPolicyId
            , assets = Map.singleton catAssetName Integer.negativeOne
            , scriptWitness =
                PlutusWitness
                    { script = WitnessReference catScriptOutputRef
                    , redeemerData = \_ -> dummyRedeemer
                    , requiredSigners = [ myKeyCred ]
                    }
            }
        ]
            |> finalize localStateUtxos []

You may have noticed that `redeemerData` is taking a function instead of just a redeemer.
This is to enable more advanced use cases such as [UTxO indexers][utxo-indexers].
But for simple use cases, we can just ignore that argument with an underscore `_`.

[utxo-indexers]: https://github.com/Anastasia-Labs/aiken-design-patterns

Ok now let’s show how sending to a Plutus script would be done.
As before, we’ll use the simple example of a lock script.
But this time, we don’t write it directly (as in the NativeScript example).
Instead we suppose the script was written in some onchain language (Aiken, Opshin, Plutus Tx, ...),
and the blueprint of the script is available, with its hash.

In the eUTxO model, UTxOs sent to a script address must have a piece of data attached.
That piece of data is referred to as the "datum".
It will be passed as argument to the script execution
when some future transaction try to spend that UTxO later.

    lockScriptHash =
        extractedFromBlueprint

    -- A script address is directly tied to the script hash
    -- and so indirectly also tied to the (immutable) script code.
    scriptAddress =
        Address.Shelley
            { networkId = Mainnet
            , paymentCredential = ScriptHash lockScriptHash
            , stakeCredential = Address.extractStakeCredential me
            }

    myKeyCred =
        Address.extractPubKeyHash me
            |> Maybe.withDefault dummyCredential

    -- Put the unlocking pubkey hash in the datum of the funds we lock
    datumWithKeyCred =
        Data.Bytes (Bytes.toAny myKeyCred)

    localStateUtxos =
        Utxo.refDictFromList (myUtxos ++ scriptsRefsUtxos)

    lockInPlutusScriptTx =
        [ Spend (FromWallet { address = me, value = fourAda, guaranteedUtxos = [] })
        , SendToOutput
            { address = scriptAddress
            , amount = fourAda
            , datumOption = Just (Datum datumWithKeyCred)
            , referenceScript = Nothing
            }
        ]
            |> finalize localStateUtxos []

Now that we know how to send values to a script, let’s see how to collect them.
We will show how to retrieve 2 ada from the previously locked 4 ada.
For that, we need to do a few things:

1.  Spend the whole UTxO, with its 4 ada in it.
    We cannot partially spend UTxOs.
    UTxOs are like bills, you spend them whole and get change for overspending.
2.  Provide the script code to the transaction.
    The script hash must match with the first part of the UTxO address we are spending.
3.  Provide our signature for the proof that the script needs.
4.  Retrieve 2 ada from that spent UTxO, and send 2 ada back to the same script.

For such a tiny script, which just checks if our signature is present,
no need to put it in a reference UTxO first.
We can embed it directly in the transaction witness.

    ( lockedUtxoRef, lockedOutput ) =
        -- TODO: Transaction.findOutputUtxosAt
        Transaction.findOutputUtxosAt scriptAddress lockInPlutusScriptTx
            |> List.head

    { updatedState } =
        Cardano.updateLocalState txId lockInPlutusScriptTx localStateUtxos

    lockScript =
        extractedFromTheBlueprint

    unlockFromPlutusScriptTx =
        -- Collect the locked UTxO at the script address
        [ Spend <|
            FromPlutusScript
                { spentInput = lockedUtxoRef
                , datumWitness = Nothing -- not needed, the datum was given by value
                , plutusScriptWitness =
                    { script = ( lockScript.version, WitnessValue lockScript.script )
                    , redeemerData = \_ -> dummyRedeemer -- unused
                    , requiredSigners = [ myKeyCred ]
                    }
                }
        , SendTo me twoAda

        -- Return the other 2 ada to the lock script (there was 4 ada initially)
        , SendToOutput
            { address = scriptAddress
            , amount = twoAda
            , datumOption = Just (Datum datumWithKeyCred)
            , referenceScript = Nothing
            }
        ]
            |> finalize updatedState []


## Code Documentation

@docs TxIntent, SpendSource, ScriptWitness, NativeScriptWitness, PlutusScriptWitness, WitnessSource
@docs CertificateIntent, CredentialWitness, VoterWitness
@docs VoteIntent, ProposalIntent, ActionProposal
@docs TxOtherInfo
@docs Fee
@docs finalize, finalizeAdvanced, TxFinalized, TxFinalizationError
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
import Cardano.Uplc as Uplc
import Cardano.Utils exposing (UnitInterval)
import Cardano.Utxo as Utxo exposing (Output, OutputReference, TransactionId)
import Cardano.Value as Value exposing (Value)
import Cbor.Encode as E
import Dict.Any exposing (AnyDict)
import Integer exposing (Integer)
import List.Extra
import Natural exposing (Natural)
import Set


{-| Represents different types of transaction intents.
-}
type TxIntent
    = SendTo Address Value
    | SendToOutput Output
    | SendToOutputAdvanced (TransactionBody -> Output)
      -- Spending assets from somewhere
    | Spend SpendSource
      -- Minting / burning assets
    | MintBurn
        { policyId : Bytes CredentialHash
        , assets : BytesMap AssetName Integer
        , scriptWitness : ScriptWitness
        }
      -- Issuing certificates
    | IssueCertificate CertificateIntent
      -- Withdrawing rewards
    | WithdrawRewards
        -- TODO: check that the addres type match the scriptWitness field
        { stakeCredential : StakeAddress
        , amount : Natural
        , scriptWitness : Maybe ScriptWitness
        }
    | Vote VoterWitness (List VoteIntent)
    | Propose ProposalIntent


{-| Represents different sources for spending assets.

TODO: check that output references match the type of source (script VS not script)

-}
type SpendSource
    = FromWallet
        { address : Address
        , value : Value
        , guaranteedUtxos : List OutputReference
        }
    | FromNativeScript
        { spentInput : OutputReference
        , nativeScriptWitness : WitnessSource NativeScript
        }
    | FromPlutusScript
        { spentInput : OutputReference
        , datumWitness : Maybe (WitnessSource Data)
        , plutusScriptWitness : PlutusScriptWitness
        }


{-| All intents requiring the on-chain publication of a certificate.

These include stake registration and delegation,
stake pool management, and voting or delegating your voting power.

-}
type CertificateIntent
    = RegisterStake { delegator : CredentialWitness, deposit : Natural }
    | UnregisterStake { delegator : CredentialWitness, refund : Natural }
    | DelegateStake { delegator : CredentialWitness, poolId : Bytes Pool.Id }
      -- Pool management
    | RegisterPool { deposit : Natural } Pool.Params
    | RetirePool { poolId : Bytes Pool.Id, epoch : Natural }
      -- Vote management
    | RegisterDrep { drep : CredentialWitness, deposit : Natural, info : Maybe Anchor }
    | UnregisterDrep { drep : CredentialWitness, refund : Natural }
    | VoteAlwaysAbstain { delegator : CredentialWitness }
    | VoteAlwaysNoConfidence { delegator : CredentialWitness }
    | DelegateVotes { delegator : CredentialWitness, drep : Credential }


{-| The type of credential to provide.

It can either be a key, typically from a wallet,
a native script, or a plutus script.

-}
type CredentialWitness
    = WithKey (Bytes CredentialHash)
    | WithScript (Bytes CredentialHash) ScriptWitness


credentialIsPlutusScript : CredentialWitness -> Bool
credentialIsPlutusScript cred =
    case cred of
        WithScript _ (PlutusWitness _) ->
            True

        _ ->
            False


{-| Voting credentials can either come from
a DRep, a stake pool, or Constitutional Committee member.
-}
type VoterWitness
    = WithCommitteeHotCred CredentialWitness
    | WithDrepCred CredentialWitness
    | WithPoolCred (Bytes CredentialHash)


{-| Represents different types of script witnesses.
-}
type ScriptWitness
    = NativeWitness NativeScriptWitness
    | PlutusWitness PlutusScriptWitness


{-| Represents a Native script witness.
Expected signatures are not put in the "required\_signers" field of the Tx
but are still used to estimate fees.
-}
type alias NativeScriptWitness =
    { script : WitnessSource NativeScript
    , expectedSigners : List (Bytes CredentialHash)
    }


{-| Represents a Plutus script witness.
-}
type alias PlutusScriptWitness =
    { script : ( Script.PlutusVersion, WitnessSource (Bytes ScriptCbor) )
    , redeemerData : TransactionBody -> Data
    , requiredSigners : List (Bytes CredentialHash)
    }


{-| Represents different sources for witnesses.
-}
type WitnessSource a
    = WitnessByValue a
    | WitnessByReference OutputReference


{-| Extract the [OutputReference] from a witness source,
if passed by reference. Return [Nothing] if passed by value.
-}
extractWitnessRef : WitnessSource a -> Maybe OutputReference
extractWitnessRef witnessSource =
    case witnessSource of
        WitnessByValue _ ->
            Nothing

        WitnessByReference ref ->
            Just ref


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
    | UnbalancedIntents String
    | InsufficientManualFee { declared : Natural, computed : Natural }
    | NotEnoughMinAda String
    | ReferenceOutputsMissingFromLocalState (List OutputReference)
    | FailedToPerformCoinSelection CoinSelection.Error
    | CollateralSelectionError CoinSelection.Error
    | DuplicatedMetadataTags Int
    | IncorrectTimeValidityRange String
    | UplcVmError String
    | GovProposalsNotSupportedInSimpleFinalize
    | FailurePleaseReportToElmCardano String


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
                NativeWitness _ ->
                    containPlutusScripts otherIntents

                PlutusWitness _ ->
                    True

        (IssueCertificate (RegisterStake { delegator })) :: otherIntents ->
            if credentialIsPlutusScript delegator then
                True

            else
                containPlutusScripts otherIntents

        (IssueCertificate (UnregisterStake { delegator })) :: otherIntents ->
            if credentialIsPlutusScript delegator then
                True

            else
                containPlutusScripts otherIntents

        (IssueCertificate (DelegateStake { delegator })) :: otherIntents ->
            if credentialIsPlutusScript delegator then
                True

            else
                containPlutusScripts otherIntents

        (IssueCertificate (RegisterPool _ _)) :: otherIntents ->
            containPlutusScripts otherIntents

        (IssueCertificate (RetirePool _)) :: otherIntents ->
            containPlutusScripts otherIntents

        (IssueCertificate (RegisterDrep { drep })) :: otherIntents ->
            if credentialIsPlutusScript drep then
                True

            else
                containPlutusScripts otherIntents

        (IssueCertificate (UnregisterDrep { drep })) :: otherIntents ->
            if credentialIsPlutusScript drep then
                True

            else
                containPlutusScripts otherIntents

        (IssueCertificate (VoteAlwaysAbstain { delegator })) :: otherIntents ->
            if credentialIsPlutusScript delegator then
                True

            else
                containPlutusScripts otherIntents

        (IssueCertificate (VoteAlwaysNoConfidence { delegator })) :: otherIntents ->
            if credentialIsPlutusScript delegator then
                True

            else
                containPlutusScripts otherIntents

        (IssueCertificate (DelegateVotes { delegator })) :: otherIntents ->
            if credentialIsPlutusScript delegator then
                True

            else
                containPlutusScripts otherIntents

        (WithdrawRewards { scriptWitness }) :: otherIntents ->
            case scriptWitness of
                Just (PlutusWitness _) ->
                    True

                _ ->
                    containPlutusScripts otherIntents

        (Vote voter _) :: otherIntents ->
            case voter of
                WithCommitteeHotCred (WithScript _ (PlutusWitness _)) ->
                    True

                WithDrepCred (WithScript _ (PlutusWitness _)) ->
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
            , scriptWitness : WitnessSource (Bytes ScriptCbor)
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
                buildTxRound : TransactionBody -> Fee -> Result TxFinalizationError TxFinalized
                buildTxRound roundTxBody roundFees =
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
                    in
                    -- UTxO selection
                    Result.map2
                        (\coinSelection collateralSelection ->
                            --> coinSelection : Address.Dict (Selection, List Output)
                            -- Accumulate all selected UTxOs and newly created outputs
                            accumPerAddressSelection coinSelection
                                --> { selectedInputs : Utxo.RefDict Ouptut, createdOutputs : List Output }
                                -- Aggregate with pre-selected inputs and pre-created outputs
                                |> (\selection -> updateTxBody processedIntents selection roundTxBody)
                                --> TransactionBody
                                |> buildTx localStateUtxos feeAmount collateralSelection processedIntents processedOtherInfo
                        )
                        (computeCoinSelection localStateUtxos roundFees processedIntents coinSelectionAlgo)
                        (computeCollateralSelection localStateUtxos collateralSources collateralAmount)

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
            buildTxRound Transaction.newBody fee
                --> Result String Transaction
                |> Result.andThen (\{ tx } -> buildTxRound tx.body (adjustFees tx))
                -- Evaluate plutus script cost
                |> Result.andThen (\{ tx } -> (adjustExecutionCosts <| evalScriptsCosts localStateUtxos) tx)
                -- Redo a final round of above
                |> Result.andThen (\tx -> buildTxRound tx.body (adjustFees tx))
                |> Result.andThen (\{ tx } -> (adjustExecutionCosts <| evalScriptsCosts localStateUtxos) tx)
                -- Redo a final round of above
                |> Result.andThen (\tx -> buildTxRound tx.body (adjustFees tx))
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
    , preSelected : List { input : OutputReference, redeemer : Maybe (TransactionBody -> Data) }
    , preCreated : TransactionBody -> { sum : Value, outputs : List Output }
    , nativeScriptSources : List (WitnessSource NativeScript)
    , plutusScriptSources : List ( PlutusVersion, WitnessSource (Bytes ScriptCbor) )
    , datumSources : List (WitnessSource Data)
    , expectedSigners : List (List (Bytes CredentialHash)) -- like requiredSigners, but not to put in the required_signers field of the Tx
    , requiredSigners : List (List (Bytes CredentialHash))
    , mints : List { policyId : Bytes CredentialHash, assets : BytesMap AssetName Integer, redeemer : Maybe (TransactionBody -> Data) }
    , withdrawals : List { stakeAddress : StakeAddress, amount : Natural, redeemer : Maybe (TransactionBody -> Data) }
    , certificates : List ( Certificate, Maybe (TransactionBody -> Data) )
    , proposalIntents : List ProposalIntent
    , votes : List { voter : Voter, votes : List VoteIntent, redeemer : Maybe (TransactionBody -> Data) }
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
                        newPreCreated txBody =
                            let
                                { sum, outputs } =
                                    preProcessedIntents.preCreated txBody
                            in
                            { sum = Value.add sum newOutput.amount
                            , outputs = newOutput :: outputs
                            }
                    in
                    { preProcessedIntents | preCreated = newPreCreated }

                SendToOutputAdvanced f ->
                    let
                        newPreCreated txBody =
                            let
                                { sum, outputs } =
                                    preProcessedIntents.preCreated txBody

                                newOutput =
                                    f txBody
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
                        , nativeScriptSources = nativeScriptWitness :: preProcessedIntents.nativeScriptSources
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
                        NativeWitness { script, expectedSigners } ->
                            { preProcessedIntents
                                | nativeScriptSources = script :: preProcessedIntents.nativeScriptSources
                                , expectedSigners = expectedSigners :: preProcessedIntents.expectedSigners
                                , mints = { policyId = policyId, assets = assets, redeemer = Nothing } :: preProcessedIntents.mints
                            }

                        PlutusWitness { script, redeemerData, requiredSigners } ->
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

                        Just (NativeWitness { script, expectedSigners }) ->
                            { preProcessedIntents
                                | withdrawals = { stakeAddress = stakeCredential, amount = amount, redeemer = Nothing } :: preProcessedIntents.withdrawals
                                , nativeScriptSources = script :: preProcessedIntents.nativeScriptSources
                                , expectedSigners = expectedSigners :: preProcessedIntents.expectedSigners
                            }

                        Just (PlutusWitness { script, redeemerData, requiredSigners }) ->
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

                Vote (WithCommitteeHotCred (WithKey cred)) votes ->
                    { preProcessedIntents
                        | votes = { voter = VoterCommitteeHotCred (VKeyHash cred), votes = votes, redeemer = Nothing } :: preProcessedIntents.votes
                    }

                Vote (WithCommitteeHotCred (WithScript cred (NativeWitness { script, expectedSigners }))) votes ->
                    { preProcessedIntents
                        | votes = { voter = VoterCommitteeHotCred (ScriptHash cred), votes = votes, redeemer = Nothing } :: preProcessedIntents.votes
                        , nativeScriptSources = script :: preProcessedIntents.nativeScriptSources
                        , expectedSigners = expectedSigners :: preProcessedIntents.expectedSigners
                    }

                Vote (WithCommitteeHotCred (WithScript cred (PlutusWitness { script, redeemerData, requiredSigners }))) votes ->
                    { preProcessedIntents
                        | votes = { voter = VoterCommitteeHotCred (ScriptHash cred), votes = votes, redeemer = Just redeemerData } :: preProcessedIntents.votes
                        , plutusScriptSources = script :: preProcessedIntents.plutusScriptSources
                        , requiredSigners = requiredSigners :: preProcessedIntents.requiredSigners
                    }

                Vote (WithDrepCred (WithKey cred)) votes ->
                    { preProcessedIntents
                        | votes = { voter = VoterDrepCred (VKeyHash cred), votes = votes, redeemer = Nothing } :: preProcessedIntents.votes
                    }

                Vote (WithDrepCred (WithScript cred (NativeWitness { script, expectedSigners }))) votes ->
                    { preProcessedIntents
                        | votes = { voter = VoterDrepCred (ScriptHash cred), votes = votes, redeemer = Nothing } :: preProcessedIntents.votes
                        , nativeScriptSources = script :: preProcessedIntents.nativeScriptSources
                        , expectedSigners = expectedSigners :: preProcessedIntents.expectedSigners
                    }

                Vote (WithDrepCred (WithScript cred (PlutusWitness { script, redeemerData, requiredSigners }))) votes ->
                    { preProcessedIntents
                        | votes = { voter = VoterDrepCred (ScriptHash cred), votes = votes, redeemer = Just redeemerData } :: preProcessedIntents.votes
                        , plutusScriptSources = script :: preProcessedIntents.plutusScriptSources
                        , requiredSigners = requiredSigners :: preProcessedIntents.requiredSigners
                    }

                Vote (WithPoolCred cred) votes ->
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
    -> CredentialWitness
    -> PreProcessedIntents
    -> PreProcessedIntents
preprocessCert certWithKeyF certWithScriptF { deposit, refund } cred preProcessedIntents =
    case cred of
        WithKey keyCred ->
            { preProcessedIntents
                | certificates = ( certWithKeyF keyCred, Nothing ) :: preProcessedIntents.certificates
                , totalDeposit = Natural.add deposit preProcessedIntents.totalDeposit
                , totalRefund = Natural.add refund preProcessedIntents.totalRefund
            }

        WithScript scriptHash (NativeWitness { script, expectedSigners }) ->
            { preProcessedIntents
                | certificates = ( certWithScriptF scriptHash, Nothing ) :: preProcessedIntents.certificates
                , nativeScriptSources = script :: preProcessedIntents.nativeScriptSources
                , expectedSigners = expectedSigners :: preProcessedIntents.expectedSigners
                , totalDeposit = Natural.add deposit preProcessedIntents.totalDeposit
                , totalRefund = Natural.add refund preProcessedIntents.totalRefund
            }

        WithScript scriptHash (PlutusWitness { script, redeemerData, requiredSigners }) ->
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
    , guaranteedUtxos : Address.Dict (List OutputReference)
    , preSelected : { sum : Value, inputs : Utxo.RefDict (Maybe (TransactionBody -> Data)) }
    , preCreated : TransactionBody -> { sum : Value, outputs : List Output }
    , nativeScriptSources : List (WitnessSource NativeScript)
    , plutusScriptSources : List ( PlutusVersion, WitnessSource (Bytes ScriptCbor) )
    , datumSources : List (WitnessSource Data)
    , expectedSigners : List (Bytes CredentialHash)
    , requiredSigners : List (Bytes CredentialHash)
    , totalMinted : MultiAsset Integer
    , mintRedeemers : BytesMap PolicyId (Maybe (TransactionBody -> Data))
    , withdrawals : Address.StakeDict { amount : Natural, redeemer : Maybe (TransactionBody -> Data) }
    , certificates : List ( Certificate, Maybe (TransactionBody -> Data) )
    , proposals : List ( ProposalProcedure, Maybe Data )
    , votes : Gov.VoterDict { votes : List VoteIntent, redeemer : Maybe (TransactionBody -> Data) }
    }


{-| Process already pre-processed intents and validate them all.
-}
processIntents : GovernanceState -> Utxo.RefDict Output -> List TxIntent -> Result TxFinalizationError ProcessedIntents
processIntents govState localStateUtxos txIntents =
    let
        preProcessedIntents =
            preProcessIntents txIntents

        -- Put all votes into a VoterDict.
        -- WARNING: if a voter is present multiple times, it will be overwritten.
        voterDict =
            preProcessedIntents.votes
                |> List.map (\{ voter, votes, redeemer } -> ( voter, { votes = votes, redeemer = redeemer } ))
                |> Gov.voterDictFromList

        -- Helper to check if a given proposal requires the guardrails script execution
        requiresGuardrails proposalIntent =
            case proposalIntent.govAction of
                ParameterChange _ ->
                    True

                TreasuryWithdrawals _ ->
                    True

                _ ->
                    False

        -- If there is any proposal requiring the guardrails script, update the plutus script sources
        plutusScriptSources =
            if List.any requiresGuardrails preProcessedIntents.proposalIntents then
                case govState.guardrailsScript of
                    Just { plutusVersion, scriptWitness } ->
                        ( plutusVersion, scriptWitness ) :: preProcessedIntents.plutusScriptSources

                    Nothing ->
                        preProcessedIntents.plutusScriptSources

            else
                preProcessedIntents.plutusScriptSources

        -- Accumulate all output references from inputs and witnesses.
        allOutputReferencesInIntents : Utxo.RefDict ()
        allOutputReferencesInIntents =
            List.concat
                [ List.map .input preProcessedIntents.preSelected
                , preProcessedIntents.guaranteedUtxos
                , List.filterMap extractWitnessRef preProcessedIntents.nativeScriptSources
                , List.map (\( _, source ) -> source) plutusScriptSources
                    |> List.filterMap extractWitnessRef
                , List.filterMap extractWitnessRef preProcessedIntents.datumSources
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
            \txBody ->
                let
                    { sum, outputs } =
                        preProcessedIntents.preCreated txBody
                in
                { sum = Value.add sum totalBurnedValue, outputs = outputs }

        preCreatedOutputs =
            preCreated Transaction.newBody

        -- Compute total inputs and outputs to check the Tx balance
        totalInput =
            Dict.Any.foldl (\_ -> Value.add) preSelected.sum preProcessedIntents.freeInputs
                |> Value.add (Value.onlyLovelace preProcessedIntents.totalRefund)

        totalOutput =
            Dict.Any.foldl (\_ -> Value.add) preCreatedOutputs.sum preProcessedIntents.freeOutputs
                |> Value.add (Value.onlyLovelace preProcessedIntents.totalDeposit)
    in
    if not <| Dict.Any.isEmpty absentOutputReferencesInLocalState then
        Err <| ReferenceOutputsMissingFromLocalState (Dict.Any.keys absentOutputReferencesInLocalState)

    else if totalInput /= totalOutput then
        let
            _ =
                Debug.log "totalInput" totalInput

            _ =
                Debug.log "totalOutput" totalOutput
        in
        Err <| UnbalancedIntents "Tx is not balanced.\n"

    else
        let
            totalMintedAndBurned : MultiAsset Integer
            totalMintedAndBurned =
                List.map (\m -> Map.singleton m.policyId m.assets) preProcessedIntents.mints
                    |> List.foldl MultiAsset.mintAdd MultiAsset.empty
                    |> MultiAsset.normalize Integer.isZero

            guaranteedUtxos : Address.Dict (List OutputReference)
            guaranteedUtxos =
                preProcessedIntents.guaranteedUtxos
                    |> List.foldl
                        (\ref acc ->
                            Dict.Any.get ref localStateUtxos
                                |> Maybe.map
                                    (\{ address } ->
                                        case Dict.Any.get address acc of
                                            Nothing ->
                                                Dict.Any.insert address [ ref ] acc

                                            Just refs ->
                                                Dict.Any.insert address (ref :: refs) acc
                                    )
                                |> Maybe.withDefault acc
                        )
                        Address.emptyDict
        in
        validMinAdaPerOutput preCreatedOutputs.outputs
            |> Result.mapError NotEnoughMinAda
            |> Result.map
                (\_ ->
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

                        witnessToHex encoder source =
                            encodeWitnessSource encoder source
                                |> E.encode
                                |> Bytes.fromBytes
                                |> Bytes.toHex
                    in
                    { freeInputs = preProcessedIntents.freeInputs
                    , freeOutputs = preProcessedIntents.freeOutputs
                    , guaranteedUtxos = guaranteedUtxos
                    , preSelected = preSelected
                    , preCreated = preCreated

                    -- TODO: an improvement would consist in fetching the referenced from the local state utxos,
                    -- and extract the script values, to even remove duplicates both in ref and values.
                    , nativeScriptSources = List.Extra.uniqueBy (witnessToHex Script.encodeNativeScript) preProcessedIntents.nativeScriptSources
                    , plutusScriptSources = List.Extra.uniqueBy (Tuple.second >> witnessToHex Bytes.toCbor) plutusScriptSources
                    , datumSources = List.Extra.uniqueBy (witnessToHex Data.toCbor) preProcessedIntents.datumSources
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
                    , votes = voterDict
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


encodeWitnessSource : (a -> E.Encoder) -> WitnessSource a -> E.Encoder
encodeWitnessSource encode witnessSource =
    case witnessSource of
        WitnessByValue a ->
            encode a

        WitnessByReference ref ->
            Utxo.encodeOutputReference ref


{-| Helper function
-}
addPreSelectedInput :
    OutputReference
    -> Value
    -> Maybe (TransactionBody -> Data)
    -> { sum : Value, inputs : Utxo.RefDict (Maybe (TransactionBody -> Data)) }
    -> { sum : Value, inputs : Utxo.RefDict (Maybe (TransactionBody -> Data)) }
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


{-| Perform collateral selection.

Only UTxOs at the provided whitelist of addresses are viable.
UTxOs are picked following a prioritization list.

  - First, prioritize UTxOs with only Ada in them,
    and with >= ? Ada, but lowest amounts prioritized over higher amounts.
  - Second, prioritize UTxOs with >= ? Ada, and that would cost minimal fees to add,
    so basically no reference script, no datums, and minimal number of assets.
  - Third, everything else, prioritized with >= ? Ada first,
    and sorted by minimal fee cost associated.
  - Finally, all the rest, sorted by "available" ada amounts (without min Ada),
    with bigger available amounts prioritized over smaller amounts.

-}
computeCollateralSelection :
    Utxo.RefDict Output
    -> Address.Dict ()
    -> Natural
    -> Result TxFinalizationError CoinSelection.Selection
computeCollateralSelection localStateUtxos collateralSources collateralAmount =
    let
        -- TODO: max inputs should come from a network parameter
        maxInputCount =
            3

        utxosInAllowedAddresses : List ( OutputReference, Output )
        utxosInAllowedAddresses =
            Dict.Any.toList localStateUtxos
                |> List.filter
                    (\( _, output ) -> Dict.Any.member output.address collateralSources)

        ( adaOnly, notAdaOnly ) =
            List.partition (\( _, output ) -> Utxo.isAdaOnly output)
                utxosInAllowedAddresses

        ( assetsOnly, notAssetsOnly ) =
            List.partition (\( _, output ) -> Utxo.isAssetsOnly output)
                notAdaOnly

        -- Some threshold to guarantee that after collateral is spent,
        -- there is still enough for an ada-only output (approximated at 1 ada)
        adaOnlyThreshold =
            Natural.add collateralAmount (Natural.fromSafeInt 1000000)

        -- Helper function to convert the lovelace amount in an output into
        -- a comparable value, safe from JS float overflow.
        -- By removing 5 decimals, we are guaranteed to have amounts
        -- lower than 450B (45B ada total supply), which is way below JS max safe integer around 2^53
        adaComparableAmount : Natural -> Float
        adaComparableAmount lovelace =
            lovelace
                |> Natural.divBy (Natural.fromSafeInt 100000)
                |> Maybe.withDefault Natural.zero
                |> Natural.toInt
                |> toFloat

        -- First, prioritize UTxOs with only Ada in them,
        -- and with >= ? Ada, but lowest amounts prioritized over higher amounts.
        ( highAdaOnly, lowAdaOnly ) =
            List.partition
                (\( _, { amount } ) -> amount.lovelace |> Natural.isGreaterThan adaOnlyThreshold)
                adaOnly

        highAdaOnlyCount =
            List.length highAdaOnly

        highAdaOnlySorted =
            List.sortBy (\( _, { amount } ) -> adaComparableAmount amount.lovelace) highAdaOnly

        availableUtxos =
            if highAdaOnlyCount >= maxInputCount then
                highAdaOnlySorted

            else
                -- Second, prioritize UTxOs with >= ? Ada, and that would cost minimal fees to add,
                -- so basically no reference script, no datums, and minimal number of assets.
                let
                    -- Add another ada for priority UTxOs with other tokens
                    assetOnlyThreshold =
                        Natural.add adaOnlyThreshold (Natural.fromSafeInt 1000000)

                    ( highAssetsOnly, lowAssetsOnly ) =
                        List.partition
                            (\( _, { amount } ) -> amount.lovelace |> Natural.isGreaterThan assetOnlyThreshold)
                            assetsOnly

                    highAssetsOnlyCount =
                        List.length highAssetsOnly

                    highAssetsOnlySorted =
                        List.sortBy (Tuple.second >> Utxo.bytesWidth) highAssetsOnly
                in
                if highAdaOnlyCount + highAssetsOnlyCount >= maxInputCount then
                    List.concat [ highAdaOnlySorted, highAssetsOnlySorted ]

                else
                    -- Third, everything else, prioritized with >= ? Ada first,
                    -- and sorted by minimal fee cost associated.
                    -- Finally, all the rest, sorted by "available" ada amounts (without min Ada),
                    -- with bigger available amounts prioritized over smaller amounts.
                    --
                    -- TODO: Improve, but honestly it’s very low priority,
                    -- so for now we just sort the rest by free ada (after removing min Ada).
                    let
                        freeAdaComparable : Output -> Float
                        freeAdaComparable output =
                            adaComparableAmount (Utxo.freeAda output)

                        allOtherUtxos =
                            List.concat [ lowAdaOnly, lowAssetsOnly, notAssetsOnly ]

                        allOtherUtxosSorted =
                            List.sortBy (Tuple.second >> freeAdaComparable) allOtherUtxos
                    in
                    List.concat [ highAdaOnlySorted, highAssetsOnlySorted, allOtherUtxosSorted ]
    in
    CoinSelection.inOrderedList maxInputCount
        { alreadySelectedUtxos = []
        , targetAmount = Value.onlyLovelace collateralAmount
        , availableUtxos = availableUtxos
        }
        |> Result.mapError CollateralSelectionError


{-| Perform coin selection for the required input per address.

For each address, create an [Output] with the change.
The output must satisfy minAda.

TODO: If there is more than 5 ada free in the change (after minAda),
also create a pure-ada output so that we don’t deplete all outputs viable for collateral.

-}
computeCoinSelection :
    Utxo.RefDict Output
    -> Fee
    -> ProcessedIntents
    -> CoinSelection.Algorithm
    -> Result TxFinalizationError (Address.Dict ( CoinSelection.Selection, List Output ))
computeCoinSelection localStateUtxos fee processedIntents coinSelectionAlgo =
    let
        dummyOutput =
            { address = Byron <| Bytes.fromHexUnchecked ""
            , amount = Value.zero
            , datumOption = Nothing
            , referenceScript = Nothing
            }

        -- Inputs not available for selection because already manually preselected
        notAvailableInputs =
            -- Using dummyOutput to have the same type as localStateUtxos
            Dict.Any.map (\_ _ -> dummyOutput) processedIntents.preSelected.inputs

        -- Precompute selectable inputs per addresses
        availableInputs : Address.Dict (List ( OutputReference, Output ))
        availableInputs =
            Dict.Any.diff localStateUtxos notAvailableInputs
                --> Utxo.RefDict Output
                |> Dict.Any.foldl
                    (\ref output ->
                        -- append the output to the list of outputs for the same address
                        Dict.Any.update output.address
                            (Just << (::) ( ref, output ) << Maybe.withDefault [])
                    )
                    Address.emptyDict

        -- TODO: adjust at least with the number of different tokens in target Amount
        maxInputCount =
            10

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
        -- It’s address dict keys are all different from those of freeInputsWithFee
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
        -- It’s address dict keys are a subset of those of freeInputsWithFee
        relatedFreeOutputValues : Address.Dict Value
        relatedFreeOutputValues =
            Dict.Any.diff processedIntents.freeOutputs independentFreeOutputValues

        -- Merge the two dicts :
        --   - freeInputsWithFee (that will become the coin selection target value)
        --   - relatedFreeOutputValues (that will be added to the coin selection change)
        targetValuesAndOutputs : Address.Dict { targetInputValue : Value, freeOutput : Value }
        targetValuesAndOutputs =
            let
                whenInput addr v =
                    Dict.Any.insert addr { targetInputValue = v, freeOutput = Value.zero }

                whenOutput addr v =
                    Dict.Any.insert addr { targetInputValue = Value.zero, freeOutput = v }

                whenBoth addr input output =
                    -- TODO: some optimization can be done here to reduce both sides
                    Dict.Any.insert addr { targetInputValue = input, freeOutput = output }
            in
            Dict.Any.merge whenInput
                whenBoth
                whenOutput
                freeInputsWithFee
                relatedFreeOutputValues
                Address.emptyDict

        -- Perform coin selection and output creation with the change
        -- for all address where there are target values (inputs and fees)
        -- TODO: do it instead per credential, not per address???
        coinSelectionAndChangeOutputs : Result TxFinalizationError (Address.Dict ( CoinSelection.Selection, List Output ))
        coinSelectionAndChangeOutputs =
            targetValuesAndOutputs
                -- Apply the selection algo for each address with input requirements
                |> Dict.Any.map
                    (\addr { targetInputValue, freeOutput } ->
                        let
                            hasFreeOutput =
                                freeOutput /= Value.zero

                            availableUtxosDict =
                                Maybe.withDefault [] (Dict.Any.get addr availableInputs)
                                    |> Utxo.refDictFromList

                            context targetAmount alreadySelected =
                                { targetAmount = targetAmount
                                , alreadySelectedUtxos = alreadySelected
                                , availableUtxos =
                                    Dict.Any.diff availableUtxosDict (Utxo.refDictFromList alreadySelected)
                                        |> Dict.Any.toList
                                }

                            -- Create the output(s) with the change + free output, if there is enough minAda
                            makeChangeOutput : CoinSelection.Selection -> Result CoinSelection.Error ( CoinSelection.Selection, List Output )
                            makeChangeOutput selection =
                                case ( selection.change, hasFreeOutput ) of
                                    ( Nothing, False ) ->
                                        Ok ( selection, [] )

                                    _ ->
                                        let
                                            change =
                                                Value.add (Maybe.withDefault Value.zero selection.change) freeOutput

                                            changeOutput =
                                                { address = addr
                                                , amount = change
                                                , datumOption = Nothing
                                                , referenceScript = Nothing
                                                }

                                            minAda =
                                                Utxo.minAda changeOutput
                                        in
                                        if change.lovelace |> Natural.isGreaterThanOrEqual minAda then
                                            -- TODO: later, if there is more than 5 free ada, make an additional ada-only output
                                            Ok ( selection, [ changeOutput ] )

                                        else
                                            Err <|
                                                CoinSelection.UTxOBalanceInsufficient
                                                    { selectedUtxos = selection.selectedUtxos
                                                    , missingValue = Value.onlyLovelace <| Natural.sub minAda change.lovelace
                                                    }

                            coinSelectIter targetValue alreadySelected =
                                coinSelectionAlgo maxInputCount (context targetValue alreadySelected)
                                    |> Result.andThen makeChangeOutput

                            guaranteedSelected =
                                Dict.Any.get addr processedIntents.guaranteedUtxos
                                    |> Maybe.withDefault []
                                    |> List.filterMap (\ref -> Dict.Any.get ref localStateUtxos |> Maybe.map (\output -> ( ref, output )))
                        in
                        -- Try coin selection up to 2 times if the only missing value is Ada.
                        -- Why 2 times? because the first time, it might be missing minAda for the change output.
                        case coinSelectIter targetInputValue guaranteedSelected of
                            (Err (CoinSelection.UTxOBalanceInsufficient err1)) as err ->
                                if MultiAsset.isEmpty err1.missingValue.assets then
                                    coinSelectIter (Value.add targetInputValue err1.missingValue) err1.selectedUtxos

                                else
                                    err

                            selectionResult ->
                                selectionResult
                    )
                -- Join the Dict (Result _ _) into Result _ Dict
                |> resultDictJoin
                |> Result.mapError FailedToPerformCoinSelection
    in
    Result.map2
        (Dict.Any.foldl (\addr output -> Dict.Any.insert addr ( { selectedUtxos = [], change = Nothing }, [ output ] )))
        coinSelectionAndChangeOutputs
        validIndependentFreeOutputs


{-| Helper function to join Dict Result into Result Dict.
-}
resultDictJoin : AnyDict comparable key (Result err value) -> Result err (AnyDict comparable key value)
resultDictJoin dict =
    Dict.Any.foldl (\key -> Result.map2 (Dict.Any.insert key)) (Ok <| Dict.Any.removeAll dict) dict


{-| Helper function to accumulate all selected UTxOs and newly created outputs.
-}
accumPerAddressSelection :
    Address.Dict ( CoinSelection.Selection, List Output )
    -> { selectedInputs : Utxo.RefDict Output, createdOutputs : List Output }
accumPerAddressSelection allSelections =
    Dict.Any.foldl
        (\_ ( { selectedUtxos }, createdOutputs ) acc ->
            { selectedInputs =
                List.foldl (\( ref, output ) -> Dict.Any.insert ref output) acc.selectedInputs selectedUtxos
            , createdOutputs = createdOutputs ++ acc.createdOutputs
            }
        )
        { selectedInputs = Utxo.emptyRefDict, createdOutputs = [] }
        allSelections


{-| Helper function to update Tx inputs/outputs after coin selection.
-}
updateTxBody : ProcessedIntents -> { selectedInputs : Utxo.RefDict Output, createdOutputs : List Output } -> TransactionBody -> TransactionBody
updateTxBody intents { selectedInputs, createdOutputs } old =
    -- reference inputs do not change with UTxO selection, only spent inputs
    { old
        | inputs =
            let
                preSelected : Utxo.RefDict ()
                preSelected =
                    Dict.Any.map (\_ _ -> ()) intents.preSelected.inputs

                algoSelected : Utxo.RefDict ()
                algoSelected =
                    Dict.Any.map (\_ _ -> ()) selectedInputs
            in
            Dict.Any.keys (Dict.Any.union preSelected algoSelected)
        , outputs = (intents.preCreated old).outputs ++ createdOutputs
    }


{-| Build the Transaction from the processed intents and the latest inputs/outputs.
-}
buildTx :
    Utxo.RefDict Output
    -> Natural
    -> CoinSelection.Selection
    -> ProcessedIntents
    -> ProcessedOtherInfo
    -> TransactionBody
    -> TxFinalized
buildTx localStateUtxos feeAmount collateralSelection processedIntents otherInfo txBody =
    let
        -- WitnessSet ######################################
        --
        ( nativeScripts, nativeScriptRefs ) =
            split witnessSourceToResult processedIntents.nativeScriptSources

        ( plutusScripts, plutusScriptRefs ) =
            splitScripts processedIntents.plutusScriptSources

        ( datumWitnessValues, datumWitnessRefs ) =
            split witnessSourceToResult processedIntents.datumSources

        -- Compute datums for pre-selected inputs.
        preSelected : Utxo.RefDict (Maybe Data)
        preSelected =
            processedIntents.preSelected.inputs
                |> Dict.Any.map (\_ -> Maybe.map (\f -> f txBody))

        -- Add a default Nothing to all inputs picked by the selection algorithm.
        algoSelected : Utxo.RefDict (Maybe Data)
        algoSelected =
            List.map (\ref -> ( ref, Nothing )) txBody.inputs
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
                            (\redeemerF -> makeRedeemer Redeemer.Mint id (redeemerF txBody))
                            maybeRedeemerF
                    )
                |> List.filterMap identity

        -- The StakeDict used for the withdrawals field
        -- uses the same ordering as Haskell (with script credentials first)
        sortedWithdrawals : List ( StakeAddress, Natural, Maybe Data )
        sortedWithdrawals =
            Dict.Any.toList processedIntents.withdrawals
                |> List.map (\( addr, w ) -> ( addr, w.amount, Maybe.map (\f -> f txBody) w.redeemer ))

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
                            (\redeemerF -> makeRedeemer Redeemer.Cert id (redeemerF txBody))
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
        sortedVotes : List ( Voter, { votes : List VoteIntent, redeemer : Maybe (TransactionBody -> Data) } )
        sortedVotes =
            Dict.Any.toList processedIntents.votes

        -- Build the Vote redeemer with the same order as txVotes
        voteRedeemers : List Redeemer
        voteRedeemers =
            sortedVotes
                |> List.indexedMap
                    (\id ( _, { redeemer } ) ->
                        Maybe.map
                            (\redeemerF -> makeRedeemer Redeemer.Vote id (redeemerF txBody))
                            redeemer
                    )
                |> List.filterMap identity

        -- Look for inputs at addresses that will need signatures
        walletCredsInInputs : List (Bytes CredentialHash)
        walletCredsInInputs =
            txBody.inputs
                |> List.filterMap
                    (\ref ->
                        Dict.Any.get ref localStateUtxos
                            |> Maybe.andThen (Address.extractPubKeyHash << .address)
                    )

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
                        [ sortedSpendRedeemers
                        , sortedMintRedeemers
                        , sortedWithdrawalsRedeemers
                        , certRedeemers
                        , proposalRedeemers
                        , voteRedeemers
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
        allReferenceInputs =
            List.concat
                [ txBody.referenceInputs
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
            { inputs = txBody.inputs
            , outputs = txBody.outputs
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


witnessSourceToResult : WitnessSource a -> Result a OutputReference
witnessSourceToResult witnessSource =
    case witnessSource of
        WitnessByValue value ->
            Err value

        WitnessByReference ref ->
            Ok ref


splitScripts : List ( PlutusVersion, WitnessSource (Bytes ScriptCbor) ) -> ( List ( PlutusVersion, Bytes ScriptCbor ), List OutputReference )
splitScripts scripts =
    split (\( v, source ) -> Result.mapError (Tuple.pair v) <| witnessSourceToResult source) scripts


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
