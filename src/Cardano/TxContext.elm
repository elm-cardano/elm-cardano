module Cardano.TxContext exposing (TxContext, new, fromTx, updateInputsOutputs)

{-| Context available to the Tx builder to create redeemers and datums.
Very similar to the Plutus script context, but available offchain.

@docs TxContext, new, fromTx, updateInputsOutputs

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address exposing (CredentialHash, StakeAddress)
import Cardano.Gov exposing (ActionId, ProposalProcedure, Voter, VotingProcedure)
import Cardano.MultiAsset as MultiAsset exposing (MultiAsset)
import Cardano.Redeemer exposing (Redeemer)
import Cardano.Transaction exposing (Certificate, Transaction)
import Cardano.Utxo as Utxo exposing (DatumOption(..), Output, OutputReference)
import Cardano.Value exposing (Value)
import Dict.Any
import Integer exposing (Integer)
import Natural exposing (Natural)


{-| Some context available to the Tx builder to create redeemers and datums.

The contents of the `TxContext` are very similar to those of the Plutus script context.
This is because the goal is to help pre-compute offchain elements that will make
onchain code more efficient.

For example, you could pre-compute indexes of elements in inputs list,
or the redeemers list in the script context for faster onchain lookups.
For this reason, lists in this `TxContext` are ordered the same as in the Plutus script context.

-}
type alias TxContext =
    { fee : Natural
    , validityRange : { start : Maybe Int, end : Maybe Natural }
    , inputs : List ( OutputReference, Output )
    , referenceInputs : List ( OutputReference, Output )
    , outputs : List Output
    , mint : MultiAsset Integer
    , certificates : List Certificate
    , withdrawals : List ( StakeAddress, Natural )
    , votes : List ( Voter, List ( ActionId, VotingProcedure ) )
    , proposals : List ProposalProcedure
    , requiredSigners : List (Bytes CredentialHash)
    , redeemers : List Redeemer
    , currentTreasuryValue : Maybe Natural
    , treasuryDonation : Maybe Natural
    }


{-| Empty TxContext for initializations.
-}
new : TxContext
new =
    { fee = Natural.zero
    , validityRange = { start = Nothing, end = Nothing }
    , inputs = []
    , referenceInputs = []
    , outputs = []
    , mint = MultiAsset.empty
    , certificates = []
    , withdrawals = []
    , votes = []
    , proposals = []
    , requiredSigners = []
    , redeemers = []
    , currentTreasuryValue = Nothing
    , treasuryDonation = Nothing
    }


{-| Create a TxContext from a pre-existing transaction.

WARNING: currently, this function makes the assumption
that all `List` fields in the transaction are already sorted correctly.
This assumption is valid within the Tx builder.

-}
fromTx : Utxo.RefDict Output -> Transaction -> TxContext
fromTx utxos { body, witnessSet } =
    -- TODO: check order of things in the various list,
    -- and re-order if necessary, now that this function is public.
    let
        refsToUtxos refs =
            List.filterMap (\ref -> Dict.Any.get ref utxos |> Maybe.map (Tuple.pair ref)) refs
    in
    { fee = body.fee
    , validityRange = { start = body.validityIntervalStart, end = body.ttl }

    -- Inputs are already ordered in the updateTxContext function.
    -- Then order is kept in the buildTx function, generating the body in use here.
    , inputs = refsToUtxos body.inputs

    -- Reference inputs are sorted in the buildTx function.
    , referenceInputs = refsToUtxos body.referenceInputs
    , outputs = body.outputs
    , mint = body.mint
    , certificates = body.certificates

    -- Withdrawals are sorted in the buildTx function.
    , withdrawals = body.withdrawals

    -- Votes are sorted in the buildTx function.
    , votes = body.votingProcedures
    , proposals = body.proposalProcedures
    , requiredSigners = body.requiredSigners

    -- We order the redeemers in the witness set already in the buildTx function.
    , redeemers = witnessSet.redeemer |> Maybe.withDefault []
    , currentTreasuryValue = body.currentTreasuryValue
    , treasuryDonation = body.treasuryDonation
    }


{-| Helper function to update the TxContext inputs and outputs after coin selection.

Reference inputs do not change with UTxO selection, only spent inputs.
Inputs are sorted by output ref.

-}
updateInputsOutputs :
    { preSelectedInputs : Utxo.RefDict Output, preCreatedOutputs : TxContext -> { sum : Value, outputs : List Output } }
    -> { selectedUtxos : Utxo.RefDict Output, changeOutputs : List Output }
    -> TxContext
    -> TxContext
updateInputsOutputs { preSelectedInputs, preCreatedOutputs } { selectedUtxos, changeOutputs } old =
    { old
        | inputs = Dict.Any.toList (Dict.Any.union preSelectedInputs selectedUtxos)
        , outputs = (preCreatedOutputs old).outputs ++ changeOutputs
    }
