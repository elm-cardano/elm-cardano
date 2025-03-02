module Cardano.CoinSelection exposing
    ( Context, Error(..), Selection, Algorithm
    , largestFirst, inOrderedList
    )

{-| Module `Cardano.CoinSelection` provides functionality for performing
coin selection based on a set of available UTXOs and a set of requested outputs.
It exports functions for sorting UTXOs and performing the Largest-First coin
selection algorithm as described in CIP2 (<https://cips.cardano.org/cips/cip2/>).


# Types

@docs Context, Error, Selection, Algorithm


# Strategies

@docs largestFirst, inOrderedList

-}

import Bytes.Comparable exposing (Bytes)
import Cardano.MultiAsset as MultiAsset exposing (AssetName, PolicyId)
import Cardano.Utxo as Utxo exposing (Output, OutputReference)
import Cardano.Value as Value exposing (Value)
import Natural as N exposing (Natural)


{-| Enumerates the possible errors that can occur during coin selection.
-}
type Error
    = MaximumInputCountExceeded
    | UTxOBalanceInsufficient { selectedUtxos : List ( OutputReference, Output ), missingValue : Value }


{-| Represents the result of a successful coin selection.
-}
type alias Selection =
    { selectedUtxos : List ( OutputReference, Output )
    , change : Maybe Value
    }


{-| Holds the arguments necessary for performing coin selection.
-}
type alias Context =
    { availableUtxos : List ( OutputReference, Output )
    , alreadySelectedUtxos : List ( OutputReference, Output )
    , targetAmount : Value
    }


{-| Alias for the function signature of a utxo selection algorithm.
-}
type alias Algorithm =
    Int -> Context -> Result Error Selection


{-| Implements the simplest coin selection algorithm,
which just adds UTxOs until the target amount is reached.

Takes a `Context` record containing the available UTXOs, initially
selected UTXOs, requested outputs, and change address, along with an `Int`
representing the maximum number of inputs allowed. Returns either a
`Error` or a `Selection`.

Remark: the selected UTxOs are returned in reverse order for efficiency of list construction.

TODO: if possible, remove extraneous inputs.
Indeed, when selecting later CNT, they might contain enough previous CNT too.

-}
inOrderedList : Algorithm
inOrderedList maxInputCount context =
    let
        targetLovelace =
            Value.onlyLovelace context.targetAmount.lovelace

        -- Split targetAmount into individual tokens
        targetAssets : List ( Bytes PolicyId, Bytes AssetName, Natural )
        targetAssets =
            MultiAsset.split context.targetAmount.assets
    in
    -- Select for Ada first
    accumOutputsUntilDone
        { maxInputCount = maxInputCount
        , selectedInputCount = List.length context.alreadySelectedUtxos
        , accumulatedAmount = Value.sum (List.map (Tuple.second >> .amount) context.alreadySelectedUtxos)
        , targetAmount = targetLovelace
        , availableOutputs = context.availableUtxos
        , selectedOutputs = context.alreadySelectedUtxos
        }
        -- Then select for each token
        |> inOrderedListIter targetAssets
        |> Result.map
            (\state ->
                { selectedUtxos = state.selectedOutputs
                , change =
                    if state.accumulatedAmount == context.targetAmount then
                        Nothing

                    else
                        Just (Value.substract state.accumulatedAmount context.targetAmount |> Value.normalize)
                }
            )


{-| Apply in-order selection for each token successively.
-}
inOrderedListIter :
    List ( Bytes PolicyId, Bytes AssetName, Natural )
    -> Result Error SelectionState
    -> Result Error SelectionState
inOrderedListIter targets stateResult =
    case ( stateResult, targets ) of
        ( Err _, _ ) ->
            stateResult

        ( _, [] ) ->
            stateResult

        ( Ok state, ( policyId, name, amount ) :: others ) ->
            let
                newState =
                    { state | targetAmount = Value.onlyToken policyId name amount }
            in
            inOrderedListIter others (accumOutputsUntilDone newState)


{-| Implements the Largest-First coin selection algorithm as described in CIP2.

Takes a `Context` record containing the available UTXOs, initially
selected UTXOs, requested outputs, and change address, along with an `Int`
representing the maximum number of inputs allowed. Returns either a
`Error` or a `Selection`. See <https://cips.cardano.org/cips/cip2/#largestfirst>

TODO: if possible, remove extraneous inputs.
Indeed, when selecting later CNT, they might contain enough previous CNT too.

-}
largestFirst : Algorithm
largestFirst maxInputCount context =
    let
        targetLovelace =
            Value.onlyLovelace context.targetAmount.lovelace

        -- Split targetAmount into individual tokens
        targetAssets : List ( Bytes PolicyId, Bytes AssetName, Natural )
        targetAssets =
            MultiAsset.split context.targetAmount.assets

        sortedAvailableUtxoByLovelace =
            -- TODO: actually use the "free" lovelace, by substracting the UTxO minAda for sorting
            -- Create and use a function called "Utxo.compareFreeLovelace"
            List.sortWith (\( _, o1 ) ( _, o2 ) -> reverseOrder Utxo.compareLovelace o1 o2) context.availableUtxos
    in
    -- Select for Ada first
    accumOutputsUntilDone
        { maxInputCount = maxInputCount
        , selectedInputCount = List.length context.alreadySelectedUtxos
        , accumulatedAmount = Value.sum (List.map (Tuple.second >> .amount) context.alreadySelectedUtxos)
        , targetAmount = targetLovelace
        , availableOutputs = sortedAvailableUtxoByLovelace
        , selectedOutputs = context.alreadySelectedUtxos
        }
        -- Then select for each token
        |> largestFirstIter targetAssets
        |> Result.map
            (\state ->
                { selectedUtxos = state.selectedOutputs
                , change =
                    if state.accumulatedAmount == context.targetAmount then
                        Nothing

                    else
                        Just (Value.substract state.accumulatedAmount context.targetAmount |> Value.normalize)
                }
            )


type alias SelectionState =
    { maxInputCount : Int
    , selectedInputCount : Int
    , accumulatedAmount : Value
    , targetAmount : Value
    , availableOutputs : List ( OutputReference, Output )
    , selectedOutputs : List ( OutputReference, Output )
    }


{-| Apply largest-first selection for each token successively.
-}
largestFirstIter :
    List ( Bytes PolicyId, Bytes AssetName, Natural )
    -> Result Error SelectionState
    -> Result Error SelectionState
largestFirstIter targets stateResult =
    case ( stateResult, targets ) of
        ( Err _, _ ) ->
            stateResult

        ( _, [] ) ->
            stateResult

        ( Ok state, ( policyId, name, amount ) :: others ) ->
            let
                getToken value =
                    MultiAsset.get policyId name value.assets
                        |> Maybe.withDefault N.zero

                -- Sort UTxOs with largest amounts of the token first
                -- TODO: remark it’s a bit wasteful to sort if already satisfied
                -- but let’s leave that optimization for another time
                -- TODO: remark it’s also wasteful to sort all utxos
                -- instead of just the ones that contain the token, and append the others
                sortOrder ( _, o1 ) ( _, o2 ) =
                    reverseOrder (Value.compare getToken) o1.amount o2.amount

                newState =
                    { state
                        | targetAmount = Value.onlyToken policyId name amount
                        , availableOutputs = List.sortWith sortOrder state.availableOutputs
                    }
            in
            largestFirstIter others (accumOutputsUntilDone newState)


reverseOrder : (a -> a -> Order) -> a -> a -> Order
reverseOrder f x y =
    f y x


accumOutputsUntilDone : SelectionState -> Result Error SelectionState
accumOutputsUntilDone ({ maxInputCount, selectedInputCount, accumulatedAmount, targetAmount, availableOutputs, selectedOutputs } as state) =
    if selectedInputCount > maxInputCount then
        Err MaximumInputCountExceeded

    else if not (Value.atLeast targetAmount accumulatedAmount) then
        case availableOutputs of
            [] ->
                Err
                    (UTxOBalanceInsufficient
                        { selectedUtxos = selectedOutputs
                        , missingValue =
                            Value.substract targetAmount accumulatedAmount
                                |> Value.normalize
                        }
                    )

            utxo :: utxos ->
                accumOutputsUntilDone
                    { maxInputCount = maxInputCount
                    , selectedInputCount = selectedInputCount + 1
                    , accumulatedAmount = Value.add (Tuple.second utxo |> .amount) accumulatedAmount
                    , targetAmount = targetAmount
                    , availableOutputs = utxos
                    , selectedOutputs = utxo :: selectedOutputs
                    }

    else
        Ok state
