module Cardano.CoinSelection exposing
    ( Context, Error(..), Selection, Algorithm
    , largestFirst, inOrderedList
    , collateral
    )

{-| Module `Cardano.CoinSelection` provides functionality for performing
coin selection based on a set of available UTXOs and a set of requested outputs.
It exports functions for sorting UTXOs and performing the Largest-First coin
selection algorithm as described in CIP2 (<https://cips.cardano.org/cips/cip2/>).


# Types

@docs Context, Error, Selection, Algorithm


# Strategies

@docs largestFirst, inOrderedList


# Collateral

@docs collateral

-}

import Bytes.Comparable exposing (Bytes)
import Cardano.Address as Address
import Cardano.MultiAsset as MultiAsset exposing (AssetName, PolicyId)
import Cardano.Utxo as Utxo exposing (Output, OutputReference)
import Cardano.Value as Value exposing (Value)
import Dict.Any
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
                        Just (Value.subtract state.accumulatedAmount context.targetAmount |> Value.normalize)
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
                        Just (Value.subtract state.accumulatedAmount context.targetAmount |> Value.normalize)
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
                            Value.subtract targetAmount accumulatedAmount
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
collateral :
    Utxo.RefDict Output
    -> Address.Dict ()
    -> Natural
    -> Result Error Selection
collateral localStateUtxos collateralSources collateralAmount =
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
            N.add collateralAmount (N.fromSafeInt 1000000)

        -- Helper function to convert the lovelace amount in an output into
        -- a comparable value, safe from JS float overflow.
        -- By removing 5 decimals, we are guaranteed to have amounts
        -- lower than 450B (45B ada total supply), which is way below JS max safe integer around 2^53
        adaComparableAmount : Natural -> Float
        adaComparableAmount lovelace =
            lovelace
                |> N.divBy (N.fromSafeInt 100000)
                |> Maybe.withDefault N.zero
                |> N.toInt
                |> toFloat

        -- First, prioritize UTxOs with only Ada in them,
        -- and with >= ? Ada, but lowest amounts prioritized over higher amounts.
        ( highAdaOnly, lowAdaOnly ) =
            List.partition
                (\( _, { amount } ) -> amount.lovelace |> N.isGreaterThan adaOnlyThreshold)
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
                        N.add adaOnlyThreshold (N.fromSafeInt 1000000)

                    ( highAssetsOnly, lowAssetsOnly ) =
                        List.partition
                            (\( _, { amount } ) -> amount.lovelace |> N.isGreaterThan assetOnlyThreshold)
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
    inOrderedList maxInputCount
        { alreadySelectedUtxos = []
        , targetAmount = Value.onlyLovelace collateralAmount
        , availableUtxos = availableUtxos
        }
