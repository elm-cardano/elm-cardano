module ElmCardano.Transaction.Builder exposing
    ( Tx
    , collateral
    , collateralReturn
    , complete
    , fee
    , input
    , inputData
    , new
    , output
    , payToAddress
    , payToContract
    , redeemer
    , referenceInput
    , requiredSigner
    , scriptDataHash
    , toString
    , totalCollateral
    )

import Bytes exposing (Bytes)
import BytesMap
import ElmCardano.Core exposing (Coin)
import ElmCardano.Data exposing (Data)
import ElmCardano.Hash exposing (Blake2b_224, Hash)
import ElmCardano.Redeemer exposing (Redeemer)
import ElmCardano.Transaction
    exposing
        ( Transaction
        , TransactionBody
        , WitnessSet
        , serialize
        )
import ElmCardano.Utxo exposing (DatumOption(..), Input, Output(..))
import ElmCardano.Value exposing (Value(..))


type Tx
    = Tx Transaction


new : Tx
new =
    Tx
        { body =
            { inputs = []
            , outputs = []
            , fee = Nothing
            , ttl = Nothing
            , certificates = []
            , withdrawals = BytesMap.empty
            , update = Nothing
            , auxiliaryDataHash = Nothing
            , validityIntervalStart = Nothing
            , mint = BytesMap.empty
            , scriptDataHash = Nothing
            , collateral = []
            , requiredSigners = []
            , networkId = Nothing
            , collateralReturn = Nothing
            , totalCollateral = Nothing
            , referenceInputs = []
            }
        , witnessSet =
            { vkeywitness = Nothing
            , nativeScripts = Nothing
            , bootstrapWitness = Nothing
            , plutusV1Script = Nothing
            , plutusData = Nothing
            , redeemer = Nothing
            , plutusV2Script = Nothing
            }
        , isValid = True
        , auxiliaryData = Nothing
        }


toString : Tx -> String
toString (Tx inner) =
    Debug.toString inner


updateBody : (TransactionBody -> TransactionBody) -> Transaction -> Tx
updateBody apply inner =
    Tx
        { inner
            | body = apply inner.body
        }


updateWitnessSet : (WitnessSet -> WitnessSet) -> Transaction -> Tx
updateWitnessSet apply inner =
    Tx
        { inner
            | witnessSet = apply inner.witnessSet
        }


input : Input -> Tx -> Tx
input newInput (Tx inner) =
    inner |> updateBody (addInput newInput)


inputData : Data -> Tx -> Tx
inputData data (Tx inner) =
    inner |> updateWitnessSet (addInputData data)


addInputData : Data -> WitnessSet -> WitnessSet
addInputData data witnessSet =
    { witnessSet
        | plutusData = prependMaybeList data witnessSet.plutusData
    }


redeemer : Redeemer -> Tx -> Tx
redeemer r (Tx inner) =
    inner |> updateWitnessSet (addRedeemer r)


addRedeemer : Redeemer -> WitnessSet -> WitnessSet
addRedeemer r witnessSet =
    { witnessSet
        | redeemer = prependMaybeList r witnessSet.redeemer
    }


addInput : Input -> TransactionBody -> TransactionBody
addInput newInput body =
    { body | inputs = newInput :: body.inputs }


payToContract : Bytes -> Coin -> Data -> Tx -> Tx
payToContract address amount datum tx =
    tx
        |> output
            (PostAlonzo
                { address = address
                , value = Coin amount
                , datumOption = Just (Datum datum)
                , referenceScript = Nothing
                }
            )


payToAddress : Bytes -> Coin -> Tx -> Tx
payToAddress address amount tx =
    tx
        |> output
            (Legacy
                { address = address
                , amount = Coin amount
                , datumHash = Nothing
                }
            )


output : Output -> Tx -> Tx
output newOutput (Tx inner) =
    inner |> updateBody (addOutput newOutput)


addOutput : Output -> TransactionBody -> TransactionBody
addOutput newOutput body =
    { body | outputs = body.outputs ++ [ newOutput ] }


fee : Int -> Tx -> Tx
fee amount (Tx inner) =
    inner |> updateBody (addFee amount)


addFee : Int -> TransactionBody -> TransactionBody
addFee amount body =
    { body | fee = Just amount }


scriptDataHash : Bytes -> Tx -> Tx
scriptDataHash dataHash (Tx inner) =
    inner |> updateBody (addScriptDataHash dataHash)


addScriptDataHash : Bytes -> TransactionBody -> TransactionBody
addScriptDataHash dataHash body =
    { body | scriptDataHash = Just dataHash }


collateral : Input -> Tx -> Tx
collateral newInput (Tx inner) =
    inner |> updateBody (addCollateral newInput)


addCollateral : Input -> TransactionBody -> TransactionBody
addCollateral newInput body =
    { body
        | collateral = newInput :: body.collateral
    }


requiredSigner : Hash Blake2b_224 -> Tx -> Tx
requiredSigner signer (Tx inner) =
    inner |> updateBody (addRequiredSigner signer)


addRequiredSigner : Hash Blake2b_224 -> TransactionBody -> TransactionBody
addRequiredSigner signer body =
    { body
        | requiredSigners = signer :: body.requiredSigners
    }


collateralReturn : Bytes -> Coin -> Tx -> Tx
collateralReturn address amount (Tx inner) =
    inner
        |> updateBody
            (addCollateralReturn
                (Legacy
                    { address = address
                    , amount = Coin amount
                    , datumHash = Nothing
                    }
                )
            )


addCollateralReturn : Output -> TransactionBody -> TransactionBody
addCollateralReturn return body =
    { body | collateralReturn = Just return }


totalCollateral : Int -> Tx -> Tx
totalCollateral amount (Tx inner) =
    inner |> updateBody (addTotalCollateral amount)


addTotalCollateral : Int -> TransactionBody -> TransactionBody
addTotalCollateral amount body =
    { body | totalCollateral = Just amount }


referenceInput : Input -> Tx -> Tx
referenceInput newInput (Tx inner) =
    inner |> updateBody (addReferenceInput newInput)


addReferenceInput : Input -> TransactionBody -> TransactionBody
addReferenceInput newInput body =
    { body
        | referenceInputs = newInput :: body.referenceInputs
    }


complete : Tx -> Bytes
complete (Tx inner) =
    serialize inner


prependMaybeList : a -> Maybe (List a) -> Maybe (List a)
prependMaybeList value list =
    list
        |> Maybe.withDefault []
        |> (::) value
        |> Just
