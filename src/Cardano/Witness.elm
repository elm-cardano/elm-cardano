module Cardano.Witness exposing
    ( Voter(..), toVoter, Credential(..), toCredential, Script(..), NativeScript, PlutusScript, Source(..), Error(..)
    , credentialIsPlutusScript, mapSource, toHex, sourceToResult, extractRef
    , checkDatum, checkScript, checkNativeScript, checkPlutusScript
    )

{-| Handling witnesses for Tx building intents.

@docs Voter, toVoter, Credential, toCredential, Script, NativeScript, PlutusScript, Source, Error

@docs credentialIsPlutusScript, mapSource, toHex, sourceToResult, extractRef

@docs checkDatum, checkScript, checkNativeScript, checkPlutusScript

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address as Address exposing (CredentialHash)
import Cardano.Data as Data exposing (Data)
import Cardano.Gov as Gov
import Cardano.Script as Script exposing (ScriptCbor)
import Cardano.TxContext exposing (TxContext)
import Cardano.Utxo as Utxo exposing (DatumOption(..), Output, OutputReference)
import Cbor.Encode as E
import Dict
import Dict.Any


{-| Voting credentials can either come from
a DRep, a stake pool, or Constitutional Committee member.
-}
type Voter
    = WithCommitteeHotCred Credential
    | WithDrepCred Credential
    | WithPoolCred (Bytes CredentialHash)


{-| Helper function to convert a voter to a Gov.Voter.
-}
toVoter : Voter -> Gov.Voter
toVoter voter =
    case voter of
        WithCommitteeHotCred cred ->
            Gov.VoterCommitteeHotCred <| toCredential cred

        WithDrepCred cred ->
            Gov.VoterDrepCred <| toCredential cred

        WithPoolCred hash ->
            Gov.VoterPoolId hash


{-| The type of credential to provide.

It can either be a key, typically from a wallet,
a native script, or a plutus script.

-}
type Credential
    = WithKey (Bytes CredentialHash)
    | WithScript (Bytes CredentialHash) Script


{-| Helper function to convert a credential witness to an Address.Credential.
-}
toCredential : Credential -> Address.Credential
toCredential cred =
    case cred of
        WithKey hash ->
            Address.VKeyHash hash

        WithScript hash _ ->
            Address.ScriptHash hash


{-| True if the this is a credential witness for a Plutus script.
-}
credentialIsPlutusScript : Credential -> Bool
credentialIsPlutusScript cred =
    case cred of
        WithScript _ (Plutus _) ->
            True

        _ ->
            False


{-| Represents different types of script witnesses.
-}
type Script
    = Native NativeScript
    | Plutus PlutusScript


{-| Represents a Native script witness.

Expected signatures are not put in the "required\_signers" field of the Tx
but are still used to estimate fees.

If you expect to sign with all credentials present in the multisig,
you can use `Dict.values (Cardano.Script.extractSigners script)`.

Otherwise, just list the credentials you intend to sign with.

-}
type alias NativeScript =
    { script : Source Script.NativeScript
    , expectedSigners : List (Bytes CredentialHash)
    }


{-| Represents a Plutus script witness.
-}
type alias PlutusScript =
    { script : ( Script.PlutusVersion, Source (Bytes ScriptCbor) )
    , redeemerData : TxContext -> Data
    , requiredSigners : List (Bytes CredentialHash)
    }


{-| Represents different sources for witnesses.
-}
type Source a
    = ByValue a
    | ByReference OutputReference


{-| Map a function over a witness source (if by value).
-}
mapSource : (a -> b) -> Source a -> Source b
mapSource f source =
    case source of
        ByValue value ->
            ByValue (f value)

        ByReference ref ->
            ByReference ref


{-| Extract the [OutputReference] from a witness source,
if passed by reference. Return [Nothing] if passed by value.
-}
extractRef : Source a -> Maybe OutputReference
extractRef source =
    case source of
        ByValue _ ->
            Nothing

        ByReference ref ->
            Just ref


{-| Encode a witness source into a unique Hex string representation.
-}
toHex : (a -> E.Encoder) -> Source a -> String
toHex encoder source =
    encodeSource encoder source
        |> E.encode
        |> Bytes.fromBytes
        |> Bytes.toHex


encodeSource : (a -> E.Encoder) -> Source a -> E.Encoder
encodeSource encode witnessSource =
    case witnessSource of
        ByValue a ->
            encode a

        ByReference ref ->
            Utxo.encodeOutputReference ref


{-| Transform a witness source into a Result type.

This isn’t to semantically say it can fail,
just to take advantage of all the functions operating on the Result type.
In Haskell, we would have converted to Either.

-}
sourceToResult : Source a -> Result a OutputReference
sourceToResult source =
    case source of
        ByValue value ->
            Err value

        ByReference ref ->
            Ok ref


{-| Error type describing all kind of errors that can happen while validating witnesses.
-}
type Error
    = InvalidExpectedSigners { scriptHash : Bytes CredentialHash } String
    | ScriptHashMismatch { expected : Bytes CredentialHash, witness : Bytes CredentialHash } String
    | ExtraneousDatum (Source Data) String
    | MissingDatum (Bytes Utxo.DatumHash) String
    | DatumHashMismatch { expected : Bytes Utxo.DatumHash, witness : Bytes Utxo.DatumHash } String
    | ReferenceOutputsMissingFromLocalState (List OutputReference)
    | MissingReferenceScript OutputReference
    | InvalidScriptRef OutputReference (Bytes Script.Script) String


{-| Check the witness of a script.
-}
checkScript : Utxo.RefDict Output -> Bytes CredentialHash -> Script -> Result Error ()
checkScript localStateUtxos expectedHash script =
    case script of
        Native nativeScript ->
            checkNativeScript localStateUtxos expectedHash nativeScript

        Plutus plutusScript ->
            checkPlutusScript localStateUtxos expectedHash plutusScript


{-| Check the witness of a native script. Both the script hash and the validity of expected signers.
-}
checkNativeScript : Utxo.RefDict Output -> Bytes CredentialHash -> NativeScript -> Result Error ()
checkNativeScript localStateUtxos expectedHash { script, expectedSigners } =
    let
        checkSigners nativeScript _ =
            checkExpectedSigners expectedSigners nativeScript
                |> Result.mapError (InvalidExpectedSigners { scriptHash = expectedHash })
    in
    case script of
        ByValue nativeScript ->
            checkScriptMatch { expected = expectedHash, witness = Script.hash <| Script.Native nativeScript }
                |> Result.andThen (checkSigners nativeScript)

        ByReference outputRef ->
            getRefScript localStateUtxos outputRef
                |> Result.andThen
                    (\scriptRef ->
                        case Script.refScript scriptRef of
                            Nothing ->
                                Err <| InvalidScriptRef outputRef (Script.refBytes scriptRef) "UTxO contains an invalid reference script (bytes cannot be decoded into an actual script)"

                            Just (Script.Plutus _) ->
                                Err <| InvalidScriptRef outputRef (Script.refBytes scriptRef) "UTxO reference contains a Plutus script instead of a native script"

                            Just (Script.Native nativeScript) ->
                                checkScriptMatch { expected = expectedHash, witness = Script.refHash scriptRef }
                                    |> Result.andThen (checkSigners nativeScript)
                    )


checkExpectedSigners : List (Bytes CredentialHash) -> Script.NativeScript -> Result String ()
checkExpectedSigners expected nativeScript =
    let
        expectedDict =
            Dict.fromList (List.map (\x -> ( Bytes.toHex x, x )) expected)

        signersInScript =
            Script.extractSigners nativeScript

        signersNotInScript =
            Dict.diff expectedDict signersInScript
    in
    if Dict.isEmpty signersNotInScript then
        if Script.isMultisigSatisfied expected nativeScript then
            Ok ()

        else
            Err "Native multisig not satisfied"

    else
        Err <|
            "These signers in the expected list, are not part of the multisig: "
                ++ String.join ", " (Dict.keys signersNotInScript)


{-| Check that the datum witness matches the output datum option.
-}
checkDatum : Utxo.RefDict Output -> Maybe DatumOption -> Maybe (Source Data) -> Result Error ()
checkDatum localStateUtxos maybeDatumOption maybeDatumWitness =
    case ( maybeDatumOption, maybeDatumWitness ) of
        ( Nothing, Nothing ) ->
            Ok ()

        ( Nothing, Just datumWitness ) ->
            Err <| ExtraneousDatum datumWitness "Datum witness was provided but the corresponding UTxO has no datum"

        ( Just (DatumValue _), Nothing ) ->
            Ok ()

        ( Just (DatumValue _), Just datumWitness ) ->
            Err <| ExtraneousDatum datumWitness "Datum witness was provided but the corresponding UTxO already has a datum provided by value"

        ( Just (DatumHash datumHash), Nothing ) ->
            Err <| MissingDatum datumHash "Datum in UTxO is a hash, but no witness was provided"

        ( Just (DatumHash datumHash), Just witness ) ->
            let
                checkDatumMatch hashes =
                    if hashes.expected == hashes.witness then
                        Ok ()

                    else
                        Err <| DatumHashMismatch hashes "Provided witness has wrong datum hash. Maybe you provided the wrong witness Data, or it is encoded differently than the original one."
            in
            case witness of
                ByValue data ->
                    checkDatumMatch { expected = datumHash, witness = Data.hash data }

                ByReference utxoRef ->
                    let
                        checkDatumOption datumOption =
                            case datumOption of
                                Nothing ->
                                    Err <| MissingDatum datumHash "The referenced UTxO presented as witness does not contain a datum"

                                Just (DatumHash _) ->
                                    Err <| MissingDatum datumHash "The referenced UTxO presented as witness contains a datum hash again instead of a datum value"

                                Just (DatumValue { rawBytes }) ->
                                    checkDatumMatch { expected = datumHash, witness = Data.rawDatumHash rawBytes }
                    in
                    getUtxo localStateUtxos utxoRef
                        |> Result.andThen (\output -> checkDatumOption output.datumOption)


{-| Check the validity of a Plutus script witness.
Witness sources are checked, script hashes are matched.
-}
checkPlutusScript : Utxo.RefDict Output -> Bytes CredentialHash -> PlutusScript -> Result Error ()
checkPlutusScript localStateUtxos expectedHash plutusScriptWitness =
    let
        ( version, witnessSource ) =
            plutusScriptWitness.script
    in
    case witnessSource of
        ByValue scriptBytes ->
            let
                computedScriptHash =
                    Script.plutusScriptFromBytes version scriptBytes
                        |> Script.Plutus
                        |> Script.hash
            in
            checkScriptMatch { expected = expectedHash, witness = computedScriptHash }

        ByReference outputRef ->
            let
                checkValidScript scriptRef =
                    case Script.refScript scriptRef of
                        Just _ ->
                            Ok scriptRef

                        Nothing ->
                            Err <| InvalidScriptRef outputRef (Script.refBytes scriptRef) "UTxO contains an invalid reference script (bytes cannot be decoded into an actual script)"
            in
            getRefScript localStateUtxos outputRef
                |> Result.andThen checkValidScript
                |> Result.andThen (\scriptRef -> checkScriptMatch { expected = expectedHash, witness = Script.refHash scriptRef })


checkScriptMatch : { expected : Bytes CredentialHash, witness : Bytes CredentialHash } -> Result Error ()
checkScriptMatch hashes =
    if hashes.expected == hashes.witness then
        Ok ()

    else
        Err <| ScriptHashMismatch hashes "Provided witness has wrong script hash"


getRefScript : Utxo.RefDict Output -> OutputReference -> Result Error Script.Reference
getRefScript localStateUtxos ref =
    getUtxo localStateUtxos ref
        |> Result.andThen (.referenceScript >> Result.fromMaybe (MissingReferenceScript ref))


getUtxo : Utxo.RefDict Output -> OutputReference -> Result Error Output
getUtxo utxos ref =
    Dict.Any.get ref utxos
        |> Result.fromMaybe (ReferenceOutputsMissingFromLocalState [ ref ])
