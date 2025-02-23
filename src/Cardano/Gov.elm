module Cardano.Gov exposing
    ( Id(..), idFromBech32, idToBech32
    , Drep(..), decodeDrep, encodeDrep
    , ProposalProcedure, proposalProcedureFromCbor
    , Action(..), decodeAction, encodeAction
    , ActionId, actionIdToString, actionIdFromCbor, encodeActionId
    , Constitution, decodeConstitution, encodeConstitution
    , ProtocolParamUpdate, noParamUpdate, decodeProtocolParamUpdate, encodeProtocolParamUpdate
    , PoolVotingThresholds, decodePoolVotingThresholds, encodePoolVotingThresholds
    , DrepVotingThresholds, decodeDrepVotingThresholds, encodeDrepVotingThresholds
    , CostModels, decodeCostModels, encodeCostModels
    , ProtocolVersion, decodeProtocolVersion, encodeProtocolVersion
    , Nonce(..)
    , VotingProcedure, votingProcedureFromCbor, encodeVotingProcedure
    , Vote(..), encodeVote
    , Voter(..), VoterDict, emptyVoterDict, voterDictFromList, voterCredentialHash, voterKeyCred, voterLedgerOrder, voterFromCbor, encodeVoter
    , Anchor, AnchorDataHash, decodeAnchor, encodeAnchor
    )

{-| Handling gov-related stuff.

@docs Id, idFromBech32, idToBech32

@docs Drep, decodeDrep, encodeDrep

@docs ProposalProcedure, proposalProcedureFromCbor

@docs Action, decodeAction, encodeAction

@docs ActionId, actionIdToString, actionIdFromCbor, encodeActionId

@docs Constitution, decodeConstitution, encodeConstitution

@docs ProtocolParamUpdate, noParamUpdate, decodeProtocolParamUpdate, encodeProtocolParamUpdate

@docs PoolVotingThresholds, decodePoolVotingThresholds, encodePoolVotingThresholds

@docs DrepVotingThresholds, decodeDrepVotingThresholds, encodeDrepVotingThresholds

@docs CostModels, decodeCostModels, encodeCostModels

@docs ProtocolVersion, decodeProtocolVersion, encodeProtocolVersion

@docs Nonce

@docs VotingProcedure, votingProcedureFromCbor, encodeVotingProcedure

@docs Vote, encodeVote

@docs Voter, VoterDict, emptyVoterDict, voterDictFromList, voterCredentialHash, voterKeyCred, voterLedgerOrder, voterFromCbor, encodeVoter

@docs Anchor, AnchorDataHash, decodeAnchor, encodeAnchor

-}

import Bech32.Decode as Bech32
import Bech32.Encode as Bech32
import Bytes.Comparable as Bytes exposing (Any, Bytes)
import Cardano.Address as Address exposing (Credential(..), CredentialHash, StakeAddress, extractCredentialHash)
import Cardano.MultiAsset exposing (PolicyId)
import Cardano.Pool as Pool
import Cardano.Redeemer as Redeemer exposing (ExUnitPrices, ExUnits, decodeExUnitPrices, encodeExUnitPrices)
import Cardano.Utils exposing (RationalNumber, UnitInterval, decodeRational, encodeRationalNumber)
import Cardano.Utxo exposing (TransactionId)
import Cbor.Decode as D
import Cbor.Decode.Extra as D
import Cbor.Encode as E
import Cbor.Encode.Extra as EE
import Dict.Any exposing (AnyDict)
import List.Extra
import Natural exposing (Natural)


{-| Governance Id related to [CIP 129](https://github.com/cardano-foundation/CIPs/blob/master/CIP-0129/README.md).
In addition to CC and DRep, this also includes SPOs as potential governance Ids!
-}
type Id
    = CcHotCredId Credential
    | CcColdCredId Credential
    | DrepId Credential
    | PoolId (Bytes Pool.Id)
    | GovActionId ActionId


{-| Convert a Gov Id from its Bech32 governance Id (CIP 129) or Pool Id (CIP 5).

Remark: will fail if a gov action index is >= 256 since we make the simplifying assumption it’s only 1 byte long.

-}
idFromBech32 : String -> Maybe Id
idFromBech32 str =
    case Pool.fromBech32 str of
        Just id ->
            Just <| PoolId id

        Nothing ->
            case Bech32.decode str of
                Err _ ->
                    Nothing

                Ok { prefix, data } ->
                    let
                        dataU8 =
                            Bytes.toU8 (Bytes.fromBytes data)
                    in
                    case ( prefix, dataU8 ) of
                        ( "cc_hot", 2 :: bytesU8 ) ->
                            Just <| CcHotCredId <| VKeyHash <| Bytes.fromU8 bytesU8

                        ( "cc_hot", 3 :: bytesU8 ) ->
                            Just <| CcHotCredId <| ScriptHash <| Bytes.fromU8 bytesU8

                        ( "cc_cold", 18 :: bytesU8 ) ->
                            Just <| CcColdCredId <| VKeyHash <| Bytes.fromU8 bytesU8

                        ( "cc_cold", 19 :: bytesU8 ) ->
                            Just <| CcColdCredId <| ScriptHash <| Bytes.fromU8 bytesU8

                        ( "drep", 34 :: bytesU8 ) ->
                            Just <| DrepId <| VKeyHash <| Bytes.fromU8 bytesU8

                        ( "drep", 35 :: bytesU8 ) ->
                            Just <| DrepId <| ScriptHash <| Bytes.fromU8 bytesU8

                        ( "gov_action", _ ) ->
                            case List.Extra.splitAt 32 dataU8 of
                                ( txIdU8, [ index ] ) ->
                                    Just <|
                                        GovActionId
                                            { transactionId = Bytes.fromU8 txIdU8
                                            , govActionIndex = index
                                            }

                                _ ->
                                    Nothing

                        _ ->
                            Nothing


{-| Convert a Gov Id from its Bech32 governance Id (CIP 129) or Pool Id (CIP 5).

Remark: will be wrong if a gov action index is >= 256 since we make the simplifying assumption it’s only 1 byte long.

-}
idToBech32 : Id -> String
idToBech32 id =
    case id of
        CcHotCredId (VKeyHash bytes) ->
            Bech32.encode { prefix = "cc_hot", data = Bytes.toBytes <| Bytes.fromU8 <| (2 :: Bytes.toU8 bytes) }
                |> Result.withDefault "cc_hot"

        CcHotCredId (ScriptHash bytes) ->
            Bech32.encode { prefix = "cc_hot", data = Bytes.toBytes <| Bytes.fromU8 <| (3 :: Bytes.toU8 bytes) }
                |> Result.withDefault "cc_hot"

        CcColdCredId (VKeyHash bytes) ->
            Bech32.encode { prefix = "cc_cold", data = Bytes.toBytes <| Bytes.fromU8 <| (18 :: Bytes.toU8 bytes) }
                |> Result.withDefault "cc_cold"

        CcColdCredId (ScriptHash bytes) ->
            Bech32.encode { prefix = "cc_cold", data = Bytes.toBytes <| Bytes.fromU8 <| (19 :: Bytes.toU8 bytes) }
                |> Result.withDefault "cc_cold"

        DrepId (VKeyHash bytes) ->
            Bech32.encode { prefix = "drep", data = Bytes.toBytes <| Bytes.fromU8 <| (34 :: Bytes.toU8 bytes) }
                |> Result.withDefault "drep"

        DrepId (ScriptHash bytes) ->
            Bech32.encode { prefix = "drep", data = Bytes.toBytes <| Bytes.fromU8 <| (35 :: Bytes.toU8 bytes) }
                |> Result.withDefault "drep"

        PoolId poolId ->
            Bech32.encode { prefix = "pool", data = Bytes.toBytes poolId }
                |> Result.withDefault "pool"

        GovActionId actionId ->
            Bech32.encode { prefix = "gov_action", data = Bytes.toBytes <| Bytes.fromU8 <| Bytes.toU8 actionId.transactionId ++ [ actionId.govActionIndex ] }
                |> Result.withDefault "gov_action"


{-| Delegate representative.
-}
type Drep
    = DrepCredential Credential -- 0, 1
    | AlwaysAbstain -- 2
    | AlwaysNoConfidence -- 3


{-| Represents different types of voters.
-}
type Voter
    = VoterCommitteeHotCred Credential -- 0, addr_keyhash // 1, scripthash
    | VoterDrepCred Credential -- 2, addr_keyhash // 3, scripthash
    | VoterPoolId (Bytes CredentialHash) -- 4, addr_keyhash


{-| Convenient alias for a `Dict` with [Voter] keys.
When converting to a `List`, its keys are sorted with the same order as the Haskell node.
The order is determined by [voterLedgerOrder].

WARNING: do not compare them with `==` since they contain functions.

-}
type alias VoterDict a =
    AnyDict ( Int, String ) Voter a


{-| Create a empty voter dictionary.
For other operations, use the `AnyDict` module directly.

WARNING: do not compare them with `==` since they contain functions.

-}
emptyVoterDict : VoterDict a
emptyVoterDict =
    Dict.Any.empty voterLedgerOrder


{-| Create a voter dictionary from a list.
For other operations, use the `AnyDict` module directly.

WARNING: do not compare them with `==` since they contain functions.

-}
voterDictFromList : List ( Voter, a ) -> VoterDict a
voterDictFromList voters =
    Dict.Any.fromList voterLedgerOrder voters


{-| Extract the credential hash of a voter.
-}
voterCredentialHash : Voter -> Bytes CredentialHash
voterCredentialHash voter =
    case voter of
        VoterCommitteeHotCred cred ->
            extractCredentialHash cred

        VoterDrepCred cred ->
            extractCredentialHash cred

        VoterPoolId hash ->
            hash


{-| Helper function to extract keys that would need signing.
-}
voterKeyCred : Voter -> Maybe (Bytes CredentialHash)
voterKeyCred voter =
    case voter of
        VoterCommitteeHotCred (VKeyHash hash) ->
            Just hash

        VoterDrepCred (VKeyHash hash) ->
            Just hash

        VoterPoolId hash ->
            Just hash

        _ ->
            Nothing


{-| Helper function to help sort voters for redeemers in the same order as Haskell node.

The problem is that the ledger code sort these maps with auto derived order.
And the Credential definition uses ScriptHash first, instead of VKeyHash first.
<https://github.com/IntersectMBO/cardano-ledger/blob/2f199b94716350b5fbd6c07505eb333d89cffa90/libs/cardano-ledger-core/src/Cardano/Ledger/Credential.hs#L85>

-}
voterLedgerOrder : Voter -> ( Int, String )
voterLedgerOrder voter =
    case voter of
        VoterCommitteeHotCred (ScriptHash hash) ->
            ( 0, Bytes.toHex hash )

        VoterCommitteeHotCred (VKeyHash hash) ->
            ( 1, Bytes.toHex hash )

        VoterDrepCred (ScriptHash hash) ->
            ( 2, Bytes.toHex hash )

        VoterDrepCred (VKeyHash hash) ->
            ( 3, Bytes.toHex hash )

        VoterPoolId hash ->
            ( 4, Bytes.toHex hash )


{-| Decoder for Voter type.
-}
voterFromCbor : D.Decoder Voter
voterFromCbor =
    D.length
        |> D.ignoreThen D.int
        |> D.andThen
            (\tag ->
                case tag of
                    0 ->
                        D.map (VoterCommitteeHotCred << Address.VKeyHash) (D.map Bytes.fromBytes D.bytes)

                    1 ->
                        D.map (VoterCommitteeHotCred << Address.ScriptHash) (D.map Bytes.fromBytes D.bytes)

                    2 ->
                        D.map (VoterDrepCred << Address.VKeyHash) (D.map Bytes.fromBytes D.bytes)

                    3 ->
                        D.map (VoterDrepCred << Address.ScriptHash) (D.map Bytes.fromBytes D.bytes)

                    4 ->
                        D.map VoterPoolId (D.map Bytes.fromBytes D.bytes)

                    _ ->
                        D.failWith ("Invalid voter tag: " ++ String.fromInt tag)
            )


{-| Represents different types of votes.
-}
type Vote
    = VoteNo -- 0
    | VoteYes -- 1
    | VoteAbstain -- 2


{-| Represents a voting procedure.
-}
type alias VotingProcedure =
    { vote : Vote
    , anchor : Maybe Anchor
    }


{-| Decoder for VotingProcedure type.
-}
votingProcedureFromCbor : D.Decoder VotingProcedure
votingProcedureFromCbor =
    D.tuple VotingProcedure <|
        D.elems
            >> D.elem
                (D.int
                    |> D.andThen
                        (\v ->
                            case v of
                                0 ->
                                    D.succeed VoteNo

                                1 ->
                                    D.succeed VoteYes

                                2 ->
                                    D.succeed VoteAbstain

                                _ ->
                                    D.failWith ("Invalid vote value: " ++ String.fromInt v)
                        )
                )
            >> D.elem (D.maybe decodeAnchor)


{-| Represents a proposal procedure.
-}
type alias ProposalProcedure =
    { deposit : Natural
    , depositReturnAccount : StakeAddress
    , govAction : Action
    , anchor : Anchor
    }


{-| Decoder for ProposalProcedure type.
-}
proposalProcedureFromCbor : D.Decoder ProposalProcedure
proposalProcedureFromCbor =
    D.tuple ProposalProcedure
        (D.elems
            >> D.elem D.natural
            >> D.elem Address.decodeReward
            >> D.elem decodeAction
            >> D.elem decodeAnchor
        )


{-| Represents different types of governance actions.
-}
type Action
    = ParameterChange
        --0
        { latestEnacted : Maybe ActionId
        , protocolParamUpdate : ProtocolParamUpdate
        , guardrailsPolicy : Maybe (Bytes PolicyId)
        }
      -- 1
    | HardForkInitiation
        { latestEnacted : Maybe ActionId
        , protocolVersion : ProtocolVersion
        }
      -- 2
    | TreasuryWithdrawals
        { withdrawals : List ( StakeAddress, Natural )
        , guardrailsPolicy : Maybe (Bytes PolicyId)
        }
      -- 3
    | NoConfidence { latestEnacted : Maybe ActionId }
      -- 4
    | UpdateCommittee
        { latestEnacted : Maybe ActionId
        , removedMembers : List Credential
        , addedMembers : List { newMember : Credential, expirationEpoch : Natural }
        , quorumThreshold : UnitInterval
        }
      -- 5
    | NewConstitution
        { latestEnacted : Maybe ActionId
        , constitution : Constitution
        }
      -- 6
    | Info


{-| Represents a constitution.
-}
type alias Constitution =
    { anchor : Anchor
    , scripthash : Maybe (Bytes CredentialHash)
    }


{-| Represents an action ID.
-}
type alias ActionId =
    { transactionId : Bytes TransactionId
    , govActionIndex : Int
    }


{-| Convert [ActionId] into its Hex string.
-}
actionIdToString : ActionId -> String
actionIdToString { transactionId, govActionIndex } =
    Bytes.toHex transactionId
        ++ "#"
        ++ String.fromInt govActionIndex


{-| Decoder for ActionId type.
-}
actionIdFromCbor : D.Decoder ActionId
actionIdFromCbor =
    D.tuple ActionId
        (D.elems
            >> D.elem (D.map Bytes.fromBytes D.bytes)
            >> D.elem D.int
        )


{-| Decoder for Anchor type.
-}
decodeAnchor : D.Decoder Anchor
decodeAnchor =
    D.tuple Anchor
        (D.elems
            >> D.elem D.string
            >> D.elem (D.map Bytes.fromBytes D.bytes)
        )


{-| Decoder for Action type.
-}
decodeAction : D.Decoder Action
decodeAction =
    D.length
        |> D.ignoreThen D.int
        |> D.andThen
            (\tag ->
                case tag of
                    0 ->
                        D.map3
                            (\govActionId update policy ->
                                ParameterChange
                                    { latestEnacted = govActionId
                                    , protocolParamUpdate = update
                                    , guardrailsPolicy = policy
                                    }
                            )
                            (D.maybe actionIdFromCbor)
                            decodeProtocolParamUpdate
                            (D.maybe (D.map Bytes.fromBytes D.bytes))

                    1 ->
                        D.map2
                            (\govActionId version ->
                                HardForkInitiation
                                    { latestEnacted = govActionId
                                    , protocolVersion = version
                                    }
                            )
                            (D.maybe actionIdFromCbor)
                            decodeProtocolVersion

                    2 ->
                        D.map2
                            (\withdrawals policy ->
                                TreasuryWithdrawals
                                    { withdrawals = withdrawals
                                    , guardrailsPolicy = policy
                                    }
                            )
                            (D.associativeList Address.decodeReward D.natural)
                            (D.maybe (D.map Bytes.fromBytes D.bytes))

                    3 ->
                        D.map
                            (\govActionId ->
                                NoConfidence { latestEnacted = govActionId }
                            )
                            (D.maybe actionIdFromCbor)

                    4 ->
                        D.map4
                            (\govActionId removed added threshold ->
                                UpdateCommittee
                                    { latestEnacted = govActionId
                                    , removedMembers = removed
                                    , addedMembers = added
                                    , quorumThreshold = threshold
                                    }
                            )
                            (D.maybe actionIdFromCbor)
                            (D.set Address.decodeCredential)
                            (D.associativeList Address.decodeCredential D.natural
                                |> D.map (List.map (\( member, epoch ) -> { newMember = member, expirationEpoch = epoch }))
                            )
                            decodeRational

                    5 ->
                        D.map2
                            (\govActionId constitution ->
                                NewConstitution
                                    { latestEnacted = govActionId
                                    , constitution = constitution
                                    }
                            )
                            (D.maybe actionIdFromCbor)
                            decodeConstitution

                    6 ->
                        D.succeed Info

                    _ ->
                        D.failWith ("Invalid action tag: " ++ String.fromInt tag)
            )


{-| Decoder for Constitution type.
-}
decodeConstitution : D.Decoder Constitution
decodeConstitution =
    D.tuple Constitution
        (D.elems
            >> D.elem decodeAnchor
            >> D.elem (D.maybe (D.map Bytes.fromBytes D.bytes))
        )


{-| Decoder for ProtocolParamUpdate type.
-}
decodeProtocolParamUpdate : D.Decoder ProtocolParamUpdate
decodeProtocolParamUpdate =
    -- TODO: Make it fail for an unknown field. Maybe use D.fold instead.
    D.record D.int ProtocolParamUpdate <|
        D.fields
            -- ? 0:  uint               ; minfee A
            >> D.optionalField 0 D.natural
            -- ? 1:  uint               ; minfee B
            >> D.optionalField 1 D.natural
            -- ? 2:  uint               ; max block body size
            >> D.optionalField 2 D.int
            -- ? 3:  uint               ; max transaction size
            >> D.optionalField 3 D.int
            -- ? 4:  uint               ; max block header size
            >> D.optionalField 4 D.int
            -- ? 5:  coin               ; key deposit
            >> D.optionalField 5 D.natural
            -- ? 6:  coin               ; pool deposit
            >> D.optionalField 6 D.natural
            -- ? 7: epoch               ; maximum epoch
            >> D.optionalField 7 D.natural
            -- ? 8: uint                ; n_opt: desired number of stake pools
            >> D.optionalField 8 D.int
            -- ? 9: rational            ; pool pledge influence
            >> D.optionalField 9 decodeRational
            -- ? 10: unit_interval      ; expansion rate
            >> D.optionalField 10 decodeRational
            -- ? 11: unit_interval      ; treasury growth rate
            >> D.optionalField 11 decodeRational
            -- ? 12: unit_interval      ; d. decentralization constant (deprecated)
            >> D.optionalField 12 decodeRational
            -- ? 13: $nonce             ; extra entropy (deprecated)
            >> D.optionalField 13 decodeExtraEntropy
            -- ? 14: [protocol_version] ; protocol version
            >> D.optionalField 14 decodeProtocolVersion
            -- ? 15: coin               ; min utxo value (deprecated)
            >> D.optionalField 15 D.natural
            -- ? 16: coin                ; min pool cost
            >> D.optionalField 16 D.natural
            -- ? 17: coin                ; ada per utxo byte
            >> D.optionalField 17 D.natural
            -- ? 18: costmdls            ; cost models for script languages
            >> D.optionalField 18 decodeCostModels
            -- ? 19: ex_unit_prices      ; execution costs
            >> D.optionalField 19 decodeExUnitPrices
            -- ? 20: ex_units            ; max tx ex units
            >> D.optionalField 20 Redeemer.exUnitsFromCbor
            -- ? 21: ex_units            ; max block ex units
            >> D.optionalField 21 Redeemer.exUnitsFromCbor
            -- ? 22: uint                ; max value size
            >> D.optionalField 22 D.int
            -- ? 23: uint                ; collateral percentage
            >> D.optionalField 23 D.int
            -- ? 24: uint                ; max collateral inputs
            >> D.optionalField 24 D.int
            -- Conway params
            -- poolVotingThresholds : Maybe PoolVotingThresholds -- 25
            >> D.optionalField 25 decodePoolVotingThresholds
            -- drepVotingThresholds : Maybe DrepVotingThresholds -- 26
            >> D.optionalField 26 decodeDrepVotingThresholds
            -- minCommitteeSize : Maybe Int -- 27
            >> D.optionalField 27 D.int
            -- committeeTermLimit : Maybe Natural -- 28
            >> D.optionalField 28 D.natural
            -- governanceActionValidityPeriod : Maybe Natural -- 29
            >> D.optionalField 29 D.natural
            -- governanceActionDeposit : Maybe Natural -- 30
            >> D.optionalField 30 D.natural
            -- drepDeposit : Maybe Natural -- 31
            >> D.optionalField 31 D.natural
            -- drepActivity : Maybe Natural -- 32
            >> D.optionalField 32 D.natural
            -- minFeeRefScriptCostPerByte : Maybe Int -- 33
            >> D.optionalField 33 D.int


{-| Decoder for PoolVotingThresholds type.
-}
decodePoolVotingThresholds : D.Decoder PoolVotingThresholds
decodePoolVotingThresholds =
    D.tuple PoolVotingThresholds
        (D.elems
            >> D.elem decodeRational
            >> D.elem decodeRational
            >> D.elem decodeRational
            >> D.elem decodeRational
            >> D.elem decodeRational
        )


{-| Decoder for DrepVotingThresholds type.
-}
decodeDrepVotingThresholds : D.Decoder DrepVotingThresholds
decodeDrepVotingThresholds =
    D.tuple DrepVotingThresholds
        (D.elems
            >> D.elem decodeRational
            >> D.elem decodeRational
            >> D.elem decodeRational
            >> D.elem decodeRational
            >> D.elem decodeRational
            >> D.elem decodeRational
            >> D.elem decodeRational
            >> D.elem decodeRational
            >> D.elem decodeRational
            >> D.elem decodeRational
        )


{-| Represents an anchor for governance actions.
-}
type alias Anchor =
    { url : String, dataHash : Bytes AnchorDataHash }


{-| Opaque phantom type for an [Anchor] data hash.
It is 32-bytes long.
-}
type AnchorDataHash
    = AnchorDataHash


{-| Adjustable parameters that power key aspects of the network.
-}
type alias ProtocolParamUpdate =
    { minFeeA : Maybe Natural -- 0
    , minFeeB : Maybe Natural -- 1
    , maxBlockBodySize : Maybe Int -- 2
    , maxTransactionSize : Maybe Int -- 3
    , maxBlockHeaderSize : Maybe Int -- 4
    , keyDeposit : Maybe Natural -- 5
    , poolDeposit : Maybe Natural -- 6
    , maximumEpoch : Maybe Natural -- 7
    , desiredNumberOfStakePools : Maybe Int -- 8
    , poolPledgeInfluence : Maybe RationalNumber -- 9
    , expansionRate : Maybe UnitInterval -- 10
    , treasuryGrowthRate : Maybe UnitInterval -- 11
    , decentralizationConstant : Maybe UnitInterval -- 12 (deprecated)
    , extraEntropy : Maybe Nonce -- 13 (deprecated)
    , protocolVersion : Maybe ProtocolVersion -- 14 (deprecated)
    , minUtxoValue : Maybe Natural -- 15 (deprecated)
    , minPoolCost : Maybe Natural -- 16
    , adaPerUtxoByte : Maybe Natural -- 17
    , costModelsForScriptLanguages : Maybe CostModels -- 18
    , executionCosts : Maybe ExUnitPrices -- 19
    , maxTxExUnits : Maybe ExUnits -- 20
    , maxBlockExUnits : Maybe ExUnits -- 21
    , maxValueSize : Maybe Int -- 22
    , collateralPercentage : Maybe Int -- 23
    , maxCollateralInputs : Maybe Int -- 24
    , poolVotingThresholds : Maybe PoolVotingThresholds -- 25
    , drepVotingThresholds : Maybe DrepVotingThresholds -- 26
    , minCommitteeSize : Maybe Int -- 27
    , committeeTermLimit : Maybe Natural -- 28
    , governanceActionValidityPeriod : Maybe Natural -- 29
    , governanceActionDeposit : Maybe Natural -- 30
    , drepDeposit : Maybe Natural -- 31
    , drepInactivityPeriod : Maybe Natural -- 32
    , minFeeRefScriptCostPerByte : Maybe Int -- 33
    }


{-| Default (no update) for [ProtocolParamUpdate].
-}
noParamUpdate : ProtocolParamUpdate
noParamUpdate =
    { minFeeA = Nothing -- 0
    , minFeeB = Nothing -- 1
    , maxBlockBodySize = Nothing -- 2
    , maxTransactionSize = Nothing -- 3
    , maxBlockHeaderSize = Nothing -- 4
    , keyDeposit = Nothing -- 5
    , poolDeposit = Nothing -- 6
    , maximumEpoch = Nothing -- 7
    , desiredNumberOfStakePools = Nothing -- 8
    , poolPledgeInfluence = Nothing -- 9
    , expansionRate = Nothing -- 10
    , treasuryGrowthRate = Nothing -- 11
    , decentralizationConstant = Nothing -- 12 (deprecated)
    , extraEntropy = Nothing -- 13 (deprecated)
    , protocolVersion = Nothing -- 14 (deprecated)
    , minUtxoValue = Nothing -- 15 (deprecated)
    , minPoolCost = Nothing -- 16
    , adaPerUtxoByte = Nothing -- 17
    , costModelsForScriptLanguages = Nothing -- 18
    , executionCosts = Nothing -- 19
    , maxTxExUnits = Nothing -- 20
    , maxBlockExUnits = Nothing -- 21
    , maxValueSize = Nothing -- 22
    , collateralPercentage = Nothing -- 23
    , maxCollateralInputs = Nothing -- 24
    , poolVotingThresholds = Nothing -- 25
    , drepVotingThresholds = Nothing -- 26
    , minCommitteeSize = Nothing -- 27
    , committeeTermLimit = Nothing -- 28
    , governanceActionValidityPeriod = Nothing -- 29
    , governanceActionDeposit = Nothing -- 30
    , drepDeposit = Nothing -- 31
    , drepInactivityPeriod = Nothing -- 32
    , minFeeRefScriptCostPerByte = Nothing -- 33
    }


{-| Represents voting thresholds for stake pools.
-}
type alias PoolVotingThresholds =
    { motionNoConfidence : UnitInterval
    , committeeNormal : UnitInterval
    , committeeNoConfidence : UnitInterval
    , hardforkInitiation : UnitInterval
    , securityRelevantParameter : UnitInterval
    }


{-| Represents voting thresholds for delegate representatives.
-}
type alias DrepVotingThresholds =
    { motionNoConfidence : UnitInterval
    , committeeNormal : UnitInterval
    , committeeNoConfidence : UnitInterval
    , updateConstitution : UnitInterval
    , hardforkInitiation : UnitInterval
    , ppNetworkGroup : UnitInterval
    , ppEconomicGroup : UnitInterval
    , ppTechnicalGroup : UnitInterval
    , ppGovernanceGroup : UnitInterval
    , treasuryWithdrawal : UnitInterval
    }


{-| Represents cost models for different Plutus versions.
-}
type alias CostModels =
    { plutusV1 : Maybe (List Int) -- 0
    , plutusV2 : Maybe (List Int) -- 1
    , plutusV3 : Maybe (List Int) -- 2
    }


{-| Represents a nonce for extra entropy.
-}
type Nonce
    = Just0
    | RandomBytes (Bytes Any)


{-| Represents a protocol version.
-}
type alias ProtocolVersion =
    ( Int, Int )



-- https://github.com/txpipe/pallas/blob/d1ac0561427a1d6d1da05f7b4ea21414f139201e/pallas-primitives/src/alonzo/model.rs#L379
-- ENCODERS


{-| Encode [CostModels] to CBOR.
-}
encodeCostModels : CostModels -> E.Encoder
encodeCostModels =
    E.record E.int <|
        E.fields
            >> E.optionalField 0 (E.list E.int) .plutusV1
            >> E.optionalField 1 (E.list E.int) .plutusV2
            >> E.optionalField 2 (E.list E.int) .plutusV3


{-| Encoder for Drep type.
-}
encodeDrep : Drep -> E.Encoder
encodeDrep drep =
    case drep of
        DrepCredential cred ->
            Address.credentialToCbor cred

        AlwaysAbstain ->
            E.int 2

        AlwaysNoConfidence ->
            E.int 3


{-| Encoder for Anchor type.
-}
encodeAnchor : Anchor -> E.Encoder
encodeAnchor =
    E.tuple
        (E.elems
            >> E.elem E.string .url
            >> E.elem Bytes.toCbor .dataHash
        )


{-| Encoder for PoolVotingThresholds type.
-}
encodePoolVotingThresholds : PoolVotingThresholds -> E.Encoder
encodePoolVotingThresholds thresholds =
    E.list encodeRationalNumber
        [ thresholds.motionNoConfidence
        , thresholds.committeeNormal
        , thresholds.committeeNoConfidence
        , thresholds.hardforkInitiation
        , thresholds.securityRelevantParameter
        ]


{-| Encoder for DrepVotingThresholds type.
-}
encodeDrepVotingThresholds : DrepVotingThresholds -> E.Encoder
encodeDrepVotingThresholds thresholds =
    E.list encodeRationalNumber
        [ thresholds.motionNoConfidence
        , thresholds.committeeNormal
        , thresholds.committeeNoConfidence
        , thresholds.updateConstitution
        , thresholds.hardforkInitiation
        , thresholds.ppNetworkGroup
        , thresholds.ppEconomicGroup
        , thresholds.ppTechnicalGroup
        , thresholds.ppGovernanceGroup
        , thresholds.treasuryWithdrawal
        ]


{-| Encoder for Action type.
-}
encodeAction : Action -> E.Encoder
encodeAction action =
    case action of
        ParameterChange { latestEnacted, protocolParamUpdate, guardrailsPolicy } ->
            E.list identity
                [ E.int 0
                , E.maybe encodeActionId latestEnacted
                , encodeProtocolParamUpdate protocolParamUpdate
                , E.maybe Bytes.toCbor guardrailsPolicy
                ]

        HardForkInitiation { latestEnacted, protocolVersion } ->
            E.list identity
                [ E.int 1
                , E.maybe encodeActionId latestEnacted
                , encodeProtocolVersion protocolVersion
                ]

        TreasuryWithdrawals { withdrawals, guardrailsPolicy } ->
            E.list identity
                [ E.int 2
                , EE.associativeList Address.stakeAddressToCbor EE.natural withdrawals
                , E.maybe Bytes.toCbor guardrailsPolicy
                ]

        NoConfidence { latestEnacted } ->
            E.list identity
                [ E.int 3
                , E.maybe encodeActionId latestEnacted
                ]

        UpdateCommittee { latestEnacted, removedMembers, addedMembers, quorumThreshold } ->
            E.list identity
                [ E.int 4
                , E.maybe encodeActionId latestEnacted
                , E.list Address.credentialToCbor removedMembers
                , EE.associativeList Address.credentialToCbor EE.natural (List.map (\m -> ( m.newMember, m.expirationEpoch )) addedMembers)
                , encodeRationalNumber quorumThreshold
                ]

        NewConstitution { latestEnacted, constitution } ->
            E.list identity
                [ E.int 5
                , E.maybe encodeActionId latestEnacted
                , encodeConstitution constitution
                ]

        Info ->
            E.list E.int [ 6 ]


{-| Encoder for ActionId type.
-}
encodeActionId : ActionId -> E.Encoder
encodeActionId =
    E.tuple
        (E.elems
            >> E.elem Bytes.toCbor .transactionId
            >> E.elem E.int .govActionIndex
        )


{-| Encoder for Constitution type.
-}
encodeConstitution : Constitution -> E.Encoder
encodeConstitution =
    E.tuple
        (E.elems
            >> E.elem encodeAnchor .anchor
            >> E.elem (E.maybe Bytes.toCbor) .scripthash
        )


{-| Encoder for ProtocolVersion type.
-}
encodeProtocolVersion : ProtocolVersion -> E.Encoder
encodeProtocolVersion ( major, minor ) =
    E.list E.int [ major, minor ]


{-| Encoder for ProtocolParamUpdate type.
-}
encodeProtocolParamUpdate : ProtocolParamUpdate -> E.Encoder
encodeProtocolParamUpdate =
    E.record E.int <|
        E.fields
            >> E.optionalField 0 EE.natural .minFeeA
            >> E.optionalField 1 EE.natural .minFeeB
            >> E.optionalField 2 E.int .maxBlockBodySize
            >> E.optionalField 3 E.int .maxTransactionSize
            >> E.optionalField 4 E.int .maxBlockHeaderSize
            >> E.optionalField 5 EE.natural .keyDeposit
            >> E.optionalField 6 EE.natural .poolDeposit
            >> E.optionalField 7 EE.natural .maximumEpoch
            >> E.optionalField 8 E.int .desiredNumberOfStakePools
            >> E.optionalField 9 encodeRationalNumber .poolPledgeInfluence
            >> E.optionalField 10 encodeRationalNumber .expansionRate
            >> E.optionalField 11 encodeRationalNumber .treasuryGrowthRate
            >> E.optionalField 14 (\( v, m ) -> E.list E.int [ v, m ]) .protocolVersion
            >> E.optionalField 16 EE.natural .minPoolCost
            >> E.optionalField 17 EE.natural .adaPerUtxoByte
            >> E.optionalField 18 encodeCostModels .costModelsForScriptLanguages
            >> E.optionalField 19 encodeExUnitPrices .executionCosts
            >> E.optionalField 20 Redeemer.encodeExUnits .maxTxExUnits
            >> E.optionalField 21 Redeemer.encodeExUnits .maxBlockExUnits
            >> E.optionalField 22 E.int .maxValueSize
            >> E.optionalField 23 E.int .collateralPercentage
            >> E.optionalField 24 E.int .maxCollateralInputs
            -- Conway fields
            >> E.optionalField 25 encodePoolVotingThresholds .poolVotingThresholds
            >> E.optionalField 26 encodeDrepVotingThresholds .drepVotingThresholds
            >> E.optionalField 27 E.int .minCommitteeSize
            >> E.optionalField 28 EE.natural .committeeTermLimit
            >> E.optionalField 29 EE.natural .governanceActionValidityPeriod
            >> E.optionalField 30 EE.natural .governanceActionDeposit
            >> E.optionalField 31 EE.natural .drepDeposit
            >> E.optionalField 32 EE.natural .drepInactivityPeriod
            >> E.optionalField 33 E.int .minFeeRefScriptCostPerByte


{-| Encoder for Voter type.
-}
encodeVoter : Voter -> E.Encoder
encodeVoter voter =
    case voter of
        VoterCommitteeHotCred cred ->
            case cred of
                Address.VKeyHash hash ->
                    E.list identity [ E.int 0, Bytes.toCbor hash ]

                Address.ScriptHash hash ->
                    E.list identity [ E.int 1, Bytes.toCbor hash ]

        VoterDrepCred cred ->
            case cred of
                Address.VKeyHash hash ->
                    E.list identity [ E.int 2, Bytes.toCbor hash ]

                Address.ScriptHash hash ->
                    E.list identity [ E.int 3, Bytes.toCbor hash ]

        VoterPoolId poolId ->
            E.list identity [ E.int 4, Bytes.toCbor poolId ]


{-| Encoder for VotingProcedure type.
-}
encodeVotingProcedure : VotingProcedure -> E.Encoder
encodeVotingProcedure =
    E.tuple
        (E.elems
            >> E.elem encodeVote .vote
            >> E.elem (E.maybe encodeAnchor) .anchor
        )


{-| Encoder for Vote type.
-}
encodeVote : Vote -> E.Encoder
encodeVote vote =
    case vote of
        VoteNo ->
            E.int 0

        VoteYes ->
            E.int 1

        VoteAbstain ->
            E.int 2



-- DECODERS


{-| Decoder for CostModels type.
-}
decodeCostModels : D.Decoder CostModels
decodeCostModels =
    -- TODO: Make it fail for an unknown field. Maybe use D.fold instead.
    D.record D.int (\v1costs v2costs v3costs -> { plutusV1 = v1costs, plutusV2 = v2costs, plutusV3 = v3costs }) <|
        D.fields
            -- plutusV1
            >> D.optionalField 0 (D.list D.int)
            -- plutusV2
            >> D.optionalField 1 (D.list D.int)
            -- plutusV3
            >> D.optionalField 2 (D.list D.int)


{-| Decoder for Nonce type.
-}
decodeExtraEntropy : D.Decoder Nonce
decodeExtraEntropy =
    D.length
        |> D.andThen
            (\l ->
                -- $nonce /= [ 0 // 1, bytes .size 32 ]
                case l of
                    1 ->
                        -- Remark: we don’t check that the value is 0
                        -- We just assume its correct and do not validate.
                        D.map (always Just0) D.int

                    2 ->
                        -- Remark: we don’t check that the value is 1
                        -- We just assume its correct and do not validate.
                        D.int |> D.ignoreThen (D.map (RandomBytes << Bytes.fromBytes) D.bytes)

                    _ ->
                        D.fail
            )


{-| Decoder for ProtocolVersion type.
-}
decodeProtocolVersion : D.Decoder ProtocolVersion
decodeProtocolVersion =
    D.tuple Tuple.pair <|
        D.elems
            >> D.elem D.int
            >> D.elem D.int


{-| Decoder for Drep type.
-}
decodeDrep : D.Decoder Drep
decodeDrep =
    D.length
        |> D.ignoreThen D.int
        |> D.andThen
            (\tag ->
                case tag of
                    0 ->
                        D.map (DrepCredential << Address.VKeyHash) (D.map Bytes.fromBytes D.bytes)

                    1 ->
                        D.map (DrepCredential << Address.ScriptHash) (D.map Bytes.fromBytes D.bytes)

                    2 ->
                        D.succeed AlwaysAbstain

                    3 ->
                        D.succeed AlwaysNoConfidence

                    _ ->
                        D.failWith ("Invalid Drep tag: " ++ String.fromInt tag)
            )
