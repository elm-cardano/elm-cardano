module Cardano.TxExamples exposing
    ( example1, example2, example3, example4, example5, example6
    , prettyTx
    )

{-| Just a module to make sure that examples compile when we change stuff.

@docs example1, example2, example3, example4, example5, example6

@docs prettyTx

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Map as Map
import Cardano exposing (ActionProposal(..), CertificateIntent(..), CredentialWitness(..), Fee(..), ScriptWitness(..), SpendSource(..), TxIntent(..), TxOtherInfo(..), VoterWitness(..), WitnessSource(..), finalize, finalizeAdvanced)
import Cardano.Address as Address exposing (Address(..), Credential(..), CredentialHash, NetworkId(..), StakeAddress, StakeCredential(..))
import Cardano.CoinSelection as CoinSelection
import Cardano.Data as Data
import Cardano.Gov as Gov exposing (Action, ActionId, Anchor, Constitution, CostModels, Drep(..), DrepVotingThresholds, PoolVotingThresholds, ProposalProcedure, ProtocolParamUpdate, ProtocolVersion, Vote(..), Voter(..), VotingProcedure, noParamUpdate)
import Cardano.Metadatum as Metadatum
import Cardano.MultiAsset as MultiAsset
import Cardano.Redeemer as Redeemer exposing (ExUnitPrices, ExUnits)
import Cardano.Script as Script exposing (NativeScript(..), PlutusVersion(..))
import Cardano.Transaction as Transaction exposing (Certificate(..), Transaction)
import Cardano.Uplc as Uplc
import Cardano.Utils exposing (RationalNumber)
import Cardano.Utxo as Utxo exposing (DatumOption(..), Output, OutputReference)
import Cardano.Value as Value exposing (Value)
import Cbor.Encode as E
import Dict.Any
import Integer
import Natural exposing (Natural)



-- EXAMPLES global state


ada =
    -- Asset amounts are typed with unbounded Natural numbers
    { one = Value.onlyLovelace (Natural.fromSafeString "1000000")
    , two = Value.onlyLovelace (Natural.fromSafeString "2000000")
    , four = Value.onlyLovelace (Natural.fromSafeString "4000000")
    , ten = Value.onlyLovelace (Natural.fromSafeString "10000000")
    }


exAddr =
    { me = makeWalletAddress "me"
    , you = makeWalletAddress "you"
    }


dog =
    { policyId = dummyCredentialHash "dog"
    , policyIdStr = "dog"
    , assetName = Bytes.fromText "yksoh"
    , assetNameStr = "yksoh"
    , scriptRef = makeRef "dogScriptRef" 0
    , refOutput =
        { address = makeAddress "dogScriptRefAddress"
        , amount = ada.two
        , datumOption = Nothing
        , referenceScript = Just <| Script.refFromScript <| Script.Native <| Script.ScriptAll [] -- dummy
        }
    }


cat =
    { policyId = dummyCredentialHash "cat"
    , policyIdStr = "cat"
    , assetName = Bytes.fromText "felix"
    , assetNameStr = "felix"
    , scriptRef = makeRef "catScriptRef" 0
    , refOutput =
        { address = makeAddress "catScriptRefAddress"
        , amount = ada.two
        , datumOption = Nothing
        , referenceScript = Just <| Script.refFromScript <| Script.Native <| Script.ScriptAll [] -- dummy
        }
    }


globalStateUtxos : Utxo.RefDict Output
globalStateUtxos =
    Utxo.refDictFromList
        [ makeAdaOutput 0 exAddr.me 2 --   2 ada at my address
        , makeAdaOutput 1 exAddr.me 10 -- 10 ada at my address
        , makeAdaOutput 2 exAddr.me 5 --   5 ada at my address
        , makeAsset 3 exAddr.me dog.policyIdStr dog.assetNameStr 2
        , makeAsset 4 exAddr.me cat.policyIdStr cat.assetNameStr 5
        , ( dog.scriptRef, dog.refOutput )
        , ( cat.scriptRef, cat.refOutput )
        ]



-- EXAMPLE 1: Simple transfer


example1 _ =
    [ Spend <| FromWallet { address = exAddr.me, value = ada.one, guaranteedUtxos = [] }
    , SendTo exAddr.you ada.one
    ]
        |> finalize globalStateUtxos [ TxMetadata { tag = Natural.fromSafeInt 14, metadata = Metadatum.Int (Integer.fromSafeInt 42) } ]



-- EXAMPLE 2: mint/burn with native script


example2 _ =
    -- minting 1 dog (amounts are of type Integer: unbounded positive or negative integers)
    [ MintBurn
        { policyId = dog.policyId
        , assets = Map.singleton dog.assetName Integer.one
        , scriptWitness = NativeWitness { script = WitnessReference dog.scriptRef, expectedSigners = [] }
        }
    , SendTo exAddr.me (Value.onlyToken dog.policyId dog.assetName Natural.one)

    -- burning 1 cat
    , Spend <|
        FromWallet
            { address = exAddr.me
            , value = Value.onlyToken cat.policyId cat.assetName Natural.one
            , guaranteedUtxos = []
            }
    , MintBurn
        { policyId = cat.policyId
        , assets = Map.singleton cat.assetName Integer.negativeOne
        , scriptWitness = NativeWitness { script = WitnessReference cat.scriptRef, expectedSigners = [] }
        }
    ]
        |> finalize globalStateUtxos []



-- EXAMPLE 3: spend from a Plutus script
-- The input index is provided in the redeemer


utxoBeingSpent =
    makeRef "previouslySentToLock" 0


findSpendingUtxo inputs =
    case inputs of
        [] ->
            0

        ( id, ref ) :: next ->
            if ref == utxoBeingSpent then
                id

            else
                findSpendingUtxo next


example3 _ =
    let
        ( myKeyCred, myStakeCred ) =
            ( Address.extractPubKeyHash exAddr.me
                |> Maybe.withDefault (Bytes.fromText "should not fail")
            , Address.extractStakeCredential exAddr.me
            )

        -- Lock script made with Aiken
        lock =
            { script = Script.plutusScriptFromBytes PlutusV3 (Bytes.fromHexUnchecked "58b501010032323232323225333002323232323253330073370e900118041baa0011323232533300a3370e900018059baa00113322323300100100322533301100114a0264a66601e66e3cdd718098010020a5113300300300130130013758601c601e601e601e601e601e601e601e601e60186ea801cdd7180718061baa00116300d300e002300c001300937540022c6014601600460120026012004600e00260086ea8004526136565734aae7555cf2ab9f5742ae881")
            , scriptHash = Bytes.fromHexUnchecked "3ff0b1bb5815347c6f0c05328556d80c1f83ca47ac410d25ffb4a330"
            }

        -- Combining the script hash with our stake credential
        -- to keep the locked ada staked.
        lockScriptAddress =
            Address.Shelley
                { networkId = Mainnet
                , paymentCredential = ScriptHash lock.scriptHash
                , stakeCredential = myStakeCred
                }

        -- Build a redeemer that contains the index of the spent script input.
        redeemer txBody =
            List.indexedMap Tuple.pair txBody.inputs
                |> findSpendingUtxo
                |> (Data.Int << Integer.fromSafeInt)

        -- Helper function to create an output at the lock script address.
        -- It contains our key credential in the datum.
        makeLockedOutput adaAmount =
            { address = lockScriptAddress
            , amount = adaAmount
            , datumOption = Just (DatumValue (Data.Bytes <| Bytes.toAny myKeyCred))
            , referenceScript = Nothing
            }

        -- Add to local state utxos some previously sent 4 ada.
        localStateUtxos =
            globalStateUtxos
                |> Dict.Any.insert utxoBeingSpent (makeLockedOutput ada.four)
    in
    -- Collect 2 ada from the lock script
    [ Spend <|
        FromPlutusScript
            { spentInput = utxoBeingSpent
            , datumWitness = Nothing
            , plutusScriptWitness =
                { script = ( Script.plutusVersion lock.script, WitnessValue <| Script.cborWrappedBytes lock.script )
                , redeemerData = redeemer
                , requiredSigners = [ myKeyCred ]
                }
            }
    , SendTo exAddr.me ada.two

    -- Return the other 2 ada to the lock script (there was 4 ada initially)
    , SendToOutput (makeLockedOutput ada.two)
    ]
        |> finalize localStateUtxos []



-- EXAMPLE 4: Register a stake key, delegate to a stake pool and to a DRep


example4 _ =
    let
        myStakeKeyHash =
            Address.extractStakeKeyHash exAddr.me
                |> Maybe.withDefault (dummyCredentialHash "ERROR")
    in
    [ Spend <| FromWallet { address = exAddr.me, value = ada.two, guaranteedUtxos = [] }
    , IssueCertificate <| RegisterStake { delegator = WithKey myStakeKeyHash, deposit = Natural.fromSafeInt 2000000 }
    , IssueCertificate <| DelegateStake { delegator = WithKey myStakeKeyHash, poolId = Bytes.dummy 28 "poolId" }
    , IssueCertificate <| DelegateVotes { delegator = WithKey myStakeKeyHash, drep = VKeyHash <| dummyCredentialHash "drep" }
    ]
        |> finalize globalStateUtxos []



-- EXAMPLE 5: Update parameters, withdraw from treasury, new constitution, no confidence, info, and hard fork


example5 _ =
    let
        myStakeKeyHash =
            Address.extractStakeKeyHash exAddr.me
                |> Maybe.withDefault (dummyCredentialHash "ERROR")

        myStakeAddress =
            { networkId = Mainnet
            , stakeCredential = VKeyHash myStakeKeyHash
            }

        govState =
            { guardrailsScript =
                -- Current guardrails script info retrieved from the devs docs:
                -- https://developers.cardano.org/docs/get-started/cardano-cli/governance/create%20governance%20actions/#the-guardrails-script
                Just
                    { policyId = Bytes.fromHexUnchecked "fa24fb305126805cf2164c161d852a0e7330cf988f1fe558cf7d4a64"
                    , plutusVersion = PlutusV3

                    -- I removed one bytes wrapping of the script cbor so that it works
                    , scriptWitness = WitnessValue <| Bytes.fromHexUnchecked "5908510101003232323232323232323232323232323232323232323232323232323232323232323232323232323232259323255333573466e1d20000011180098111bab357426ae88d55cf00104554ccd5cd19b87480100044600422c6aae74004dd51aba1357446ae88d55cf1baa3255333573466e1d200a35573a002226ae84d5d11aab9e00111637546ae84d5d11aba235573c6ea800642b26006003149a2c8a4c301f801c0052000c00e0070018016006901e4070c00e003000c00d20d00fc000c0003003800a4005801c00e003002c00d20c09a0c80e1801c006001801a4101b5881380018000600700148013003801c006005801a410100078001801c006001801a4101001f8001800060070014801b0038018096007001800600690404002600060001801c0052008c00e006025801c006001801a41209d8001800060070014802b003801c006005801a410112f501c3003800c00300348202b7881300030000c00e00290066007003800c00b003482032ad7b806038403060070014803b00380180960003003800a4021801c00e003002c00d20f40380e1801c006001801a41403f800100a0c00e0029009600f0030078040c00e002900a600f003800c00b003301a483403e01a600700180060066034904801e00060001801c0052016c01e00600f801c006001801980c2402900e30000c00e002901060070030128060c00e00290116007003800c00b003483c0ba03860070018006006906432e00040283003800a40498003003800a404d802c00e00f003800c00b003301a480cb0003003800c003003301a4802b00030001801c01e0070018016006603490605c0160006007001800600660349048276000600030000c00e0029014600b003801c00c04b003800c00300348203a2489b00030001801c00e006025801c006001801a4101b11dc2df80018000c0003003800a4055802c00e007003012c00e003000c00d2080b8b872c000c0006007003801809600700180060069040607e4155016000600030000c00e00290166007003012c00e003000c00d2080c001c000c0003003800a405d801c00e003002c00d20c80180e1801c006001801a412007800100a0c00e00290186007003013c0006007001480cb005801801e006003801800e00600500403003800a4069802c00c00f003001c00c007003803c00e003002c00c05300333023480692028c0004014c00c00b003003c00c00f003003c00e00f003800c00b00301480590052008003003800a406d801c00e003002c00d2000c00d2006c00060070018006006900a600060001801c0052038c00e007001801600690006006901260003003800c003003483281300020141801c005203ac00e006027801c006001801a403d800180006007001480f3003801804e00700180060069040404af3c4e302600060001801c005203ec00e006013801c006001801a4101416f0fd20b80018000600700148103003801c006005801a403501c3003800c0030034812b00030000c00e0029021600f003800c00a01ac00e003000c00ccc08d20d00f4800b00030000c0000000000803c00c016008401e006009801c006001801807e0060298000c000401e006007801c0060018018074020c000400e00f003800c00b003010c000802180020070018006006019801805e0003000400600580180760060138000800c00b00330134805200c400e00300080330004006005801a4001801a410112f58000801c00600901260008019806a40118002007001800600690404a75ee01e00060008018046000801801e000300c4832004c025201430094800a0030028052003002c00d2002c000300648010c0092002300748028c0312000300b48018c0292012300948008c0212066801a40018000c0192008300a2233335573e00250002801994004d55ce800cd55cf0008d5d08014c00cd5d10011263009222532900389800a4d2219002912c80344c01526910c80148964cc04cdd68010034564cc03801400626601800e0071801226601800e01518010096400a3000910c008600444002600244004a664600200244246466004460044460040064600444600200646a660080080066a00600224446600644b20051800484ccc02600244666ae68cdc3801000c00200500a91199ab9a33710004003000801488ccd5cd19b89002001800400a44666ae68cdc4801000c00a00122333573466e20008006005000912a999ab9a3371200400222002220052255333573466e2400800444008440040026eb400a42660080026eb000a4264666015001229002914801c8954ccd5cd19b8700400211333573466e1c00c006001002118011229002914801c88cc044cdc100200099b82002003245200522900391199ab9a3371066e08010004cdc1001001c002004403245200522900391199ab9a3371266e08010004cdc1001001c00a00048a400a45200722333573466e20cdc100200099b820020038014000912c99807001000c40062004912c99807001000c400a2002001199919ab9a357466ae880048cc028dd69aba1003375a6ae84008d5d1000934000dd60010a40064666ae68d5d1800c0020052225933006003357420031330050023574400318010600a444aa666ae68cdc3a400000222c22aa666ae68cdc4000a4000226600666e05200000233702900000088994004cdc2001800ccdc20010008cc010008004c01088954ccd5cd19b87480000044400844cc00c004cdc300100091119803112c800c60012219002911919806912c800c4c02401a442b26600a004019130040018c008002590028c804c8888888800d1900991111111002a244b267201722222222008001000c600518000001112a999ab9a3370e004002230001155333573466e240080044600823002229002914801c88ccd5cd19b893370400800266e0800800e00100208c8c0040048c0088cc008008005"
                    }
            , lastEnactedCommitteeAction = Nothing
            , lastEnactedConstitutionAction = Nothing
            , lastEnactedHardForkAction = Nothing
            , lastEnactedProtocolParamUpdateAction = Nothing
            }

        -- Add a 600K ada utxo to the local state
        -- for the 6 x 100K deposits
        localStateUtxos =
            globalStateUtxos
                |> Dict.Any.insert
                    (makeRef "3" 3)
                    (Utxo.fromLovelace exAddr.me <| Natural.fromSafeString "600000000000")

        evalScriptsCosts =
            Uplc.evalScriptsCosts
                { budget = Uplc.conwayDefaultBudget
                , slotConfig = Uplc.slotConfigMainnet
                , costModels = Uplc.conwayDefaultCostModels
                }

        ada100K =
            Natural.fromSafeString "100000000000"

        propose govAction offchainInfo =
            Propose
                { govAction = govAction
                , offchainInfo = offchainInfo
                , deposit = ada100K
                , depositReturnAccount = myStakeAddress
                }
    in
    [ -- 600K deposit for all the gov actions
      Spend <|
        FromWallet
            { address = exAddr.me
            , value = Value.onlyLovelace (Natural.mul Natural.six ada100K)
            , guaranteedUtxos = []
            }

    -- Change minPoolCost to 0
    , propose
        (ParameterChange { noParamUpdate | minPoolCost = Just Natural.zero })
        { url = "param-url", dataHash = Bytes.dummy 32 "param-hash-" }

    -- Withdraw 1M ada from the treasury
    , propose
        (TreasuryWithdrawals [ { destination = myStakeAddress, amount = Natural.fromSafeString "1000000000000" } ])
        { url = "withdraw-url", dataHash = Bytes.dummy 32 "withdraw-hash-" }

    -- Change the constitution to not have a guardrails script anymore
    , propose
        (NewConstitution
            { anchor = { url = "constitution-url", dataHash = Bytes.dummy 32 "const-hash-" }
            , scripthash = Nothing
            }
        )
        { url = "new-const-url", dataHash = Bytes.dummy 32 "new-const-hash-" }

    -- Change to a state of No Confidence
    , propose NoConfidence
        { url = "no-conf-url", dataHash = Bytes.dummy 32 "no-conf-hash-" }

    -- Ask an info poll about pineapple pizza
    , propose Info
        { url = "info-url", dataHash = Bytes.dummy 32 "info-hash-" }

    -- Finally, suggest a hard fork
    , propose (HardForkInitiation ( 14, 0 ))
        { url = "hf-url", dataHash = Bytes.dummy 32 "hf-hash-" }
    ]
        |> finalizeAdvanced
            { govState = govState
            , localStateUtxos = localStateUtxos
            , coinSelectionAlgo = CoinSelection.largestFirst
            , evalScriptsCosts = evalScriptsCosts
            , costModels = Uplc.conwayDefaultCostModels
            }
            (AutoFee { paymentSource = exAddr.me })
            []



-- EXAMPLE 6: Votes


example6 _ =
    let
        myStakeKeyHash =
            Address.extractStakeKeyHash exAddr.me
                |> Maybe.withDefault (dummyCredentialHash "ERROR")

        -- Action being voted on
        actionId index =
            { transactionId = Bytes.dummy 32 "actionTx-"
            , govActionIndex = index
            }

        -- Trivial native script drep that always succeed
        drepScript =
            ScriptAll []

        drepScriptHash =
            Script.hash (Script.Native drepScript)

        -- Define different voters
        withMyDrepCred =
            WithDrepCred (WithKey myStakeKeyHash)

        withMyPoolCred =
            WithPoolCred (dummyCredentialHash "poolId-")

        withMyDrepScript =
            WithDrepCred (WithScript drepScriptHash <| NativeWitness { script = WitnessValue drepScript, expectedSigners = [] })
    in
    [ Vote withMyDrepCred
        [ { actionId = actionId 0, vote = VoteYes, rationale = Nothing }
        , { actionId = actionId 1, vote = VoteYes, rationale = Nothing }
        ]
    , Vote withMyPoolCred
        [ { actionId = actionId 1, vote = VoteNo, rationale = Nothing }
        , { actionId = actionId 0, vote = VoteNo, rationale = Nothing }
        ]

    -- action 1 will be overwritten by action 0
    , Vote withMyDrepScript [ { actionId = actionId 1, vote = VoteAbstain, rationale = Nothing } ]
    , Vote withMyDrepScript [ { actionId = actionId 0, vote = VoteAbstain, rationale = Nothing } ]

    -- Small trick just to help the Tx builder figuring out who is paying the Tx fee
    , SendTo exAddr.me (Value.onlyLovelace Natural.zero)
    ]
        |> finalize globalStateUtxos []



-- Helper functions to build stuff


dummyCredentialHash : String -> Bytes CredentialHash
dummyCredentialHash str =
    Bytes.dummy 28 str


makeWalletAddress : String -> Address
makeWalletAddress name =
    Address.Shelley
        { networkId = Mainnet
        , paymentCredential = VKeyHash (dummyCredentialHash <| "key-" ++ name)
        , stakeCredential = Just (InlineCredential (VKeyHash <| dummyCredentialHash <| "stk-" ++ name))
        }


makeAddress : String -> Address
makeAddress name =
    Address.enterprise Mainnet (dummyCredentialHash <| "key-" ++ name)


makeRef : String -> Int -> OutputReference
makeRef id index =
    { transactionId = Bytes.dummy 32 id
    , outputIndex = index
    }


makeAsset : Int -> Address -> String -> String -> Int -> ( OutputReference, Output )
makeAsset index address policyId name amount =
    ( makeRef (String.fromInt index) index
    , { address = address
      , amount = makeToken policyId name amount
      , datumOption = Nothing
      , referenceScript = Nothing
      }
    )


makeAdaOutput : Int -> Address -> Int -> ( OutputReference, Output )
makeAdaOutput index address amount =
    ( makeRef (String.fromInt index) index
    , Utxo.fromLovelace address (Natural.fromSafeInt <| 1000000 * amount)
    )


makeToken : String -> String -> Int -> Value
makeToken policyId name amount =
    Value.onlyToken (dummyCredentialHash policyId) (Bytes.fromText name) (Natural.fromSafeInt amount)



-- Helper function to display pretty stuff


prettyAddr address =
    case address of
        Byron b ->
            Bytes.pretty b

        Shelley { paymentCredential, stakeCredential } ->
            [ Just "Addr:", Just (prettyCred paymentCredential), Maybe.map prettyStakeCred stakeCredential ]
                |> List.filterMap identity
                |> String.join " "

        Reward stakeAddr ->
            "StakeAddr:" ++ prettyCred stakeAddr.stakeCredential


prettyStakeCred stakeCred =
    case stakeCred of
        Address.InlineCredential cred ->
            "stake:" ++ prettyCred cred

        Address.PointerCredential _ ->
            "stake:PointerAddr"


prettyCred cred =
    case cred of
        Address.VKeyHash b ->
            "key:" ++ Bytes.pretty b

        Address.ScriptHash b ->
            "script:" ++ Bytes.pretty b


prettyWithdrawal : ( StakeAddress, Natural ) -> String
prettyWithdrawal ( { stakeCredential }, amount ) =
    "₳ " ++ Natural.toString amount ++ " @ stakeCred:" ++ prettyCred stakeCredential


prettyCert : Certificate -> String
prettyCert cert =
    case cert of
        StakeRegistrationCert { delegator } ->
            "stake-registration for " ++ prettyCred delegator

        StakeDeregistrationCert { delegator } ->
            "stake-deregistration for " ++ prettyCred delegator

        StakeDelegationCert { delegator, poolId } ->
            "stake-delegation for " ++ prettyCred delegator ++ " to pool " ++ Bytes.pretty poolId

        PoolRegistrationCert _ ->
            "pool-registration"

        PoolRetirementCert { poolId, epoch } ->
            "pool-retirement for pool " ++ Bytes.pretty poolId ++ " at epoch " ++ Natural.toString epoch

        GenesisKeyDelegationCert _ ->
            "genesis-key-delegation"

        MoveInstantaneousRewardsCert _ ->
            "move-instantaneous-rewards"

        RegCert { delegator, deposit } ->
            "stake-registration for " ++ prettyCred delegator ++ " with deposit ₳ " ++ Natural.toString deposit

        UnregCert { delegator, refund } ->
            "stake-unregistration for " ++ prettyCred delegator ++ " with refund ₳ " ++ Natural.toString refund

        VoteDelegCert { delegator, drep } ->
            "vote-deleg-cert for " ++ prettyCred delegator ++ " to " ++ prettyDrep drep

        StakeVoteDelegCert { delegator, poolId, drep } ->
            "stake-vote-deleg-cert for " ++ prettyCred delegator ++ " to " ++ prettyDrep drep ++ " and " ++ Bytes.pretty poolId

        StakeRegDelegCert _ ->
            "stake-reg-deleg-cert TODO"

        VoteRegDelegCert _ ->
            "vote-reg-deleg-cert TODO"

        StakeVoteRegDelegCert _ ->
            "stake-vote-reg-deleg-cert TODO"

        AuthCommitteeHotCert _ ->
            "auth-committee-hot-cert TODO"

        ResignCommitteeColdCert _ ->
            "resign-committee-cold-cert TODO"

        RegDrepCert { drepCredential, deposit } ->
            "reg-drep-cert as " ++ prettyCred drepCredential ++ " with deposit ₳ " ++ Natural.toString deposit

        UnregDrepCert { drepCredential, refund } ->
            "unreg-drep-cert as " ++ prettyCred drepCredential ++ " with refund ₳ " ++ Natural.toString refund

        UpdateDrepCert { drepCredential } ->
            "update-drep-cert of " ++ prettyCred drepCredential


prettyVote : ( Voter, List ( ActionId, VotingProcedure ) ) -> List String
prettyVote ( voter, votes ) =
    let
        voterStr =
            case voter of
                VoterCommitteeHotCred cred ->
                    "Committee Hot Key: " ++ prettyCred cred

                VoterDrepCred cred ->
                    "DRep: " ++ prettyCred cred

                VoterPoolId poolId ->
                    "Pool: " ++ Bytes.pretty poolId

        voteStr ( actionId, procedure ) =
            let
                prettyRationale =
                    case procedure.anchor of
                        Just { url } ->
                            " | with rationale: " ++ url

                        Nothing ->
                            ""
            in
            case procedure.vote of
                VoteNo ->
                    "vote NO for: " ++ prettyActionId actionId ++ prettyRationale

                VoteYes ->
                    "vote YES for: " ++ prettyActionId actionId ++ prettyRationale

                VoteAbstain ->
                    "vote ABSTAIN for: " ++ prettyActionId actionId ++ prettyRationale
    in
    voterStr :: List.map (indent 3 << voteStr) votes


prettyProposal : ProposalProcedure -> List String
prettyProposal proposal =
    let
        ( actionTitle, actionDetails ) =
            prettyAction proposal.govAction
    in
    [ "Proposal: " ++ actionTitle
    , "   Deposit: ₳ " ++ Natural.toString proposal.deposit ++ ", Return Account: " ++ prettyAddr (Address.Reward proposal.depositReturnAccount)
    , "   Anchor: " ++ prettyAnchor proposal.anchor
    ]
        ++ List.map (indent 3) actionDetails


prettyAction : Action -> ( String, List String )
prettyAction action =
    case action of
        Gov.ParameterChange { latestEnacted, protocolParamUpdate, guardrailsPolicy } ->
            ( "Parameter Change"
            , List.concat
                [ [ "Latest Enacted: " ++ Maybe.withDefault "None" (Maybe.map prettyActionId latestEnacted)
                  , "Guardrails Policy: " ++ Maybe.withDefault "None" (Maybe.map Bytes.pretty guardrailsPolicy)
                  , "Protocol Param Update:"
                  ]
                , List.map (indent 3) (prettyProtocolParamUpdate protocolParamUpdate)
                ]
            )

        Gov.HardForkInitiation { latestEnacted, protocolVersion } ->
            ( "Hard Fork Initiation"
            , [ "Latest Enacted: " ++ Maybe.withDefault "None" (Maybe.map prettyActionId latestEnacted)
              , "Protocol Version: " ++ prettyProtocolVersion protocolVersion
              ]
            )

        Gov.TreasuryWithdrawals { withdrawals, guardrailsPolicy } ->
            ( "Treasury Withdrawals"
            , [ "Withdrawals: " ++ String.join ", " (List.map prettyWithdrawal withdrawals)
              , "Guardrails Policy: " ++ Maybe.withDefault "None" (Maybe.map Bytes.pretty guardrailsPolicy)
              ]
            )

        Gov.NoConfidence { latestEnacted } ->
            ( "No Confidence"
            , [ "Latest Enacted: " ++ Maybe.withDefault "None" (Maybe.map prettyActionId latestEnacted) ]
            )

        Gov.UpdateCommittee { latestEnacted, removedMembers, addedMembers, quorumThreshold } ->
            ( "Update Committee"
            , [ "Latest Enacted: " ++ Maybe.withDefault "None" (Maybe.map prettyActionId latestEnacted)
              , "Removed Members: " ++ String.join ", " (List.map prettyCred removedMembers)
              , "Added Members: " ++ String.join ", " (List.map prettyAddedMember addedMembers)
              , "Quorum Threshold: " ++ prettyRational quorumThreshold
              ]
            )

        Gov.NewConstitution { latestEnacted, constitution } ->
            ( "New Constitution"
            , List.concat
                [ [ "Latest Enacted: " ++ Maybe.withDefault "None" (Maybe.map prettyActionId latestEnacted), "Constitution:" ]
                , List.map (indent 3) (prettyConstitution constitution)
                ]
            )

        Gov.Info ->
            ( "Info", [] )


prettyActionId : ActionId -> String
prettyActionId actionId =
    String.join " "
        [ "TxId:" ++ Bytes.pretty actionId.transactionId
        , "#" ++ String.fromInt actionId.govActionIndex
        ]


prettyProtocolParamUpdate : ProtocolParamUpdate -> List String
prettyProtocolParamUpdate update =
    List.filterMap identity
        [ Maybe.map (\v -> "MinFeeA: " ++ Natural.toString v) update.minFeeA
        , Maybe.map (\v -> "MinFeeB: " ++ Natural.toString v) update.minFeeB
        , Maybe.map (\v -> "MaxBlockBodySize: " ++ String.fromInt v) update.maxBlockBodySize
        , Maybe.map (\v -> "MaxTxSize: " ++ String.fromInt v) update.maxTransactionSize
        , Maybe.map (\v -> "MaxBlockHeaderSize: " ++ String.fromInt v) update.maxBlockHeaderSize
        , Maybe.map (\v -> "KeyDeposit: " ++ Natural.toString v) update.keyDeposit
        , Maybe.map (\v -> "PoolDeposit: " ++ Natural.toString v) update.poolDeposit
        , Maybe.map (\v -> "MaxEpoch: " ++ Natural.toString v) update.maximumEpoch
        , Maybe.map (\v -> "DesiredNumberOfStakePools: " ++ String.fromInt v) update.desiredNumberOfStakePools
        , Maybe.map (\v -> "PoolPledgeInfluence: " ++ prettyRational v) update.poolPledgeInfluence
        , Maybe.map (\v -> "ExpansionRate: " ++ prettyRational v) update.expansionRate
        , Maybe.map (\v -> "TreasuryGrowthRate: " ++ prettyRational v) update.treasuryGrowthRate
        , Maybe.map (\v -> "MinPoolCost: " ++ Natural.toString v) update.minPoolCost
        , Maybe.map (\v -> "AdaPerUtxoByte: " ++ Natural.toString v) update.adaPerUtxoByte
        , Maybe.map (\v -> "CostModels: " ++ prettyCostModels v) update.costModelsForScriptLanguages
        , Maybe.map (\v -> "ExUnitPrices: " ++ prettyExUnitPrices v) update.executionCosts
        , Maybe.map (\v -> "MaxTxExUnits: " ++ prettyExUnits v) update.maxTxExUnits
        , Maybe.map (\v -> "MaxBlockExUnits: " ++ prettyExUnits v) update.maxBlockExUnits
        , Maybe.map (\v -> "MaxValueSize: " ++ String.fromInt v) update.maxValueSize
        , Maybe.map (\v -> "CollateralPercentage: " ++ String.fromInt v) update.collateralPercentage
        , Maybe.map (\v -> "MaxCollateralInputs: " ++ String.fromInt v) update.maxCollateralInputs
        , Maybe.map (\v -> "PoolVotingThresholds: " ++ prettyPoolVotingThresholds v) update.poolVotingThresholds
        , Maybe.map (\v -> "DrepVotingThresholds: " ++ prettyDrepVotingThresholds v) update.drepVotingThresholds
        , Maybe.map (\v -> "MinCommitteeSize: " ++ String.fromInt v) update.minCommitteeSize
        , Maybe.map (\v -> "CommitteeTermLimit: " ++ Natural.toString v) update.committeeTermLimit
        , Maybe.map (\v -> "GovernanceActionValidityPeriod: " ++ Natural.toString v) update.governanceActionValidityPeriod
        , Maybe.map (\v -> "GovernanceActionDeposit: " ++ Natural.toString v) update.governanceActionDeposit
        , Maybe.map (\v -> "DrepDeposit: " ++ Natural.toString v) update.drepDeposit
        , Maybe.map (\v -> "DrepInactivityPeriod: " ++ Natural.toString v) update.drepInactivityPeriod
        , Maybe.map (\v -> "MinFeeRefScriptCostPerByte: " ++ String.fromInt v) update.minFeeRefScriptCostPerByte
        ]


prettyProtocolVersion : ProtocolVersion -> String
prettyProtocolVersion ( major, minor ) =
    String.fromInt major ++ "." ++ String.fromInt minor


prettyAddedMember : { newMember : Credential, expirationEpoch : Natural } -> String
prettyAddedMember { newMember, expirationEpoch } =
    prettyCred newMember ++ " (expires: " ++ Natural.toString expirationEpoch ++ ")"


prettyConstitution : Constitution -> List String
prettyConstitution constitution =
    [ "Anchor: " ++ prettyAnchor constitution.anchor
    , "Script Hash: " ++ Maybe.withDefault "None" (Maybe.map Bytes.pretty constitution.scripthash)
    ]


prettyAnchor : Anchor -> String
prettyAnchor anchor =
    "URL: " ++ anchor.url ++ ", Hash: " ++ Bytes.pretty anchor.dataHash


prettyRational : RationalNumber -> String
prettyRational { numerator, denominator } =
    String.fromInt numerator ++ "/" ++ String.fromInt denominator


prettyCostModels : CostModels -> String
prettyCostModels costModels =
    String.join " , "
        [ "PlutusV1: " ++ Maybe.withDefault "None" (Maybe.map (String.join "," << List.map String.fromInt) costModels.plutusV1)
        , "PlutusV2: " ++ Maybe.withDefault "None" (Maybe.map (String.join "," << List.map String.fromInt) costModels.plutusV2)
        , "PlutusV3: " ++ Maybe.withDefault "None" (Maybe.map (String.join "," << List.map String.fromInt) costModels.plutusV3)
        ]
        |> (\params -> "[ " ++ params ++ " ]")


prettyExUnitPrices : ExUnitPrices -> String
prettyExUnitPrices { memPrice, stepPrice } =
    "Mem: " ++ prettyRational memPrice ++ ", Step: " ++ prettyRational stepPrice


prettyExUnits : ExUnits -> String
prettyExUnits { mem, steps } =
    "Mem: " ++ String.fromInt mem ++ ", Steps: " ++ String.fromInt steps


prettyPoolVotingThresholds : PoolVotingThresholds -> String
prettyPoolVotingThresholds thresholds =
    String.join " , "
        [ "Motion No Confidence: " ++ prettyRational thresholds.motionNoConfidence
        , "Committee Normal: " ++ prettyRational thresholds.committeeNormal
        , "Committee No Confidence: " ++ prettyRational thresholds.committeeNoConfidence
        , "Hard Fork Initiation: " ++ prettyRational thresholds.hardforkInitiation
        , "Security Relevant Parameter: " ++ prettyRational thresholds.securityRelevantParameter
        ]
        |> (\params -> "[ " ++ params ++ " ]")


prettyDrepVotingThresholds : DrepVotingThresholds -> String
prettyDrepVotingThresholds thresholds =
    String.join " , "
        [ "Motion No Confidence: " ++ prettyRational thresholds.motionNoConfidence
        , "Committee Normal: " ++ prettyRational thresholds.committeeNormal
        , "Committee No Confidence: " ++ prettyRational thresholds.committeeNoConfidence
        , "Update Constitution: " ++ prettyRational thresholds.updateConstitution
        , "Hard Fork Initiation: " ++ prettyRational thresholds.hardforkInitiation
        , "PP Network Group: " ++ prettyRational thresholds.ppNetworkGroup
        , "PP Economic Group: " ++ prettyRational thresholds.ppEconomicGroup
        , "PP Technical Group: " ++ prettyRational thresholds.ppTechnicalGroup
        , "PP Governance Group: " ++ prettyRational thresholds.ppGovernanceGroup
        , "Treasury Withdrawal: " ++ prettyRational thresholds.treasuryWithdrawal
        ]
        |> (\params -> "[ " ++ params ++ " ]")


prettyDrep : Drep -> String
prettyDrep drep =
    case drep of
        DrepCredential cred ->
            prettyCred cred

        AlwaysAbstain ->
            "ALWAYS ABSTAIN"

        AlwaysNoConfidence ->
            "ALWAYS NO CONFIDENCE"


prettyValue : Value -> List String
prettyValue { lovelace, assets } =
    if MultiAsset.isEmpty assets then
        [ "₳ " ++ Natural.toString lovelace ]

    else
        "with native assets:"
            :: ("   ₳ " ++ Natural.toString lovelace)
            :: List.map (indent 3) (prettyAssets Natural.toString assets)


prettyAssets toStr multiAsset =
    Map.toList multiAsset
        |> List.concatMap
            (\( policyId, assets ) ->
                Map.toList assets
                    |> List.map
                        (\( name, amount ) ->
                            String.join " "
                                [ Bytes.pretty policyId
                                , Bytes.pretty name
                                , toStr amount
                                ]
                        )
            )


prettyDatum datumOption =
    case datumOption of
        Utxo.DatumHash h ->
            "datumHash: " ++ Bytes.pretty h

        Utxo.DatumValue data ->
            "datum: " ++ prettyCbor Data.toCbor data


prettyCbor toCbor x =
    E.encode (toCbor x) |> Bytes.fromBytes |> Bytes.toHex


prettyScriptRef scriptRef =
    case Script.refScript scriptRef of
        Script.Native _ ->
            "NativeScript: " ++ Bytes.toHex (Script.refBytes scriptRef)

        Script.Plutus _ ->
            "PlutusScript: " ++ Bytes.toHex (Script.refBytes scriptRef)


prettyScript script =
    case script of
        Script.Native nativeScript ->
            "NativeScript: " ++ prettyCbor Script.encodeNativeScript nativeScript

        Script.Plutus plutusScript ->
            "PlutusScript: " ++ Bytes.toHex (Script.cborWrappedBytes plutusScript)


prettyInput ref =
    String.join " "
        [ "TxId:" ++ Bytes.pretty ref.transactionId
        , "#" ++ String.fromInt ref.outputIndex
        ]


prettyOutput : Output -> List String
prettyOutput { address, amount, datumOption, referenceScript } =
    ("- " ++ prettyAddr address)
        :: ([ Just <| prettyValue amount
            , Maybe.map (List.singleton << prettyDatum) datumOption
            , Maybe.map (List.singleton << prettyScriptRef) referenceScript
            ]
                |> List.filterMap identity
                |> List.concat
                |> List.map (indent 2)
           )


ifNonEmpty emptyCheck collection nonEmptyContent =
    if emptyCheck collection then
        []

    else
        nonEmptyContent


prettyList sectionTitle prettify list =
    ifNonEmpty List.isEmpty list (sectionTitle :: List.map (indent 3 << prettify) list)


prettyMints sectionTitle multiAsset =
    ifNonEmpty MultiAsset.isEmpty multiAsset (sectionTitle :: List.map (indent 3) (prettyAssets Integer.toString multiAsset))


prettyVKeyWitness { vkey, signature } =
    String.join ", "
        [ "vkey:" ++ Bytes.pretty vkey
        , "signature:" ++ Bytes.pretty signature
        ]


prettyRedeemer redeemer =
    String.join " "
        [ Debug.toString redeemer.tag
        , "index:" ++ String.fromInt redeemer.index
        , "exPrice:₳" ++ Natural.toString (Redeemer.feeCost Transaction.defaultTxFeeParams.scriptExUnitPrice redeemer.exUnits)
        , "exUnits: mem " ++ String.fromInt redeemer.exUnits.mem ++ ", steps " ++ String.fromInt redeemer.exUnits.steps
        , "data:" ++ prettyCbor Data.toCbor redeemer.data
        ]


prettyMetadata ( tag, metadatum ) =
    Natural.toString tag ++ ": " ++ prettyCbor Metadatum.toCbor metadatum


indent spaces str =
    String.repeat spaces " " ++ str


prettyTx : Transaction -> String
prettyTx tx =
    let
        body =
            List.concat
                [ [ "Tx fee: ₳ " ++ (tx.body.fee |> Natural.toString) ]
                , prettyList "Tx ref inputs:" prettyInput tx.body.referenceInputs
                , prettyList "Tx inputs:" prettyInput tx.body.inputs
                , ifNonEmpty List.isEmpty tx.body.outputs [ "Tx outputs:" ]
                , List.concatMap prettyOutput tx.body.outputs
                    |> List.map (indent 3)
                , prettyMints "Tx mints:" tx.body.mint
                , prettyList "Tx withdrawals:" prettyWithdrawal tx.body.withdrawals
                , prettyList "Tx certificates:" prettyCert tx.body.certificates
                , ifNonEmpty List.isEmpty tx.body.proposalProcedures [ "Tx proposals:" ]
                , List.concatMap prettyProposal tx.body.proposalProcedures
                    |> List.map (indent 3)
                , ifNonEmpty List.isEmpty tx.body.votingProcedures [ "Tx votes:" ]
                , List.concatMap prettyVote tx.body.votingProcedures
                    |> List.map (indent 3)
                , prettyList "Tx required signers:" Bytes.pretty tx.body.requiredSigners
                , prettyList
                    ("Tx collateral (total: ₳ " ++ (Maybe.withDefault "not set" <| Maybe.map String.fromInt tx.body.totalCollateral) ++ "):")
                    prettyInput
                    tx.body.collateral
                , Maybe.map prettyOutput tx.body.collateralReturn
                    |> Maybe.withDefault []
                    |> prettyList "Tx collateral return:" identity
                ]

        witnessSet =
            List.concat <|
                List.filterMap identity
                    [ tx.witnessSet.vkeywitness
                        |> Maybe.map (prettyList "Tx vkey witness:" prettyVKeyWitness)
                    , tx.witnessSet.nativeScripts
                        |> Maybe.map (prettyList "Tx native scripts:" (prettyScript << Script.Native))
                    , tx.witnessSet.plutusV1Script
                        |> Maybe.map (prettyList "Tx plutus V1 scripts:" Bytes.pretty)
                    , tx.witnessSet.plutusV2Script
                        |> Maybe.map (prettyList "Tx plutus V2 scripts:" Bytes.pretty)
                    , tx.witnessSet.plutusV3Script
                        |> Maybe.map (prettyList "Tx plutus V3 scripts:" Bytes.pretty)
                    , tx.witnessSet.redeemer
                        |> Maybe.map (prettyList "Tx redeemers:" prettyRedeemer)

                    -- TODO: plutusData
                    ]

        -- Pretty print auxiliary data
        auxiliaryData =
            case tx.auxiliaryData of
                Nothing ->
                    []

                Just auxData ->
                    List.concat <|
                        [ prettyList "Tx metadata:" prettyMetadata auxData.labels
                        , prettyList "Tx native scripts in auxiliary data:" (prettyScript << Script.Native) auxData.nativeScripts
                        , prettyList "Tx plutus V1 scripts in auxiliary data:" Bytes.pretty auxData.plutusV1Scripts
                        , prettyList "Tx plutus V2 scripts in auxiliary data:" Bytes.pretty auxData.plutusV2Scripts
                        , prettyList "Tx plutus V3 scripts in auxiliary data:" Bytes.pretty auxData.plutusV3Scripts
                        ]
    in
    List.concat [ body, witnessSet, auxiliaryData ]
        |> String.join "\n"
