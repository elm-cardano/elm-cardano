module Cardano.Address exposing
    ( Address(..), StakeAddress, NetworkId(..), ByronAddress
    , Credential(..), StakeCredential(..), StakeCredentialPointer, CredentialHash
    , fromString, fromBech32, fromBytes
    , enterprise, script, base, pointer
    , isShelleyWallet, extractNetworkId, extractCredentialHash, extractCredentialKeyHash, extractPaymentCred, extractPubKeyHash, extractStakeCredential, extractStakeKeyHash
    , setShelleyStakeCred
    , Dict, emptyDict, dictFromList
    , StakeDict, emptyStakeDict, stakeDictFromList
    , networkIdFromInt
    , toBech32, toBytes, stakeAddressToBytes
    , toCbor, stakeAddressToCbor, credentialToCbor, encodeNetworkId
    , decode, decodeReward, decodeCredential
    , credentialToData
    )

{-| Handling Cardano addresses.

@docs Address, StakeAddress, NetworkId, ByronAddress

@docs Credential, StakeCredential, StakeCredentialPointer, CredentialHash

@docs fromString, fromBech32, fromBytes

@docs enterprise, script, base, pointer

@docs isShelleyWallet, extractNetworkId, extractCredentialHash, extractCredentialKeyHash, extractPaymentCred, extractPubKeyHash, extractStakeCredential, extractStakeKeyHash

@docs setShelleyStakeCred

@docs Dict, emptyDict, dictFromList

@docs StakeDict, emptyStakeDict, stakeDictFromList

@docs networkIdFromInt

@docs toBech32, toBytes, stakeAddressToBytes

@docs toCbor, stakeAddressToCbor, credentialToCbor, encodeNetworkId

@docs decode, decodeReward, decodeCredential

@docs credentialToData

-}

import Bech32.Decode as Bech32
import Bech32.Encode as Bech32
import Bitwise
import Bytes as B
import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Decode as BD
import Cardano.Data as Data exposing (Data)
import Cbor.Decode as D
import Cbor.Encode as E
import Dict.Any exposing (AnyDict)
import Natural as N
import Word7


{-| Full address, including the network ID.
-}
type Address
    = Byron (Bytes ByronAddress)
    | Shelley { networkId : NetworkId, paymentCredential : Credential, stakeCredential : Maybe StakeCredential }
    | Reward StakeAddress


{-| An address type only use for things related to staking, such as delegation and reward withdrawals.
-}
type alias StakeAddress =
    { networkId : NetworkId
    , stakeCredential : Credential
    }


{-| The network ID of a transaction.
-}
type NetworkId
    = Testnet -- 0
    | Mainnet -- 1


{-| Extract the network ID from an address. Return [Nothing] for a Byron address.
-}
extractNetworkId : Address -> Maybe NetworkId
extractNetworkId address =
    case address of
        Byron _ ->
            Nothing

        Shelley { networkId } ->
            Just networkId

        Reward { networkId } ->
            Just networkId


{-| Phantom type for Byron addresses.
-}
type ByronAddress
    = ByronAddress Never


{-| A general structure for representing an on-chain credential.

[Credential] can represent both payment credentials or stake credentials.

Credentials are always one of two kinds: a direct public/private key pair, or a script (native or Plutus).

-}
type Credential
    = VKeyHash (Bytes CredentialHash)
    | ScriptHash (Bytes CredentialHash)


{-| Helper function to sort credentials in the same order
than the one auto-derived by the Haskell codebase (script first).
-}
credentialToHaskellOrderComparable : Credential -> ( Int, String )
credentialToHaskellOrderComparable cred =
    case cred of
        ScriptHash hash ->
            ( 0, Bytes.toHex hash )

        VKeyHash hash ->
            ( 1, Bytes.toHex hash )


{-| A StakeCredential represents the delegation and rewards withdrawal conditions associated with some stake address / account.

A StakeCredential is either provided inline, or, by reference using an on-chain pointer.
Read more about pointers in CIP-0019 :: Pointers.

-}
type StakeCredential
    = InlineCredential Credential
    | PointerCredential StakeCredentialPointer


{-| A stake credential pointer.

This should not be used and is only present for compatibility with previous eras.

-}
type alias StakeCredentialPointer =
    { slotNumber : Int, transactionIndex : Int, certificateIndex : Int }


{-| Phantom type for 28-bytes credential hashes,
corresponding either to VKey hashes or script hashes.

This is a Blake2b-224 hash.

-}
type CredentialHash
    = CredentialHash Never


{-| Build an [Address] from any valid string representation, such as Hex or Bech32.
-}
fromString : String -> Maybe Address
fromString str =
    case fromBech32 str of
        Just addr ->
            Just addr

        Nothing ->
            Bytes.fromHex str
                |> Maybe.andThen fromBytes


{-| Build an [Address] from its Bech32 string representation (CIP 5).
-}
fromBech32 : String -> Maybe Address
fromBech32 str =
    case Bech32.decode str of
        Err _ ->
            Nothing

        Ok { prefix, data } ->
            if List.member prefix [ "byron", "addr", "addr_test", "stake", "stake_test" ] then
                BD.decode (decodeBytes data) data

            else
                Nothing


{-| Convert an [Address] into its Bech32 string representation (CIP 5).
-}
toBech32 : Address -> String
toBech32 address =
    case address of
        Byron _ ->
            Bech32.encode { prefix = "byron", data = toBytes address |> Bytes.toBytes }
                |> Result.withDefault "byron"

        Shelley { networkId } ->
            Bech32.encode
                { prefix =
                    case networkId of
                        Mainnet ->
                            "addr"

                        Testnet ->
                            "addr_test"
                , data = toBytes address |> Bytes.toBytes
                }
                |> Result.withDefault "addr"

        Reward { networkId } ->
            Bech32.encode
                { prefix =
                    case networkId of
                        Mainnet ->
                            "stake"

                        Testnet ->
                            "stake_test"
                , data = toBytes address |> Bytes.toBytes
                }
                |> Result.withDefault "stake"


{-| Create a simple enterprise address, with only a payment credential and no stake credential.
-}
enterprise : NetworkId -> Bytes CredentialHash -> Address
enterprise networkId credentials =
    Shelley
        { networkId = networkId
        , paymentCredential = VKeyHash credentials
        , stakeCredential = Nothing
        }


{-| Create a simple script address, with only a payment credential and no stake credential.
-}
script : NetworkId -> Bytes CredentialHash -> Address
script networkId credentials =
    Shelley
        { networkId = networkId
        , paymentCredential = ScriptHash credentials
        , stakeCredential = Nothing
        }


{-| Create a base address with a payement credential and a stake credential.
-}
base : NetworkId -> Credential -> Credential -> Address
base networkId paymentCredential inlineStakeCredential =
    Shelley
        { networkId = networkId
        , paymentCredential = paymentCredential
        , stakeCredential = Just <| InlineCredential inlineStakeCredential
        }


{-| Create a pointer address.
-}
pointer : NetworkId -> Credential -> { slotNumber : Int, transactionIndex : Int, certificateIndex : Int } -> Address
pointer networkId paymentCredential p =
    Shelley
        { networkId = networkId
        , paymentCredential = paymentCredential
        , stakeCredential = Just <| PointerCredential p
        }


{-| Extract the credential hash (either key hash or script hash).
-}
extractCredentialHash : Credential -> Bytes CredentialHash
extractCredentialHash cred =
    case cred of
        VKeyHash hash ->
            hash

        ScriptHash hash ->
            hash


{-| Extract the credential key hash (Nothing if it’s a script).
-}
extractCredentialKeyHash : Credential -> Maybe (Bytes CredentialHash)
extractCredentialKeyHash cred =
    case cred of
        VKeyHash hash ->
            Just hash

        ScriptHash _ ->
            Nothing


{-| Extract the payment credential of a Shelley wallet address.
-}
extractPaymentCred : Address -> Maybe Credential
extractPaymentCred address =
    case address of
        Shelley { paymentCredential } ->
            Just paymentCredential

        _ ->
            Nothing


{-| Extract the pubkey hash of a Shelley wallet address.
-}
extractPubKeyHash : Address -> Maybe (Bytes CredentialHash)
extractPubKeyHash address =
    extractPaymentCred address
        |> Maybe.andThen extractCredentialKeyHash


{-| Extract the stake credential part of a Shelley address.
-}
extractStakeCredential : Address -> Maybe StakeCredential
extractStakeCredential address =
    case address of
        Shelley { stakeCredential } ->
            stakeCredential

        _ ->
            Nothing


{-| Extract the stake key hash of a Shelley address.
-}
extractStakeKeyHash : Address -> Maybe (Bytes CredentialHash)
extractStakeKeyHash address =
    case address of
        Shelley { stakeCredential } ->
            case stakeCredential of
                Just (InlineCredential (VKeyHash hash)) ->
                    Just hash

                _ ->
                    Nothing

        _ ->
            Nothing


{-| Change the stake credential part of a Shelley address.
Ignored if the address is anything else (Byron/Reward).
-}
setShelleyStakeCred : Maybe StakeCredential -> Address -> Address
setShelleyStakeCred maybeCred address =
    case address of
        Shelley addr ->
            Shelley { addr | stakeCredential = maybeCred }

        _ ->
            address


{-| Convenient alias for a `Dict` with [Address] keys.
When converting to a `List`, its keys are sorted by address.

WARNING: do not compare them with `==` since they contain functions.

-}
type alias Dict a =
    AnyDict String Address a


{-| Initialize an empty address dictionary.
For other operations, use the `AnyDict` module directly.

WARNING: do not compare them with `==` since they contain functions.

-}
emptyDict : Dict a
emptyDict =
    Dict.Any.empty (toCbor >> E.encode >> Bytes.fromBytes >> Bytes.toHex)


{-| Create an address dictionary from a list.
For other operations, use the `AnyDict` module directly.

WARNING: do not compare them with `==` since they contain functions.

-}
dictFromList : List ( Address, a ) -> Dict a
dictFromList =
    Dict.Any.fromList (toCbor >> E.encode >> Bytes.fromBytes >> Bytes.toHex)


{-| Convenient alias for a `Dict` with [StakeAddress] keys.
When converting to a `List`, its keys are sorted by stake address.

WARNING: do not compare them with `==` since they contain functions.

-}
type alias StakeDict a =
    AnyDict ( Int, String ) StakeAddress a


{-| Initialize an empty stake address dictionary.
For other operations, use the `AnyDict` module directly.

The keys order are derived from Haskell auto-derived credential order.
Meaning Script first, then VKey.

WARNING: do not compare them with `==` since they contain functions.

-}
emptyStakeDict : StakeDict a
emptyStakeDict =
    Dict.Any.empty (\s -> credentialToHaskellOrderComparable s.stakeCredential)


{-| Create a stake address dictionary from a list.
For other operations, use the `AnyDict` module directly.

The keys order are derived from Haskell auto-derived credential order.
Meaning Script first, then VKey.

WARNING: do not compare them with `==` since they contain functions.

-}
stakeDictFromList : List ( StakeAddress, a ) -> StakeDict a
stakeDictFromList =
    Dict.Any.fromList (\s -> credentialToHaskellOrderComparable s.stakeCredential)


{-| Check if an [Address] is of the Shelley type, with a wallet payment key, not a script.
-}
isShelleyWallet : Address -> Bool
isShelleyWallet address =
    extractPubKeyHash address /= Nothing


{-| Encode an [Address] to CBOR.
-}
toCbor : Address -> E.Encoder
toCbor address =
    Bytes.toCbor (toBytes address)


{-| Convert an [Address] to its underlying [Bytes] representation.

Byron addresses are left untouched as we don't plan to have full support of Byron era.

Shelley address description from CIP-0019:

    Header type (tttt....)  Payment Part     Delegation Part
    (0) 0000....            PaymentKeyHash   StakeKeyHash
    (1) 0001....            ScriptHash       StakeKeyHash
    (2) 0010....            PaymentKeyHash   ScriptHash
    (3) 0011....            ScriptHash       ScriptHash
    (4) 0100....            PaymentKeyHash   Pointer
    (5) 0101....            ScriptHash       Pointer
    (6) 0110....            PaymentKeyHash   ø
    (7) 0111....            ScriptHash       ø

    Header type (....tttt)
    (0) ....0000 testnet
    (1) ....0001 mainnet

For example, `61....(56 chars / 28 bytes)....` is an enterprise address (6, only a payment key) on mainnet (1).

Stake address description from CIP-0019:

    Header type (tttt....)  Stake Reference
    (14) 1110....           StakeKeyHash
    (15) 1111....           ScriptHash

-}
toBytes : Address -> Bytes Address
toBytes address =
    case address of
        Byron bytes ->
            Bytes.fromHexUnchecked (Bytes.toHex bytes)

        Shelley { networkId, paymentCredential, stakeCredential } ->
            case ( paymentCredential, stakeCredential ) of
                -- (0) 0000.... PaymentKeyHash StakeKeyHash
                ( VKeyHash paymentKeyHash, Just (InlineCredential (VKeyHash stakeKeyHash)) ) ->
                    toBytesHelper networkId "0" (Bytes.toHex paymentKeyHash ++ Bytes.toHex stakeKeyHash)

                -- (1) 0001.... ScriptHash StakeKeyHash
                ( ScriptHash paymentScriptHash, Just (InlineCredential (VKeyHash stakeKeyHash)) ) ->
                    toBytesHelper networkId "1" (Bytes.toHex paymentScriptHash ++ Bytes.toHex stakeKeyHash)

                -- (2) 0010.... PaymentKeyHash ScriptHash
                ( VKeyHash paymentKeyHash, Just (InlineCredential (ScriptHash stakeScriptHash)) ) ->
                    toBytesHelper networkId "2" (Bytes.toHex paymentKeyHash ++ Bytes.toHex stakeScriptHash)

                -- (3) 0011.... ScriptHash ScriptHash
                ( ScriptHash paymentScriptHash, Just (InlineCredential (ScriptHash stakeScriptHash)) ) ->
                    toBytesHelper networkId "3" (Bytes.toHex paymentScriptHash ++ Bytes.toHex stakeScriptHash)

                -- (4) 0100.... PaymentKeyHash Pointer
                ( VKeyHash paymentKeyHash, Just (PointerCredential _) ) ->
                    toBytesHelper networkId "4" (Bytes.toHex paymentKeyHash ++ Debug.todo "encode pointer credential")

                -- (5) 0101.... ScriptHash Pointer
                ( ScriptHash paymentScriptHash, Just (PointerCredential _) ) ->
                    toBytesHelper networkId "5" (Bytes.toHex paymentScriptHash ++ Debug.todo "encode pointer credential")

                -- (6) 0110.... PaymentKeyHash ø
                ( VKeyHash paymentKeyHash, Nothing ) ->
                    toBytesHelper networkId "6" (Bytes.toHex paymentKeyHash)

                -- (7) 0111.... ScriptHash ø
                ( ScriptHash paymentScriptHash, Nothing ) ->
                    toBytesHelper networkId "7" (Bytes.toHex paymentScriptHash)

        Reward stakeAddress ->
            stakeAddressToBytes stakeAddress
                -- Just to convert the phantom type
                |> Bytes.toHex
                |> Bytes.fromHexUnchecked


{-| CBOR encoder for a stake address.
-}
stakeAddressToCbor : StakeAddress -> E.Encoder
stakeAddressToCbor stakeAddress =
    Bytes.toCbor (stakeAddressToBytes stakeAddress)


{-| Convert a stake address to its bytes representation.
-}
stakeAddressToBytes : StakeAddress -> Bytes StakeAddress
stakeAddressToBytes { networkId, stakeCredential } =
    case stakeCredential of
        -- (14) 1110.... StakeKeyHash
        VKeyHash stakeKeyHash ->
            toBytesHelper networkId "e" (Bytes.toHex stakeKeyHash)

        -- (15) 1111.... ScriptHash
        ScriptHash stakeScriptHash ->
            toBytesHelper networkId "f" (Bytes.toHex stakeScriptHash)


toBytesHelper : NetworkId -> String -> String -> Bytes a
toBytesHelper networkId headerType payload =
    let
        network =
            case networkId of
                Testnet ->
                    "0"

                Mainnet ->
                    "1"
    in
    (headerType ++ network ++ payload)
        |> Bytes.fromHexUnchecked


{-| CBOR encoder for a [Credential], be it for payment or for stake.
-}
credentialToCbor : Credential -> E.Encoder
credentialToCbor credential =
    E.list identity <|
        case credential of
            VKeyHash keyHash ->
                [ E.int 0
                , Bytes.toCbor keyHash
                ]

            ScriptHash scriptHash ->
                [ E.int 1
                , Bytes.toCbor scriptHash
                ]


{-| Convert a Credential to its Data representation.
-}
credentialToData : Credential -> Data
credentialToData credential =
    case credential of
        VKeyHash keyHash ->
            Data.Constr N.zero [ Data.Bytes <| Bytes.toAny keyHash ]

        ScriptHash scriptHash ->
            Data.Constr N.one [ Data.Bytes <| Bytes.toAny scriptHash ]


{-| CBOR encoder for [NetworkId].
-}
encodeNetworkId : NetworkId -> E.Encoder
encodeNetworkId networkId =
    E.int <|
        case networkId of
            Testnet ->
                0

            Mainnet ->
                1



-- Decode


{-| CBOR decoder for [Address].
-}
decode : D.Decoder Address
decode =
    D.bytes
        |> D.andThen
            (\bytes ->
                case BD.decode (decodeBytes bytes) bytes of
                    Just address ->
                        D.succeed address

                    Nothing ->
                        let
                            _ =
                                Debug.log "Failed to decode address" (Bytes.toHex <| Bytes.fromBytes bytes)
                        in
                        D.fail
            )


{-| CBOR decoder for [StakeAddress].
This only succeeds for a valid [Address] of the [Reward] variant.
-}
decodeReward : D.Decoder StakeAddress
decodeReward =
    decode
        |> D.andThen
            (\addr ->
                case addr of
                    Reward stakeAddress ->
                        D.succeed stakeAddress

                    _ ->
                        D.fail
            )


{-| Convert an [Address] from its [Bytes] representation.
-}
fromBytes : Bytes a -> Maybe Address
fromBytes bytes =
    let
        actualBytes =
            Bytes.toBytes bytes
    in
    BD.decode (decodeBytes actualBytes) actualBytes


{-| Address decoder from raw bytes. Internal use only.
Cannot be compose with other decoders as it may not consume the correct number of bytes.

This needs a copy of the bytes of the address to compensate
the absence of backtracking in bytes decoders.
So if we read the first byte and

-}
decodeBytes : B.Bytes -> BD.Decoder Address
decodeBytes bytesCopy =
    BD.unsignedInt8
        |> BD.andThen
            (\header ->
                case Bitwise.shiftRightBy 4 header of
                    -- (0) 0000.... PaymentKeyHash StakeKeyHash
                    0 ->
                        BD.map2
                            (\payment stake -> base (networkIdFromHeader header) (VKeyHash payment) (VKeyHash stake))
                            (BD.map Bytes.fromBytes <| BD.bytes 28)
                            (BD.map Bytes.fromBytes <| BD.bytes 28)

                    -- (1) 0001.... ScriptHash StakeKeyHash
                    1 ->
                        BD.map2
                            (\payment stake -> base (networkIdFromHeader header) (ScriptHash payment) (VKeyHash stake))
                            (BD.map Bytes.fromBytes <| BD.bytes 28)
                            (BD.map Bytes.fromBytes <| BD.bytes 28)

                    -- (2) 0010.... PaymentKeyHash ScriptHash
                    2 ->
                        BD.map2
                            (\payment stake -> base (networkIdFromHeader header) (VKeyHash payment) (ScriptHash stake))
                            (BD.map Bytes.fromBytes <| BD.bytes 28)
                            (BD.map Bytes.fromBytes <| BD.bytes 28)

                    -- (3) 0011.... ScriptHash ScriptHash
                    3 ->
                        BD.map2
                            (\payment stake -> base (networkIdFromHeader header) (ScriptHash payment) (ScriptHash stake))
                            (BD.map Bytes.fromBytes <| BD.bytes 28)
                            (BD.map Bytes.fromBytes <| BD.bytes 28)

                    -- (4) 0100.... PaymentKeyHash Pointer
                    4 ->
                        BD.map2
                            (\payment ->
                                pointer (networkIdFromHeader header) (VKeyHash payment)
                            )
                            (BD.map Bytes.fromBytes <| BD.bytes 28)
                            (BD.map3 StakeCredentialPointer Word7.fromBytes Word7.fromBytes Word7.fromBytes)

                    -- (5) 0101.... ScriptHash Pointer
                    5 ->
                        BD.map2
                            (\payment ->
                                pointer (networkIdFromHeader header) (ScriptHash payment)
                            )
                            (BD.map Bytes.fromBytes <| BD.bytes 28)
                            (BD.map3 StakeCredentialPointer Word7.fromBytes Word7.fromBytes Word7.fromBytes)

                    -- (6) 0110.... PaymentKeyHash ø
                    6 ->
                        BD.map (enterprise (networkIdFromHeader header))
                            (BD.map Bytes.fromBytes <| BD.bytes 28)

                    -- (7) 0111.... ScriptHash ø
                    7 ->
                        BD.map (script (networkIdFromHeader header))
                            (BD.map Bytes.fromBytes <| BD.bytes 28)

                    -- (8) 1000.... Byron
                    8 ->
                        BD.map (always <| Byron (Bytes.fromBytes bytesCopy)) <|
                            BD.bytes (B.width bytesCopy - 1)

                    -- (14) 1110.... StakeKeyHash
                    14 ->
                        BD.map (\cred -> Reward <| StakeAddress (networkIdFromHeader header) cred)
                            (BD.map (VKeyHash << Bytes.fromBytes) <| BD.bytes 28)

                    -- (15) 1111.... ScriptHash
                    15 ->
                        BD.map (\cred -> Reward <| StakeAddress (networkIdFromHeader header) cred)
                            (BD.map (ScriptHash << Bytes.fromBytes) <| BD.bytes 28)

                    _ ->
                        BD.fail
            )


networkIdFromHeader : Int -> NetworkId
networkIdFromHeader header =
    case Bitwise.and 0x0F header of
        0 ->
            Testnet

        1 ->
            Mainnet

        n ->
            Debug.todo ("Unrecognized network id:" ++ String.fromInt n)


{-| Convert to [NetworkId] from its integer representation.
-}
networkIdFromInt : Int -> Maybe NetworkId
networkIdFromInt n =
    case n of
        0 ->
            Just Testnet

        1 ->
            Just Mainnet

        _ ->
            Nothing


{-| Decode [Credential] which is either from a key or a script.
-}
decodeCredential : D.Decoder Credential
decodeCredential =
    D.length
        |> D.andThen
            (\length ->
                -- A stake credential contains 2 elements
                if length == 2 then
                    D.int
                        |> D.andThen
                            (\id ->
                                if id == 0 then
                                    -- If the id is 0, it's a vkey hash
                                    D.map (VKeyHash << Bytes.fromBytes) D.bytes

                                else if id == 1 then
                                    -- If the id is 1, it's a script hash
                                    D.map (ScriptHash << Bytes.fromBytes) D.bytes

                                else
                                    D.fail
                            )

                else
                    D.fail
            )
