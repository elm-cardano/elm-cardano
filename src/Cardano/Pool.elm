module Cardano.Pool exposing
    ( Id, toBech32, fromBech32
    , Params, VrfKeyHash, Relay(..), IpV4, IpV6
    , Metadata, MetadataHash
    , encodeParams, decodeParams
    )

{-| Handling Cardano values.

@docs Id, toBech32, fromBech32

@docs Params, VrfKeyHash, Relay, IpV4, IpV6

@docs Metadata, MetadataHash

@docs encodeParams, decodeParams

-}

import Bech32.Decode as Bech32
import Bech32.Encode as Bech32
import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address as Address exposing (CredentialHash, StakeAddress)
import Cardano.Utils as Utils exposing (UnitInterval)
import Cbor.Decode as D
import Cbor.Decode.Extra as DE
import Cbor.Encode as E
import Cbor.Encode.Extra as EE
import Natural exposing (Natural)


{-| Phantom type for pool ID.
This is a 28-bytes Blake2b-224 hash.
-}
type alias Id =
    CredentialHash


{-| Convert a Pool Id to its Bech32 representation.
-}
toBech32 : Bytes Id -> String
toBech32 id =
    Bech32.encode { prefix = "pool", data = Bytes.toBytes id }
        |> Result.withDefault "pool"


{-| Convert a Pool Id from its Bech32 representation.
-}
fromBech32 : String -> Maybe (Bytes Id)
fromBech32 str =
    case Bech32.decode str of
        Err _ ->
            Nothing

        Ok { prefix, data } ->
            if prefix == "pool" then
                Just <| Bytes.fromBytes data

            else
                Nothing


{-| Parameters for stake pool registration.
-}
type alias Params =
    { operator : Bytes Id
    , vrfKeyHash : Bytes VrfKeyHash
    , pledge : Natural
    , cost : Natural
    , margin : UnitInterval
    , rewardAccount : StakeAddress
    , poolOwners : List (Bytes CredentialHash)
    , relays : List Relay
    , poolMetadata : Maybe Metadata
    }


{-| Phantom type for VRF key hash.
This is a 32-bytes Blake2b-256 hash.
-}
type VrfKeyHash
    = VrfKeyHash Never


{-| A pool's relay information.
-}
type Relay
    = SingleHostAddr { port_ : Maybe Int, ipv4 : Maybe (Bytes IpV4), ipv6 : Maybe (Bytes IpV6) }
    | SingleHostName { port_ : Maybe Int, dnsName : String }
    | MultiHostName { dnsName : String }


{-| Phantom type for 4-bytes IPV4 addresses.
-}
type IpV4
    = IpV4 Never


{-| Phantom type for 16-bytes IPV6 addresses.
-}
type IpV6
    = IpV6 Never


{-| A pool's metadata hash.
-}
type alias Metadata =
    { url : String -- tstr .size (0..64)
    , poolMetadataHash : Bytes MetadataHash
    }


{-| Phantom type for 32-bytes pool metadata hash.
This is a Blacke2b-256 hash.
-}
type MetadataHash
    = PoolMetadataHash Never



-- Encoders


{-| Encode all Pool parameters.
-}
encodeParams : Params -> List E.Encoder
encodeParams poolParams =
    [ Bytes.toCbor poolParams.operator
    , Bytes.toCbor poolParams.vrfKeyHash
    , EE.natural poolParams.pledge
    , EE.natural poolParams.cost
    , Utils.encodeRationalNumber poolParams.margin
    , Address.stakeAddressToCbor poolParams.rewardAccount
    , E.list Bytes.toCbor poolParams.poolOwners
    , E.list encodeRelay poolParams.relays
    , E.maybe encodePoolMetadata poolParams.poolMetadata
    ]


encodeRelay : Relay -> E.Encoder
encodeRelay relay =
    E.list identity <|
        case relay of
            SingleHostAddr { port_, ipv4, ipv6 } ->
                [ E.int 0
                , E.maybe E.int port_
                , E.maybe Bytes.toCbor ipv4
                , E.maybe Bytes.toCbor ipv6
                ]

            SingleHostName { port_, dnsName } ->
                [ E.int 1
                , E.maybe E.int port_
                , E.string dnsName
                ]

            MultiHostName { dnsName } ->
                [ E.int 2
                , E.string dnsName
                ]


encodePoolMetadata : Metadata -> E.Encoder
encodePoolMetadata =
    E.tuple <|
        E.elems
            >> E.elem E.string .url
            >> E.elem Bytes.toCbor .poolMetadataHash



-- Decoders


{-| Decode all Pool parameters.
-}
decodeParams : D.Decoder Params
decodeParams =
    D.succeed Params
        |> D.keep (D.oneOf [ D.map Bytes.fromBytes D.bytes, DE.failWith "Failed to decode operator" ])
        |> D.keep (D.oneOf [ D.map Bytes.fromBytes D.bytes, DE.failWith "Failed to decode vrfkeyhash" ])
        |> D.keep (D.oneOf [ DE.natural, DE.failWith "Failed to decode pledge" ])
        |> D.keep DE.natural
        |> D.keep (D.oneOf [ Utils.decodeRational, DE.failWith "Failed to decode rational" ])
        |> D.keep (D.oneOf [ Address.decodeReward, DE.failWith "Failed to decode reward" ])
        |> D.keep (DE.set (D.map Bytes.fromBytes D.bytes))
        |> D.keep (D.list <| D.oneOf [ decodeRelay, DE.failWith "Failed to decode Relay" ])
        |> D.keep (D.oneOf [ D.maybe decodePoolMetadata, DE.failWith "Failed to decode pool metadata" ])


decodeRelay : D.Decoder Relay
decodeRelay =
    D.length
        |> D.andThen (\length -> D.int |> D.andThen (decodeRelayHelper length))


decodeRelayHelper : Int -> Int -> D.Decoder Relay
decodeRelayHelper length id =
    case ( length, id ) of
        -- single_host_addr = ( 0, port / null, ipv4 / null, ipv6 / null )
        ( 4, 0 ) ->
            D.map3 (\port_ ipv4 ipv6 -> SingleHostAddr { port_ = port_, ipv4 = ipv4, ipv6 = ipv6 })
                (D.maybe D.int)
                (D.maybe <| D.map Bytes.fromBytes D.bytes)
                (D.maybe <| D.map Bytes.fromBytes D.bytes)

        -- single_host_name = ( 1, port / null, dns_name )  -- An A or AAAA DNS record
        ( 3, 1 ) ->
            D.map2 (\port_ dns -> SingleHostName { port_ = port_, dnsName = dns })
                (D.maybe D.int)
                D.string

        -- multi_host_name = ( 2, dns_name )  -- A SRV DNS record
        ( 2, 2 ) ->
            D.map (\dns -> MultiHostName { dnsName = dns })
                D.string

        _ ->
            DE.failWith <|
                "Unknown length and id for relay ("
                    ++ String.fromInt length
                    ++ ", "
                    ++ String.fromInt id
                    ++ ")"


decodePoolMetadata : D.Decoder Metadata
decodePoolMetadata =
    D.tuple Metadata <|
        D.elems
            >> D.elem D.string
            >> D.elem (D.map Bytes.fromBytes D.bytes)
