module Cardano.MultiAsset exposing
    ( MultiAsset, PolicyId, AssetName
    , isEmpty, get, empty, onlyToken, normalize, mintAdd
    , balance, map2, split
    , coinsToCbor, mintToCbor, coinsFromCbor, mintFromCbor
    , toMultilineString
    )

{-| Handling multi-asset values.

@docs MultiAsset, PolicyId, AssetName
@docs isEmpty, get, empty, onlyToken, normalize, mintAdd
@docs balance, map2, split
@docs coinsToCbor, mintToCbor, coinsFromCbor, mintFromCbor
@docs toMultilineString

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Map exposing (BytesMap)
import Cardano.Address exposing (CredentialHash)
import Cbor.Decode as D
import Cbor.Decode.Extra as DE
import Cbor.Encode as E
import Cbor.Encode.Extra as EE
import Integer exposing (Integer)
import Natural exposing (Natural)


{-| Type alias for handling multi-asset values.

This type should maintain some invariants by construction.
In particular, it should never contain a zero quantity of a particular token.

TODO: make sure the previous statement stays true by construction.
This would require an opaque type for MultiAsset.

-}
type alias MultiAsset int =
    BytesMap PolicyId (BytesMap AssetName int)


{-| Phantom type for 28-bytes policy id.
This is a Blacke2b-224 hash.
-}
type alias PolicyId =
    CredentialHash


{-| Phantom type for asset names.
This is a free-form bytes array of length <= 32 bytes.
-}
type AssetName
    = AssetName Never


{-| Check if the [MultiAsset] contains no token.
-}
isEmpty : MultiAsset a -> Bool
isEmpty =
    Bytes.Map.isEmpty


{-| Retrieve the amount of a given token.
-}
get : Bytes PolicyId -> Bytes AssetName -> MultiAsset a -> Maybe a
get policyId name multiAsset =
    Bytes.Map.get policyId multiAsset
        |> Maybe.andThen (Bytes.Map.get name)


{-| Create an empty [MultiAsset].
-}
empty : MultiAsset a
empty =
    Bytes.Map.empty


{-| Create a singleton [MultiAsset].
-}
onlyToken : Bytes PolicyId -> Bytes AssetName -> int -> MultiAsset int
onlyToken policy name amount =
    Bytes.Map.singleton policy (Bytes.Map.singleton name amount)


{-| Remove assets with 0 amounts.
-}
normalize : (int -> Bool) -> MultiAsset int -> MultiAsset int
normalize deletionCheck multiAsset =
    multiAsset
        |> Bytes.Map.map (Bytes.Map.filter (not << deletionCheck))
        |> Bytes.Map.filter (not << Bytes.Map.isEmpty)


{-| Add together two mint values.
-}
mintAdd : MultiAsset Integer -> MultiAsset Integer -> MultiAsset Integer
mintAdd m1 m2 =
    map2 Integer.add Integer.zero m1 m2


{-| Compute a mint balance.
-}
balance :
    BytesMap AssetName Integer
    -> { minted : BytesMap AssetName Natural, burned : BytesMap AssetName Natural }
balance assets =
    let
        initBalance =
            { minted = Bytes.Map.empty, burned = Bytes.Map.empty }

        increase amount maybePreviousAmount =
            Maybe.withDefault Natural.zero maybePreviousAmount
                |> Natural.add amount
                |> Just

        processAsset name amount { minted, burned } =
            if Integer.isNonNegative amount then
                { minted = Bytes.Map.update name (increase <| Integer.toNatural amount) minted
                , burned = burned
                }

            else
                { minted = minted
                , burned = Bytes.Map.update name (increase <| Integer.toNatural amount) burned
                }
    in
    Bytes.Map.foldlWithKeys processAsset initBalance assets


{-| Apply a function for each token pair of two [MultiAsset].
Absent tokens in one [MultiAsset] are replaced by the default value.
-}
map2 : (a -> a -> b) -> a -> MultiAsset a -> MultiAsset a -> MultiAsset b
map2 f default m1 m2 =
    let
        whenLeft : Bytes PolicyId -> BytesMap AssetName a -> MultiAsset b -> MultiAsset b
        whenLeft policyId assets accum =
            Bytes.Map.insert policyId (map2Assets f default assets Bytes.Map.empty) accum

        whenRight : Bytes PolicyId -> BytesMap AssetName a -> MultiAsset b -> MultiAsset b
        whenRight policyId assets accum =
            Bytes.Map.insert policyId (map2Assets f default Bytes.Map.empty assets) accum

        whenBoth : Bytes PolicyId -> BytesMap AssetName a -> BytesMap AssetName a -> MultiAsset b -> MultiAsset b
        whenBoth policyId a1 a2 accum =
            Bytes.Map.insert policyId (map2Assets f default a1 a2) accum
    in
    Bytes.Map.merge whenLeft whenBoth whenRight m1 m2 empty


{-| Apply a function for each token pair of a given policy.
Absent tokens are replaced by the default value.
-}
map2Assets : (a -> a -> b) -> a -> BytesMap AssetName a -> BytesMap AssetName a -> BytesMap AssetName b
map2Assets f default a1 a2 =
    let
        whenLeft : Bytes AssetName -> a -> BytesMap AssetName b -> BytesMap AssetName b
        whenLeft name amount accum =
            Bytes.Map.insert name (f amount default) accum

        whenRight : Bytes AssetName -> a -> BytesMap AssetName b -> BytesMap AssetName b
        whenRight name amount accum =
            Bytes.Map.insert name (f default amount) accum

        whenBoth : Bytes AssetName -> a -> a -> BytesMap AssetName b -> BytesMap AssetName b
        whenBoth name amount1 amount2 accum =
            Bytes.Map.insert name (f amount1 amount2) accum
    in
    Bytes.Map.merge whenLeft whenBoth whenRight a1 a2 Bytes.Map.empty


{-| Split a [MultiAsset] into a list of each individual asset `(policyId, assetName, amount)`.
-}
split : MultiAsset a -> List ( Bytes PolicyId, Bytes AssetName, a )
split multiAsset =
    let
        processAsset : Bytes PolicyId -> Bytes AssetName -> a -> List ( Bytes PolicyId, Bytes AssetName, a ) -> List ( Bytes PolicyId, Bytes AssetName, a )
        processAsset policyId asset amount accum =
            ( policyId, asset, amount ) :: accum

        processPolicyId : Bytes PolicyId -> BytesMap AssetName a -> List ( Bytes PolicyId, Bytes AssetName, a ) -> List ( Bytes PolicyId, Bytes AssetName, a )
        processPolicyId policyId assets accum =
            Bytes.Map.foldrWithKeys (processAsset policyId) accum assets
    in
    Bytes.Map.foldrWithKeys processPolicyId [] multiAsset


{-| CBOR encoder for [MultiAsset] coins.
-}
coinsToCbor : MultiAsset Natural -> E.Encoder
coinsToCbor multiAsset =
    Bytes.Map.toCbor (Bytes.Map.toCbor EE.natural) multiAsset


{-| CBOR encoder for [MultiAsset] mints.
-}
mintToCbor : MultiAsset Integer -> E.Encoder
mintToCbor multiAsset =
    Bytes.Map.toCbor (Bytes.Map.toCbor EE.integer) multiAsset


{-| CBOR decoder for [MultiAsset] coins.
-}
coinsFromCbor : D.Decoder (MultiAsset Natural)
coinsFromCbor =
    Bytes.Map.fromCbor (Bytes.Map.fromCbor DE.natural)


{-| CBOR decoder for [MultiAsset] mints.
-}
mintFromCbor : D.Decoder (MultiAsset Integer)
mintFromCbor =
    Bytes.Map.fromCbor (Bytes.Map.fromCbor DE.integer)


{-| Helper function to display `MultiAsset`.
-}
toMultilineString : (a -> String) -> MultiAsset a -> List String
toMultilineString toStr multiAsset =
    Bytes.Map.toList multiAsset
        |> List.concatMap
            (\( policyId, assets ) ->
                Bytes.Map.toList assets
                    |> List.map
                        (\( name, amount ) ->
                            String.join " "
                                [ Bytes.pretty policyId
                                , Bytes.pretty name
                                , toStr amount
                                ]
                        )
            )
