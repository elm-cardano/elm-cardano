module Cardano.Value exposing
    ( Value, zero, onlyLovelace, onlyToken
    , add, addTokens, subtract, atLeast, sum, normalize, compare
    , encode, fromCbor
    , toData
    , toMultilineString
    )

{-| Handling Cardano values.

@docs Value, zero, onlyLovelace, onlyToken

@docs add, addTokens, subtract, atLeast, sum, normalize, compare

@docs encode, fromCbor

@docs toData

@docs toMultilineString

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Map
import Cardano.Data as Data exposing (Data)
import Cardano.MultiAsset as MultiAsset exposing (AssetName, MultiAsset, PolicyId)
import Cbor.Decode as D
import Cbor.Decode.Extra as DE
import Cbor.Encode as E
import Cbor.Encode.Extra as EE
import Integer
import Natural exposing (Natural)


{-| A multi-asset output Value. Contains tokens indexed by policy id and asset name.

This type maintains some invariants by construction.
In particular, a Value will never contain a zero quantity of a particular token.

TODO: make sure the previous statement stays true by construction.
That would require an opaque type and some property tests.

-}
type alias Value =
    { lovelace : Natural, assets : MultiAsset Natural }


{-| Empty [Value] with 0 ada and no token.
-}
zero : Value
zero =
    { lovelace = Natural.zero, assets = MultiAsset.empty }


{-| Create a [Value] just containing Ada lovelaces.
-}
onlyLovelace : Natural -> Value
onlyLovelace lovelace =
    { lovelace = lovelace, assets = MultiAsset.empty }


{-| Create a [Value] just from some token amount.
-}
onlyToken : Bytes PolicyId -> Bytes AssetName -> Natural -> Value
onlyToken policy name amount =
    { lovelace = Natural.zero
    , assets = MultiAsset.onlyToken policy name amount
    }


{-| Add the values of two UTxOs together.
-}
add : Value -> Value -> Value
add v1 v2 =
    { lovelace = Natural.add v1.lovelace v2.lovelace
    , assets = MultiAsset.map2 Natural.add Natural.zero v1.assets v2.assets
    }


{-| Add some tokens to another [Value].
-}
addTokens : MultiAsset Natural -> Value -> Value
addTokens tokens v =
    add { lovelace = Natural.zero, assets = tokens } v


{-| subtract the second value from the first one: (v1 - v2).

It’s a saturating difference, so if the second value is bigger than the first,
the difference is clamped to 0.

The resulting [Value] is not normalized by default.
So the result may contain assets with 0 amounts.
To remove all 0 amount assets, call [normalize] on the substraction result.

-}
subtract : Value -> Value -> Value
subtract v1 v2 =
    { lovelace = Natural.sub v1.lovelace v2.lovelace
    , assets = MultiAsset.map2 Natural.sub Natural.zero v1.assets v2.assets
    }


{-| Check that some value contains at least some minimum value.

    onlyLovelace Natural.two
      |> atLeast (onlyLovelace Natural.one)
      --> True

-}
atLeast : Value -> Value -> Bool
atLeast minimum v =
    normalize (subtract minimum v) == zero


{-| Sum the values of all tokens.
-}
sum : List Value -> Value
sum allValues =
    List.foldl add zero allValues


{-| Remove 0 amounts in non-ada assets.
-}
normalize : Value -> Value
normalize v =
    { lovelace = v.lovelace
    , assets = MultiAsset.normalize Natural.isZero v.assets
    }


{-| Compare by amount of a given token.
-}
compare : (Value -> Natural) -> Value -> Value -> Order
compare withToken a b =
    Natural.compare (withToken a) (withToken b)


{-| CBOR encoder for [Value].
-}
encode : Value -> E.Encoder
encode { lovelace, assets } =
    if MultiAsset.isEmpty assets then
        EE.natural lovelace

    else
        (E.tuple <|
            E.elems
                >> E.elem EE.natural (\_ -> lovelace)
                >> E.elem MultiAsset.coinsToCbor
                    (\_ -> assets)
        )
            ()


{-| CBOR decoder for [Value].
-}
fromCbor : D.Decoder Value
fromCbor =
    D.oneOf
        -- value = coin / [coin,multiasset<uint>]
        [ D.map onlyLovelace DE.natural
        , D.tuple Value <|
            D.elems
                >> D.elem DE.natural
                >> D.elem MultiAsset.coinsFromCbor
        ]


{-| Convert a [Value] into [Data].

The generated Data is a double Map, and will only contain
an entry for Ada if there is more than 0 lovelace in the value.

-}
toData : Value -> Data
toData { lovelace, assets } =
    let
        assetsWithAda =
            if lovelace == Natural.zero then
                assets

            else
                Bytes.Map.insert Bytes.empty (Bytes.Map.singleton Bytes.empty lovelace) assets
    in
    MultiAsset.toData (Data.Int << Integer.fromNatural) assetsWithAda


{-| Helper function to display a `Value`.
-}
toMultilineString : Value -> List String
toMultilineString { lovelace, assets } =
    let
        indent spaces str =
            String.repeat spaces " " ++ str
    in
    if MultiAsset.isEmpty assets then
        [ "₳ " ++ Natural.toString lovelace ]

    else
        "with native assets:"
            :: ("   ₳ " ++ Natural.toString lovelace)
            :: List.map (indent 3) (MultiAsset.toMultilineString Natural.toString assets)
