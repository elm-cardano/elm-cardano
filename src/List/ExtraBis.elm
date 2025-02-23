module List.ExtraBis exposing (chunksOf, get64, indexedMap64, sublist, take64)

import List.Extra exposing (getAt)
import UInt64 as U64 exposing (UInt64)


chunksOf : Int -> List a -> List (List a)
chunksOf =
    List.Extra.greedyGroupsOf


get64 : UInt64 -> List a -> Maybe a
get64 u64 list =
    case U64.toInt31 u64 of
        Just u31 ->
            getAt u31 list

        Nothing ->
            let
                ( iMS, iLS ) =
                    U64.toInt32s u64

                chunked =
                    chunksOf 0xFFFFFFFF list
            in
            getAt iMS chunked
                |> Maybe.andThen (getAt iLS)


take64 : UInt64 -> List a -> List a
take64 u64 list =
    case U64.toInt31 u64 of
        Just u31 ->
            List.take u31 list

        Nothing ->
            let
                ( iMS, iLS ) =
                    U64.toInt32s u64

                chunked =
                    chunksOf 0xFFFFFFFF list
            in
            List.take iMS chunked |> List.concat |> List.take iLS


indexedMap64 : (UInt64 -> a -> b) -> List a -> List b
indexedMap64 fn list =
    List.foldl
        (\x ( acc, i ) ->
            ( acc ++ [ fn i x ], U64.add i U64.one )
        )
        ( [], U64.zero )
        list
        |> Tuple.first


sublist : Int -> Int -> List a -> List a
sublist start length =
    List.drop start >> List.take length
