module List.Extra exposing (chunksOf, get64, indexedMap64, last, removeAt, sublist, take64, updateAt)

import UInt64 as U64 exposing (UInt64)


chunksOf : Int -> List a -> List (List a)
chunksOf size list =
    if List.isEmpty list then
        []

    else
        List.take size list :: chunksOf size (List.drop size list)


get : Int -> List a -> Maybe a
get n =
    List.drop n >> List.head


get64 : UInt64 -> List a -> Maybe a
get64 u64 list =
    case U64.toInt31 u64 of
        Just u31 ->
            get u31 list

        Nothing ->
            let
                ( iMS, iLS ) =
                    U64.toInt32s u64

                chunked =
                    chunksOf 0xFFFFFFFF list
            in
            get iMS chunked
                |> Maybe.andThen (get iLS)


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


updateAt : Int -> (a -> a) -> List a -> List a
updateAt i fn =
    List.indexedMap
        (\j x ->
            if j == i then
                fn x

            else
                x
        )


removeAt : Int -> List a -> List a
removeAt i list =
    List.take i list ++ List.drop (i + 1) list


sublist : Int -> Int -> List a -> List a
sublist start length =
    List.drop start >> List.take length


last : List a -> Maybe a
last list =
    case List.drop (List.length list - 1) list of
        [] ->
            Nothing

        x :: _ ->
            Just x
