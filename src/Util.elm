module Util exposing (..)


lines : String -> List String
lines =
    String.split "\n"


words : String -> List String
words =
    String.split " "


values : List (Maybe a) -> List a
values =
    List.filterMap Basics.identity


results : List (Result x a) -> List a
results =
    List.filterMap Result.toMaybe


tupleValues : ( Maybe a, Maybe a ) -> Maybe ( a, a )
tupleValues ( ma, mb ) =
    Maybe.map2 (\x y -> ( x, y )) ma mb


joinWords : List String -> String
joinWords =
    String.join " "


listToMaybe : List a -> Maybe a
listToMaybe list =
    case list of
        [ a ] ->
            Just a

        _ ->
            Nothing


sum : List number -> number
sum =
    List.foldl (+) 0


cross : (a -> a -> b) -> a -> a -> ( b, b )
cross f x y =
    ( f x y
    , f y x
    )


crossOr : (a -> a -> Bool) -> a -> a -> Bool
crossOr f a b =
    cross f a b |> (\( x, y ) -> x || y)


elemAt : Int -> List a -> Maybe a
elemAt index list =
    List.indexedMap Tuple.pair list
        |> List.filterMap
            (\( i, elem ) ->
                if i == index then
                    Just elem

                else
                    Nothing
            )
        |> List.head


flip : (b -> a -> c) -> a -> b -> c
flip f a b =
    f b a
