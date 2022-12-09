module Util exposing (..)

import Parser exposing ((|.), (|=), Parser)


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


tupleValues : ( Maybe a, Maybe b ) -> Maybe ( a, b )
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


template : (String -> String) -> (String -> String) -> String -> String
template part1 part2 input =
    joinWords
        [ "part 1: "
        , part1 input
        , "; part 2: "
        , part2 input
        ]


type Either a b
    = Left a
    | Right b


parseEither : Parser a -> Parser b -> Parser (Either a b)
parseEither pa pb =
    Parser.oneOf
        [ Parser.succeed Left
            |= pa
        , Parser.succeed Right
            |= pb
        ]


transpose : List (List a) -> List (List a)
transpose input =
    List.foldr (List.map2 (::)) (List.repeat (rowsLength input) []) input


rowsLength : List (List a) -> Int
rowsLength input =
    case input of
        [] ->
            0

        x :: _ ->
            List.length x


takeWhile : (a -> Bool) -> List a -> List a
takeWhile func list =
    case list of
        [] ->
            []

        x :: xs ->
            if func x then
                x :: takeWhile func xs

            else
                []


updateIf : (a -> Bool) -> (a -> a) -> a -> a
updateIf boolFunc updateFunc value =
    if boolFunc value then
        updateFunc value

    else
        value


unique : List comparable -> List comparable
unique list =
    List.sort list |> uniqueHelp


uniqueHelp : List comparable -> List comparable
uniqueHelp list =
    case list of
        [] ->
            []

        x :: xs ->
            if List.member x xs then
                uniqueHelp xs

            else
                x :: uniqueHelp xs
