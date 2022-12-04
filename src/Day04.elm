module Day04 exposing (..)

import Parser exposing ((|.), (|=), Parser)
import Util


eval : String -> String
eval input =
    Util.joinWords
        [ "part 1: "
        , part1 input
        , "; part 2: "
        , part2 input
        ]


part2 : String -> String
part2 input =
    -- break input into lines
    Util.lines input
        -- map each line to a pair of bounds
        |> List.map toPairs
        -- unsafe List (Result x a) to List a
        |> Util.results
        -- map each pair of bounds to a boolean describing whether or not one partially overlaps the other
        -- and make sure to check both cases, first-overlap-second and second-overlap-first
        |> List.map pairsPartiallyOverlap
        -- filter out the Falses
        |> List.filter identity
        -- count the number of items in the list
        |> List.length
        -- to string
        |> String.fromInt


pairsPartiallyOverlap : ( ( Int, Int ), ( Int, Int ) ) -> Bool
pairsPartiallyOverlap ( ( a, b ), ( x, y ) ) =
    let
        ab : List Int
        ab =
            List.range a b

        xy : List Int
        xy =
            List.range x y
    in
    isIn ab xy || isIn xy ab


isIn : List a -> List a -> Bool
isIn a b =
    List.any
        (\val ->
            List.member val b
        )
        a



{---------------------------------------- ----------------------------------------}


part1 : String -> String
part1 input =
    -- break input into lines
    Util.lines input
        -- map each line to a pair of bounds
        |> List.map toPairs
        -- unsafe List (Result x a) to List a
        |> Util.results
        -- map each pair of bounds to a boolean describing whether or not one fully overlaps the other
        -- and make sure to check both cases, first-overlap-second and second-overlap-first
        |> List.map pairsFullyOverlap
        -- filter out the Falses
        |> List.filter identity
        -- count the number of items in the list
        |> List.length
        -- to string
        |> String.fromInt


toPairs : String -> Result (List Parser.DeadEnd) ( ( Int, Int ), ( Int, Int ) )
toPairs input =
    Parser.run parser input


pairsFullyOverlap : ( ( Int, Int ), ( Int, Int ) ) -> Bool
pairsFullyOverlap ( ( a, b ), ( x, y ) ) =
    (a <= x && b >= y) || (x <= a && y >= b)


parser : Parser ( ( Int, Int ), ( Int, Int ) )
parser =
    Parser.succeed (\a b x y -> ( ( a, b ), ( x, y ) ))
        |= Parser.int
        |. Parser.symbol "-"
        |= Parser.int
        |. Parser.symbol ","
        |= Parser.int
        |. Parser.symbol "-"
        |= Parser.int
