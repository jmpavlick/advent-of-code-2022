module Day03 exposing (..)

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
        -- group into threes
        |> group
        -- compare all 3 elements in each group
        |> List.map threeWayCompare
        -- convert List (Maybe a) to List a
        |> Util.values
        -- map each match to its priority value
        |> List.map priorityMap
        -- sum them
        |> List.sum
        |> String.fromInt


group : List String -> List ( List Char, List Char, List Char )
group input =
    case input of
        a :: b :: c :: xs ->
            ( String.toList a
            , String.toList b
            , String.toList c
            )
                :: group xs

        _ ->
            []


threeWayCompare : ( List Char, List Char, List Char ) -> Maybe Char
threeWayCompare ( a, b, c ) =
    findMatches ( a, b )
        |> Tuple.pair c
        |> findMatch


findMatches : ( List Char, List Char ) -> List Char
findMatches ( a, b ) =
    let
        step : Char -> List Char -> List Char
        step current state =
            if List.member current a then
                current :: state

            else
                state
    in
    List.foldl step [] b



{---------------------------------------- ----------------------------------------}


part1 : String -> String
part1 input =
    -- break input into lines
    Util.lines input
        -- map each line to compartments
        |> List.map compartments
        -- find the match for each compartment
        |> List.map findMatch
        -- convert  List (Maybe a) to List a
        |> Util.values
        -- map each match to its priority value
        |> List.map priorityMap
        -- sum them
        |> List.sum
        |> String.fromInt


{-| 'a' - 'z' are 1 - 26
'A' - 'Z' are 27 - 52
-}
priorityMap : Char -> Int
priorityMap char =
    let
        lowerOffset : Int
        lowerOffset =
            Char.toCode 'a' - 1

        upperOffset : Int
        upperOffset =
            Char.toCode 'A' - 27
    in
    Char.toCode char
        - (if Char.isLower char then
            lowerOffset

           else
            upperOffset
          )


compartments : String -> ( List Char, List Char )
compartments input =
    let
        chars : List Char
        chars =
            String.toList input

        half : Int
        half =
            List.length chars
                |> (\l -> Basics.toFloat l / 2)
                |> Basics.round
    in
    ( List.take half chars
    , List.drop half chars
    )


findMatch : ( List Char, List Char ) -> Maybe Char
findMatch ( a, b ) =
    let
        step : Char -> Maybe Char -> Maybe Char
        step current state =
            case state of
                Nothing ->
                    if List.member current a then
                        Just current

                    else
                        Nothing

                just ->
                    just
    in
    List.foldl step Nothing b
