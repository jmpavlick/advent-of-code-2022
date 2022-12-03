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
part2 =
    always "todo"


part1 : String -> String
part1 input =
    Util.lines input
        -- break input into lines
        |> List.map compartments
        -- map each line to compartments
        |> List.map findMatch
        -- find the match for each compartment
        |> Util.values
        -- List (Maybe a) -> List a
        |> List.map priorityMap
        -- map each match to its priority value
        |> List.foldl (+) 0
        -- sum them
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
