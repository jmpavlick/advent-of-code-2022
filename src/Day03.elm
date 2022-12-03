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
part1 =
    always "todo"


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
