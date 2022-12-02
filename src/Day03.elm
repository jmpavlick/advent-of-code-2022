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
