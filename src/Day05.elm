module Day05 exposing (..)

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
    "TODO"



{---------------------------------------- ----------------------------------------}


part1 : String -> String
part1 input =
    "TODO"
