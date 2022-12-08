module Day08 exposing (..)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser)
import Util exposing (Either(..))


eval : String -> String
eval input =
    Util.template part1 part2 input


part2 : String -> String
part2 input =
    "TODO"



{---------------------------------------- ----------------------------------------}


part1 : String -> String
part1 input =
    parse input
        |> quadcopter
        |> countVisible
        |> Debug.toString


mapVisibleFromLeft : List Tree -> List Tree
mapVisibleFromLeft input =
    let
        step : Tree -> List Tree -> List Tree
        step ( visible, height ) acc =
            ( visible
                || height
                > (List.map Tuple.second acc
                    |> List.maximum
                    |> Maybe.withDefault -1
                  )
            , height
            )
                :: acc
    in
    List.foldl step [] input
        |> List.reverse



{- | quadcopter: compute visibility from every angle -}


quadcopter : Forest -> Forest
quadcopter forest =
    List.map mapVisibleFromLeft forest
        |> Debug.log "init"
        |> List.map List.reverse
        |> List.map mapVisibleFromLeft
        |> Debug.log "reversed"
        |> List.map List.reverse
        |> Util.transpose
        |> List.map mapVisibleFromLeft
        |> Debug.log "transposed"
        |> List.map List.reverse
        |> List.map mapVisibleFromLeft
        |> Debug.log "transposed reversed"


countVisible : Forest -> Int
countVisible =
    List.concat >> List.filter Tuple.first >> List.length


type alias Forest =
    List (List ( Bool, Int ))


type alias Tree =
    ( Bool, Int )


initTree : Int -> Tree
initTree int =
    ( False, int )


parse : String -> Forest
parse input =
    Util.lines input
        |> List.map
            (\line ->
                String.toList line
                    |> List.map String.fromChar
                    |> List.map String.toInt
                    |> Util.values
                    |> List.map initTree
            )
