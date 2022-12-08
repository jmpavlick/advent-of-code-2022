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


type alias Scenic =
    { a : Int
    , b : Int
    , c : Int
    , d : Int
    }


initTreeScenic : Int -> Tree Scenic
initTreeScenic int =
    ( { a = 0, b = 0, c = 0, d = 0 }, int )


incA : Tree Scenic -> Tree Scenic
incA ( scenic, int ) =
    ( { scenic | a = scenic.a + 1 }, int )


type alias ScenicSelectUpdate =
    ( Tree Scenic -> Int, Tree Scenic -> Tree Scenic )


type alias ScenicUpdate =
    Scenic -> Int -> Scenic


scoreScenicFromLeft : ScenicSelectUpdate -> List (Tree Scenic) -> List (Tree Scenic)
scoreScenicFromLeft ( selector, update ) input =
    let
        step : Tree Scenic -> List (Tree Scenic) -> List (Tree Scenic)
        step ( scenic, height ) acc =
            Debug.todo ""
    in
    List.foldl step [] input
        |> List.reverse


scoreScenicToRight : ScenicUpdate -> List (Tree Scenic) -> List (Tree Scenic)
scoreScenicToRight update input =
    case input of
        [] ->
            []

        ( scenic, height ) :: xs ->
            ( List.map
                (\( _, xsh ) ->
                    height > xsh
                )
                xs
                |> Util.takeWhile ((==) True)
                |> List.length
                |> update scenic
            , height
            )
                :: scoreScenicToRight update xs



{---------------------------------------- ----------------------------------------}


part1 : String -> String
part1 input =
    parse input initTreeBool
        |> quadcopter scoreVisibleFromLeft
        |> countVisible
        |> Debug.toString


scoreVisibleFromLeft : List (Tree Bool) -> List (Tree Bool)
scoreVisibleFromLeft input =
    let
        step : Tree Bool -> List (Tree Bool) -> List (Tree Bool)
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


quadcopter : (List (Tree a) -> List (Tree a)) -> Forest a -> Forest a
quadcopter scoreFunc forest =
    List.map scoreFunc forest
        |> Debug.log "init"
        |> List.map List.reverse
        |> List.map scoreFunc
        |> Debug.log "reversed"
        |> List.map List.reverse
        |> Util.transpose
        |> List.map scoreFunc
        |> Debug.log "transposed"
        |> List.map List.reverse
        |> List.map scoreFunc
        |> Debug.log "transposed reversed"


countVisible : Forest Bool -> Int
countVisible =
    List.concat >> List.filter Tuple.first >> List.length


type alias Forest a =
    List (List ( a, Int ))


type alias Tree a =
    ( a, Int )


initTreeBool : Int -> Tree Bool
initTreeBool int =
    ( False, int )


parse : String -> (Int -> Tree a) -> Forest a
parse input init =
    Util.lines input
        |> List.map
            (\line ->
                String.toList line
                    |> List.map String.fromChar
                    |> List.map String.toInt
                    |> Util.values
                    |> List.map init
            )
