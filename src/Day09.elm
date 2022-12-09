module Day09 exposing (..)

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
    --parse input
    [ ( Right, 4 )
    , ( Left, 3 )
    ]
        |> run
        |> Debug.log "run output"
        |> getUniqueTailPositions
        |> List.length
        |> Debug.toString


type alias Move =
    ( Direction, Int )


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Position =
    ( Int, Int )


initPosition : Position
initPosition =
    ( 0, 0 )


type alias Rope =
    { head : Position
    , tail : Position
    }


initRope : Rope
initRope =
    { head = initPosition
    , tail = initPosition
    }


toDirection : String -> Maybe Direction
toDirection input =
    case input of
        "U" ->
            Just Up

        "R" ->
            Just Right

        "L" ->
            Just Left

        "D" ->
            Just Right

        _ ->
            Nothing


parse : String -> List Move
parse input =
    Util.lines input
        |> List.map
            (\line ->
                case String.split " " line of
                    [ direction, value ] ->
                        ( toDirection direction, String.toInt value )

                    _ ->
                        ( Nothing, Nothing )
            )
        |> List.map Util.tupleValues
        |> Util.values


moveTail : Rope -> Bool
moveTail ({ head, tail } as rope) =
    let
        ( hx, hy ) =
            head

        ( tx, ty ) =
            tail
    in
    ((hx /= tx) && (hy /= ty) && (Basics.abs (hx - tx) == 1) && (Basics.abs (hy - ty) == 1))
        || rope
        == initRope
        |> not


run : List Move -> List Rope
run moves =
    let
        step : Move -> List Rope -> List Rope
        step m acc =
            List.reverse acc
                |> List.head
                |> Maybe.withDefault initRope
                |> move m
                |> List.append acc
    in
    List.foldl step [] moves


move : Move -> Rope -> List Rope
move ( direction, steps ) rope =
    let
        hx : Int
        hx =
            Tuple.first rope.head

        hy : Int
        hy =
            Tuple.second rope.head
    in
    if steps == 0 then
        []

    else
        case direction of
            Up ->
                let
                    new : Rope
                    new =
                        updateRope (hy + 1) setHeadY rope
                            |> Util.updateIf moveTail (updateRope hy setTailY >> updateRope hx setTailX)
                in
                new :: move ( direction, steps - 1 ) new

            Down ->
                let
                    new : Rope
                    new =
                        updateRope (hy - 1) setHeadY rope
                            |> Util.updateIf moveTail (updateRope hy setTailY >> updateRope hx setTailX)
                in
                new :: move ( direction, steps - 1 ) new

            Left ->
                let
                    new : Rope
                    new =
                        updateRope (hx - 1) setHeadX rope
                            |> Util.updateIf moveTail (updateRope hy setTailY >> updateRope hx setTailX)
                in
                new :: move ( direction, steps - 1 ) new

            Right ->
                let
                    new : Rope
                    new =
                        updateRope (hx + 1) setHeadX rope
                            |> Util.updateIf moveTail (updateRope hy setTailY >> updateRope hx setTailX)
                in
                new :: move ( direction, steps - 1 ) new


getUniqueTailPositions : List Rope -> List Position
getUniqueTailPositions list =
    List.map .tail list
        |> Util.unique


updateRope : Int -> (Int -> Rope -> Rope) -> Rope -> Rope
updateRope value func rope =
    func value rope


setHeadX : Int -> Rope -> Rope
setHeadX value rope =
    { rope | head = Tuple.mapFirst (always value) rope.head }


setHeadY : Int -> Rope -> Rope
setHeadY value rope =
    { rope | head = Tuple.mapSecond (always value) rope.head }


setTailX : Int -> Rope -> Rope
setTailX value rope =
    { rope | tail = Tuple.mapFirst (always value) rope.tail }


setTailY : Int -> Rope -> Rope
setTailY value rope =
    { rope | tail = Tuple.mapSecond (always value) rope.tail }
