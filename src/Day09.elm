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
    List.map (\x -> ( x, holdTail x ))
        [ { head = ( 0, 0 )
          , tail = ( 0, 0 )
          }
        , { head = ( 0, 1 )
          , tail = ( 0, 0 )
          }
        , { head = ( 1, 1 )
          , tail = ( 0, 0 )
          }
        , { head = ( 1, 0 )
          , tail = ( 0, 0 )
          }
        , { head = ( 1, -1 )
          , tail = ( 0, 0 )
          }
        , { head = ( 0, -1 )
          , tail = ( 0, 0 )
          }
        , { head = ( -1, -1 )
          , tail = ( 0, 0 )
          }
        , { head = ( -1, 0 )
          , tail = ( 0, 0 )
          }
        , { head = ( -1, 1 )
          , tail = ( 0, 0 )
          }
        ]
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


holdTail : Rope -> Bool
holdTail ({ head, tail } as rope) =
    let
        ( hx, hy ) =
            head

        ( tx, ty ) =
            tail
    in
    ((hx /= tx) && (hy /= ty) && (Basics.abs (hx - tx) == 1) && (Basics.abs (hy - ty) == 1))
        || rope
        == initRope


move : List Rope -> Move -> Rope -> Rope
move history ( direction, steps ) rope =
    if steps == 0 then
        rope

    else
        case direction of
            Up ->
                updateRope 1 updateHeadX rope
                    -- |> Util.updateIf (Debug.todo "")
                    |> Debug.todo ""

            _ ->
                Debug.todo ""


updateRope : Int -> (Int -> Rope -> Rope) -> Rope -> Rope
updateRope value func rope =
    func value rope


updateHeadX : Int -> Rope -> Rope
updateHeadX value rope =
    { rope | head = Tuple.mapFirst (always value) rope.head }


updateHeadY : Int -> Rope -> Rope
updateHeadY value rope =
    { rope | head = Tuple.mapSecond (always value) rope.head }


updateTailX : Int -> Rope -> Rope
updateTailX value rope =
    { rope | tail = Tuple.mapFirst (always value) rope.tail }


updateTailY : Int -> Rope -> Rope
updateTailY value rope =
    { rope | tail = Tuple.mapSecond (always value) rope.tail }
