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
    List.map isDiagonal
        [ { head = ( 0, 0 )
          , tail = ( 1, 1 )
          }
        ]
        |> Debug.toString


type Move
    = Up Int
    | Right Int
    | Left Int
    | Down Int


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


toMove : String -> (Int -> Maybe Move)
toMove input =
    case input of
        "U" ->
            Up >> Just

        "R" ->
            Right >> Just

        "L" ->
            Left >> Just

        "D" ->
            Down >> Just

        _ ->
            always Nothing


parse : String -> List Move
parse input =
    Util.lines input
        |> List.map
            (\line ->
                case String.split " " line of
                    [ direction, value ] ->
                        String.toInt value
                            |> Maybe.map (toMove direction)

                    _ ->
                        Nothing
            )
        |> Util.values
        |> Util.values


isDiagonal : Rope -> Bool
isDiagonal { head, tail } =
    let
        ( hx, hy ) =
            head

        ( tx, ty ) =
            tail
    in
    (hx /= tx) && (hy /= ty)


move : Move -> Rope -> Rope
move m { head, tail } =
    Debug.todo ""


updateRope : Int -> (Int -> Rope -> Rope) -> Rope -> Rope
updateRope value func rope =
    func value rope


updateHeadX : Int -> Rope -> Rope
updateHeadX value rope =
    let
        ( hx, hy ) =
            rope.head
    in
    { rope | head = ( hx + value, hy ) }


updateHeadY : Int -> Rope -> Rope
updateHeadY value rope =
    let
        ( hx, hy ) =
            rope.head
    in
    { rope | head = ( hx, hy + value ) }


updateTailX : Int -> Rope -> Rope
updateTailX value rope =
    let
        ( tx, ty ) =
            rope.tail
    in
    { rope | tail = ( tx + value, ty ) }


updateTailY : Int -> Rope -> Rope
updateTailY value rope =
    let
        ( tx, ty ) =
            rope.tail
    in
    { rope | tail = ( tx, ty + value ) }
