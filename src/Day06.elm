module Day06 exposing (..)

import Parser exposing ((|.), (|=), Parser)
import Util


eval : String -> String
eval input =
    Util.template part1 part2 input


part2 : String -> String
part2 input =
    reduce2 input |> String.length |> Debug.toString


reduce2 : String -> String
reduce2 input =
    String.toList input |> reduce2Help 0 |> String.fromList


reduce2Help : Int -> List Char -> List Char
reduce2Help count list =
    if List.length list < 14 then
        []

    else if List.take 14 list |> allDifferent then
        List.concat [ List.take 14 list, List.repeat count ' ' ] |> Debug.log "wtf"

    else
        reduce2Help (1 + count) (List.drop 1 list)



{---------------------------------------- ----------------------------------------}


part1 : String -> String
part1 input =
    reduce input |> (\reduced -> calculatePosition { input = input, reduced = reduced }) |> Debug.toString


reduce : String -> String
reduce input =
    case String.toList input of
        a :: b :: c :: d :: xs ->
            if allDifferent [ a, b, c, d ] then
                String.fromList xs

            else
                b :: c :: d :: xs |> String.fromList |> reduce

        _ ->
            ""


calculatePosition : { input : String, reduced : String } -> Int
calculatePosition { input, reduced } =
    String.length input - (String.length reduced |> Debug.log "reduced length")


allDifferent : List comparable -> Bool
allDifferent input =
    let
        step : a -> List a -> List a
        step val acc =
            if List.member val acc then
                acc

            else
                val :: acc
    in
    List.foldr step [] input
        |> (==) input
