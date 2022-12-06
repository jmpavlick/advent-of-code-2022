module Main exposing (..)

import Day01
import Day02Part1
import Day02Part2
import Day03
import Day04
import Day05
import Day06


type alias Flags =
    { filename : String
    , input : String
    }


type alias Model =
    ()


type Msg
    = Msg


main : Program Flags Model Msg
main =
    Platform.worker
        { init = \flags -> eval flags |> Debug.log "output" |> always ( (), Cmd.none )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


eval : Flags -> String
eval { filename, input } =
    case filename of
        "Day01.txt" ->
            Day01.eval input

        "Day02Part1.txt" ->
            Day02Part1.eval input

        "Day02Part2.txt" ->
            Day02Part2.eval input

        "Day03.txt" ->
            Day03.eval input

        "Day04.txt" ->
            Day04.eval input

        "Day05.txt" ->
            Day05.eval input

        "Day06.txt" ->
            Day06.eval input

        _ ->
            String.join " " [ "No matching eval for", filename ]
