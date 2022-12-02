module Main exposing (..)

import Day01
import Day02Part1


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

        _ ->
            String.join " " [ "No matching eval for", filename ]
