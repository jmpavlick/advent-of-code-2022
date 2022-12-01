module Main exposing (..)

type alias Flags =
    { filename : String
    , input : String
    }

type alias Model = ()

type Msg = Msg

main : Program Flags Model Msg
main =
    Platform.worker
        { init = \_ -> ((), Cmd.none)
        , update = \_ _ -> ((), Cmd.none)
        , subscriptions = \_ -> Sub.none
        }