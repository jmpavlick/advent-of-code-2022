module Main exposing (..)

import Day01


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
    let
        _ =
            rotate nx
                |> Debug.log "uwu whats this"
    in
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

        _ ->
            String.join " " [ "No matching eval for", filename ]


nx : List (List String)
nx =
    [ [ "a", "b", "c", "d" ]
    , [ "e", "f", "g", "h" ]
    , [ "i", "j", "k", "l" ]
    , [ "m", "n", "o", "p" ]
    ]


step : List a -> ( List a, List (List a) ) -> ( List a, List (List a) )
step list ( accHeads, accTails ) =
    case list of
        [] ->
            ( accHeads, accTails )

        x :: xs ->
            ( x :: accHeads, xs :: accTails )


separate : List (List a) -> ( List a, List (List a) )
separate input =
    List.foldr step ( [], [] ) input


recurse : ( List a, List (List a) ) -> List (List a)
recurse ( x, xs ) =
    case xs of
        [] ->
            []

        exess ->
            case x of
                [] ->
                    separate exess |> recurse

                ex ->
                    ex :: (separate exess |> recurse)


rotate : List (List a) -> List (List a)
rotate input =
    recurse ( [], input )


shift : List (Maybe a) -> List (Maybe a)
shift list =
    