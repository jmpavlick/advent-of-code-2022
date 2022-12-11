module Day10 exposing (..)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser)
import Util exposing (Either(..))


eval : String -> String
eval input =
    Util.template part1 part2 input


part2 : String -> String
part2 input =
    parse input
        |> List.foldl update init
        |> List.drop 1
        |> List.reverse
        |> List.map shift
        |> List.map asPixel
        |> Util.groupsOf 40
        |> List.map (String.concat >> Debug.log "")
        |> always ""


shift : IndexedInstruction -> IndexedInstruction
shift iix =
    { iix | cycleCount = iix.cycleCount - 1 }


asPixel : IndexedInstruction -> String
asPixel { cycleCount, x } =
    if List.member (Basics.modBy 40 cycleCount) [ x - 1, x, x + 1 ] then
        "#"

    else
        " "



{---------------------------------------- ----------------------------------------}


part1 : String -> String
part1 input =
    parse input
        |> List.foldl update init
        |> List.reverse
        |> filter
        |> List.map (\{ cycleCount, x } -> cycleCount * x)
        |> List.sum
        |> Debug.toString


type Instruction
    = NoOp
    | PushAddX
    | AddXValue Int


type alias IndexedInstruction =
    { cycleCount : Int
    , x : Int
    }


type alias Model =
    List IndexedInstruction


init : Model
init =
    List.singleton { cycleCount = 1, x = 1 }


type alias Msg =
    ( Int, Instruction )


parse : String -> List Msg
parse input =
    Util.lines input
        |> List.map Util.words
        |> List.filterMap
            (\words ->
                case words of
                    [ _ ] ->
                        List.singleton NoOp |> Just

                    [ _, addXValue ] ->
                        String.toInt addXValue
                            |> Maybe.map (\v -> Just [ PushAddX, AddXValue v ])
                            |> Maybe.withDefault Nothing

                    _ ->
                        Nothing
            )
        |> List.concat
        |> List.indexedMap (\index instruction -> Tuple.pair (index + 2) instruction)


update : Msg -> Model -> Model
update ( index, instruction ) model =
    case model of
        [] ->
            []

        m :: _ ->
            (case instruction of
                NoOp ->
                    { m | cycleCount = index }

                PushAddX ->
                    { m | cycleCount = index }

                AddXValue value ->
                    { m
                        | cycleCount = index
                        , x = m.x + value
                    }
            )
                :: model


filter : Model -> Model
filter model =
    List.filterMap
        (\({ cycleCount, x } as cycle) ->
            if List.member cycleCount [ 20, 60, 100, 140, 180, 220 ] then
                Just cycle

            else
                Nothing
        )
        model
