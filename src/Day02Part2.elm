module Day02Part2 exposing (eval)

import Day02Part1 exposing (Move(..), Outcome(..), play, tupleValues, values)


eval : String -> String
eval input =
    parse input
        |> List.map play
        |> List.foldl (+) 0
        |> String.fromInt


parse : String -> List ( Move, Move )
parse input =
    String.split "\n" input
        |> List.map (String.split " ")
        |> List.map
            (\arr ->
                case arr of
                    [ elfLetter, outcomeLetter ] ->
                        Just ( elfLetter, outcomeLetter )

                    _ ->
                        Nothing
            )
        |> values
        |> List.map
            (\( elfLetter, outcomeLetter ) ->
                let
                    elfMove : Maybe Move
                    elfMove =
                        letterToMove elfLetter

                    desiredOutcome : Maybe Outcome
                    desiredOutcome =
                        letterToOutcome outcomeLetter
                in
                ( elfMove
                , Maybe.map2 (\e o -> throwTheGame ( e, o )) elfMove desiredOutcome
                )
                    |> tupleValues
            )
        |> values


letterToMove : String -> Maybe Move
letterToMove input =
    case input of
        "A" ->
            Just Rock

        "B" ->
            Just Paper

        "C" ->
            Just Scissors

        _ ->
            Nothing


letterToOutcome : String -> Maybe Outcome
letterToOutcome input =
    case input of
        "X" ->
            Just Lose

        "Y" ->
            Just Draw

        "Z" ->
            Just Win

        _ ->
            Nothing


throwTheGame : ( Move, Outcome ) -> Move
throwTheGame ( elf, outcome ) =
    case ( elf, outcome ) of
        ( Rock, Win ) ->
            Paper

        ( Rock, Lose ) ->
            Scissors

        ( Paper, Lose ) ->
            Rock

        ( Paper, Win ) ->
            Scissors

        ( Scissors, Lose ) ->
            Paper

        ( Scissors, Win ) ->
            Rock

        ( a, Draw ) ->
            a
