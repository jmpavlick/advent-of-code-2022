module Day02Part2 exposing (eval)


eval : String -> String
eval input =
    parse input
        |> List.map play
        |> List.foldl (+) 0
        |> String.fromInt


type Move
    = Rock
    | Paper
    | Scissors


type Outcome
    = Win
    | Lose
    | Draw


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


tupleValues : ( Maybe a, Maybe a ) -> Maybe ( a, a )
tupleValues ( ma, mb ) =
    Maybe.map2 (\x y -> ( x, y )) ma mb


values : List (Maybe a) -> List a
values =
    List.filterMap Basics.identity


play : ( Move, Move ) -> Int
play ( elf, player ) =
    (winLoseDraw elf player |> outcomeScore)
        + moveScore player


moveScore : Move -> Int
moveScore move =
    case move of
        Rock ->
            1

        Paper ->
            2

        Scissors ->
            3


winLoseDraw : Move -> Move -> Outcome
winLoseDraw elf player =
    case ( elf, player ) of
        ( Rock, Paper ) ->
            Win

        ( Rock, Scissors ) ->
            Lose

        ( Paper, Rock ) ->
            Lose

        ( Paper, Scissors ) ->
            Win

        ( Scissors, Paper ) ->
            Lose

        ( Scissors, Rock ) ->
            Win

        ( Rock, Rock ) ->
            Draw

        ( Paper, Paper ) ->
            Draw

        ( Scissors, Scissors ) ->
            Draw


outcomeScore : Outcome -> Int
outcomeScore outcome =
    case outcome of
        Win ->
            6

        Lose ->
            0

        Draw ->
            3
