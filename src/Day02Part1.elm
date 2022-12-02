module Day02Part1 exposing (Move(..), Outcome(..), eval, play, tupleValues, values)


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
                    [ elfLetter, playerLetter ] ->
                        Just ( elfLetter, playerLetter )

                    _ ->
                        Nothing
            )
        |> values
        |> List.map
            (\( elfLetter, playerLetter ) ->
                ( letterToMove elfLetter
                , letterToMove playerLetter
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

        "X" ->
            Just Rock

        "Y" ->
            Just Paper

        "Z" ->
            Just Scissors

        _ ->
            Nothing


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
