module Day01 exposing (..)


eval : String -> String
eval input =
    let
        inputRankedDesc : List Int
        inputRankedDesc =
            parse input
    in
    String.join " "
        [ "part 1:"
        , inputRankedDesc |> List.head |> Maybe.withDefault 0 |> Debug.toString
        , "; part 2:"
        , List.take 3 inputRankedDesc
            |> List.foldl (+) 0
            |> Debug.toString
        ]


type alias Line =
    String


cursedToInt : Line -> Int
cursedToInt =
    String.toInt >> Maybe.withDefault 0


step : Line -> List Int -> List Int
step line list =
    if String.isEmpty line then
        0 :: list

    else
        case list of
            x :: xs ->
                cursedToInt line + x :: xs

            [] ->
                cursedToInt line |> List.singleton


parse : String -> List Int
parse input =
    String.split "\n" input
        |> List.foldl step []
        |> List.sort
        |> List.reverse
