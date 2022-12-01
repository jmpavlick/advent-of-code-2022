module Day01 exposing (..)


eval : String -> String
eval input =
    parse input |> Debug.toString


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


parse : String -> Int
parse input =
    String.split "\n" input
        |> List.foldl step []
        |> List.sort
        |> List.reverse
        |> List.head
        |> Maybe.withDefault 0
