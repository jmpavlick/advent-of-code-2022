module Day08 exposing (..)

import Util exposing (Either(..))


eval : String -> String
eval input =
    Util.template part1 part2 input


part2 : String -> String
part2 input =
    parse input initTreeScenic
        |> scenicQuadcopter
        |> List.concat
        |> List.map calculateScenicScore
        |> List.maximum
        |> Debug.toString


type alias Scenic =
    { a : Int
    , b : Int
    , c : Int
    , d : Int
    }


initTreeScenic : Int -> Tree Scenic
initTreeScenic value =
    ( { a = 0, b = 0, c = 0, d = 0 }, value )


type alias ScenicUpdate =
    Scenic -> Int -> Scenic


scenicUpdateA : ScenicUpdate
scenicUpdateA scenic value =
    { scenic | a = value }


scenicUpdateB : ScenicUpdate
scenicUpdateB scenic value =
    { scenic | b = value }


scenicUpdateC : ScenicUpdate
scenicUpdateC scenic value =
    { scenic | c = value }


scenicUpdateD : ScenicUpdate
scenicUpdateD scenic value =
    { scenic | d = value }


scoreScenicToRight : ScenicUpdate -> List (Tree Scenic) -> List (Tree Scenic)
scoreScenicToRight update input =
    case input of
        [] ->
            []

        ( scenic, height ) :: xs ->
            ( List.map
                (\( _, xsh ) ->
                    height > xsh
                )
                xs
                -- at least one tree is visible, even if it's taller than us, as long as it has a neighbor
                |> oneMoreTrue
                |> List.length
                |> update scenic
            , height
            )
                :: scoreScenicToRight update xs


oneMoreTrue : List Bool -> List Bool
oneMoreTrue list =
    case list of
        [] ->
            []

        x :: xs ->
            if x then
                True :: oneMoreTrue xs

            else
                True :: []


scenicQuadcopter : Forest Scenic -> Forest Scenic
scenicQuadcopter forest =
    List.map (scoreScenicToRight scenicUpdateA) forest
        --|> Debug.log "init"
        |> List.map List.reverse
        |> List.map (scoreScenicToRight scenicUpdateB)
        --|> Debug.log "reversed"
        |> List.map List.reverse
        |> Util.transpose
        |> List.map (scoreScenicToRight scenicUpdateC)
        --|> Debug.log "transposed"
        |> List.map List.reverse
        |> List.map (scoreScenicToRight scenicUpdateD)


calculateScenicScore : Tree Scenic -> Int
calculateScenicScore ( { a, b, c, d }, _ ) =
    a * b * c * d



{---------------------------------------- ----------------------------------------}


part1 : String -> String
part1 input =
    parse input initTreeBool
        |> boolQuadcopter scoreVisibleFromLeft
        |> countVisible
        |> Debug.toString


scoreVisibleFromLeft : List (Tree Bool) -> List (Tree Bool)
scoreVisibleFromLeft input =
    let
        step : Tree Bool -> List (Tree Bool) -> List (Tree Bool)
        step ( visible, height ) acc =
            ( visible
                || height
                > (List.map Tuple.second acc
                    |> List.maximum
                    |> Maybe.withDefault -1
                  )
            , height
            )
                :: acc
    in
    List.foldl step [] input
        |> List.reverse


{-| boolQuadcopter: compute visibility from every angle
-}
boolQuadcopter : (List (Tree a) -> List (Tree a)) -> Forest a -> Forest a
boolQuadcopter scoreFunc forest =
    List.map scoreFunc forest
        --|> Debug.log "init"
        |> List.map List.reverse
        |> List.map scoreFunc
        --|> Debug.log "reversed"
        |> List.map List.reverse
        |> Util.transpose
        |> List.map scoreFunc
        --|> Debug.log "transposed"
        |> List.map List.reverse
        |> List.map scoreFunc



--|> Debug.log "transposed reversed"


countVisible : Forest Bool -> Int
countVisible =
    List.concat >> List.filter Tuple.first >> List.length


type alias Forest a =
    List (List ( a, Int ))


type alias Tree a =
    ( a, Int )


initTreeBool : Int -> Tree Bool
initTreeBool int =
    ( False, int )


parse : String -> (Int -> Tree a) -> Forest a
parse input init =
    Util.lines input
        |> List.map
            (\line ->
                String.toList line
                    |> List.map String.fromChar
                    |> List.map String.toInt
                    |> Util.values
                    |> List.map init
            )
