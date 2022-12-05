module Day05 exposing (..)

import Array exposing (Array)
import Parser exposing ((|.), (|=), Parser)
import Util


eval : String -> String
eval input =
    Util.joinWords
        [ "part 1: "
        , part1 input
        , "; part 2: "
        , part2 input
        ]


part2 : String -> String
part2 input =
    "TODO"



{---------------------------------------- ----------------------------------------}


part1 : String -> String
part1 input =
    let
        lines : List String
        lines =
            Util.lines input

        pileOfCrates : PileOfCrates
        pileOfCrates =
            List.map parseCrateLine lines
                |> List.map Util.results
                |> List.filter (\list -> List.length list > 0)
                |> rotate
                |> List.map Util.values
                |> List.map Array.fromList
                |> Array.fromList

        instructions : List Instruction
        instructions =
            List.map parseInstruction lines
                |> Util.results

        testArray =
            Array.fromList [ Array.fromList [ "a", "b" ], Array.fromList [ "c", "d" ], Array.fromList [ "e", "f" ] ]

        testUpdateAtIndex =
            updateAtIndex 2 (Array.fromList [ "z" ]) testArray
    in
    List.foldl moveCrate pileOfCrates instructions
        |> getMessage


getMessage : PileOfCrates -> String
getMessage pile =
    Array.map Array.toList pile
        |> Array.toList
        |> List.map List.head
        |> Util.values
        |> String.concat
        |> Debug.toString


moveCrate : Instruction -> PileOfCrates -> PileOfCrates
moveCrate { move, from, to } pile =
    let
        sourceIndex : Int
        sourceIndex =
            from - 1

        targetIndex : Int
        targetIndex =
            to - 1
    in
    case Array.get sourceIndex pile of
        Nothing ->
            pile

        Just crates ->
            takeCrates move crates
                |> (\{ leftovers, flippedToMove } ->
                        updateAtIndex sourceIndex leftovers pile
                            |> (\newPile ->
                                    case Array.get targetIndex newPile of
                                        Nothing ->
                                            newPile

                                        Just newCrates ->
                                            updateAtIndex targetIndex (Array.append flippedToMove newCrates) newPile
                               )
                   )


takeCrates : Int -> Array String -> { leftovers : Array String, flippedToMove : Array String }
takeCrates count crates =
    { leftovers = Array.slice count (Array.length crates) crates
    , flippedToMove =
        if count == 0 then
            Array.empty

        else
            Array.slice 0 count crates |> Array.toList |> List.reverse |> Array.fromList
    }


updateAtIndex : Int -> Array String -> PileOfCrates -> PileOfCrates
updateAtIndex index newPile crates =
    let
        pilesBefore : PileOfCrates
        pilesBefore =
            if index == 0 then
                Array.empty

            else
                Array.slice 0 index crates

        pilesAfter : PileOfCrates
        pilesAfter =
            Array.slice (index + 1) (Array.length crates) crates
    in
    Array.append (Array.append pilesBefore (Array.fromList [ newPile ])) pilesAfter


type alias PileOfCrates =
    Array (Array String)


type alias Instruction =
    { move : Int
    , from : Int
    , to : Int
    }


type alias Crate =
    Maybe String


parseCrateLine : String -> List (Result (List Parser.DeadEnd) Crate)
parseCrateLine input =
    case String.toList input of
        a :: b :: c :: xs ->
            -- List.drop 1 xs removes the separating whitespace
            (String.fromList [ a, b, c ]
                |> Parser.run crateParser
            )
                :: parseCrateLine (List.drop 1 xs |> String.fromList)

        _ ->
            []


parseInstruction : String -> Result (List Parser.DeadEnd) Instruction
parseInstruction =
    Parser.run <|
        Parser.succeed Instruction
            |. Parser.chompWhile Char.isAlpha
            |. Parser.symbol " "
            |= Parser.int
            |. Parser.symbol " "
            |. Parser.chompWhile Char.isAlpha
            |. Parser.symbol " "
            |= Parser.int
            |. Parser.symbol " "
            |. Parser.chompWhile Char.isAlpha
            |. Parser.symbol " "
            |= Parser.int


crateParser : Parser (Maybe String)
crateParser =
    Parser.oneOf
        [ Parser.succeed Nothing
            |. Parser.symbol " "
            |. Parser.symbol " "
            |. Parser.symbol " "
        , (Parser.getChompedString
            >> Parser.map
                (String.toList
                    >> List.filter Char.isAlpha
                    >> String.fromList
                    >> Just
                )
          )
          <|
            Parser.succeed ()
                |. Parser.symbol "["
                |. Parser.chompIf Char.isAlpha
                |. Parser.symbol "]"
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
