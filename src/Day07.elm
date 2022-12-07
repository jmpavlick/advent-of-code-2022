module Day07 exposing (..)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser)
import Util exposing (Either(..))


eval : String -> String
eval input =
    Util.template part1 part2 input


part2 : String -> String
part2 input =
    "TODO"



{---------------------------------------- ----------------------------------------}
{-
   [x] define a type to hold our puzzle input
   [x] write a parser to output values of that type
   [x] execute that parser over our input
   [x] define a type to hold the file and directory data as we process it -
       this is going to be some sort of bi-directional linked list or tree? maybe a stack?
   [ ] write a function that evaluates our input data
   [ ] write a function to search our input data
   [ ] search our input data for values that match our criteria
   [ ] sum them
   [ ] print

-}


part1 : String -> String
part1 input =
    runInputParser input
        |> List.foldl update ( [], Dict.empty )
        --|> Tuple.second
        -- >> sumAtAllPaths
        --|> List.filter (\x -> x < 10000)
        |> Debug.toString


type Command
    = CdIn String
    | CdOut
    | Ls


type Output
    = File Int
    | Directory String


type FsNode
    = F Int
    | D (List String) (List FsNode)


sumFsNode : FsNode -> Int
sumFsNode node =
    case node of
        F i ->
            i

        D _ [] ->
            0

        D path (x :: xs) ->
            sumFsNode x + sumFsNode (D path xs)


runInputParser : String -> List (Either Command Output)
runInputParser =
    Util.lines >> List.map (Parser.run parseInput) >> Util.results



{-
   group : List String -> List (Either Command Output) -> List ()
   group path eco =
       case eco of
           [] ->
               Debug.todo ""

           x :: xs ->
               case x of
                   Left command ->
                       case command of
                           CdIn dir ->
                               group (dir :: path) xs

                           Ls ->
                               group path xs

                   Right output ->
                       Debug.todo ""
-}


parseInput : Parser (Either Command Output)
parseInput =
    Util.parseEither parseCommand parseOutput


parseCommand : Parser Command
parseCommand =
    Parser.oneOf
        [ Parser.succeed CdOut
            |. Parser.keyword "$ cd .."
        , Parser.succeed Ls
            |. Parser.keyword "$ ls"
        , Parser.map CdIn <|
            Parser.getChompedString <|
                Parser.succeed ()
                    |. Parser.keyword "$ cd"
                    |. Parser.spaces
                    |. Parser.chompWhile Char.isAlpha
        ]


parseOutput : Parser Output
parseOutput =
    Parser.oneOf
        [ Parser.map Directory <|
            Parser.getChompedString <|
                Parser.succeed ()
                    |. Parser.keyword "dir"
                    |. Parser.spaces
                    |. Parser.chompWhile Char.isAlpha
        , Parser.succeed File
            |= Parser.int
        ]



---- i am ggoing nuts


type alias Filesystem =
    Dict String (List (Either String Int))


type alias DictNode =
    ( List String, Filesystem )


update : Either Command Output -> DictNode -> DictNode
update eco dictNode =
    case eco of
        Left command ->
            updateCommand command dictNode

        Right output ->
            updateOutput output dictNode


updateCommand : Command -> DictNode -> DictNode
updateCommand cmd ( cwd, filesystem ) =
    case cmd of
        CdIn dir ->
            ( dir :: cwd, filesystem )

        CdOut ->
            case cwd of
                [] ->
                    ( [], filesystem )

                x :: xs ->
                    ( xs, filesystem )

        Ls ->
            ( cwd, filesystem )


updateOutput : Output -> DictNode -> DictNode
updateOutput out ( cwd, filesystem ) =
    let
        path : String
        path =
            String.concat cwd

        values : List (Either String Int)
        values =
            Dict.get path filesystem
                |> Maybe.withDefault []

        mapOutput : Output -> Either String Int
        mapOutput o =
            case o of
                File i ->
                    Right i

                Directory d ->
                    Left d
    in
    ( cwd
    , Dict.insert path (mapOutput out :: values) filesystem
    )


sumAtPath : Filesystem -> String -> Int
sumAtPath filesystem path =
    case Dict.get path filesystem |> Maybe.withDefault [] of
        [] ->
            0

        x :: xs ->
            case x of
                Left dir ->
                    Dict.insert path xs filesystem
                        |> Util.flip sumAtPath (dir ++ path)

                Right i ->
                    i
                        + (Dict.insert path xs filesystem
                            |> Util.flip sumAtPath path
                          )


sumAtAllPaths : Filesystem -> List Int
sumAtAllPaths filesystem =
    Dict.keys filesystem
        |> List.map (sumAtPath filesystem)
