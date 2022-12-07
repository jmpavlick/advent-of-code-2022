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
        |> (\ecos -> initFilesystem [] ecos Dict.empty)
        |> sumAllDirectories
        |> List.filter (\x -> x <= 100000)
        |> List.sum
        |> Debug.toString


type Command
    = CdIn String
    | CdOut
    | Ls


type Output
    = File Int
    | Directory String


runInputParser : String -> List (Either Command Output)
runInputParser =
    Util.lines >> List.map (Parser.run parseInput) >> Util.results


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
        , Parser.map (\x -> String.replace "$ cd " "" x |> CdIn) <|
            Parser.getChompedString <|
                Parser.succeed ()
                    |. Parser.keyword "$ cd"
                    |. Parser.spaces
                    |. Parser.chompWhile Char.isAlpha
        ]


parseOutput : Parser Output
parseOutput =
    Parser.oneOf
        [ Parser.map (\x -> String.replace "dir " "" x |> Directory) <|
            Parser.getChompedString <|
                Parser.succeed ()
                    |. Parser.keyword "dir"
                    |. Parser.spaces
                    |. Parser.chompWhile Char.isAlpha
        , Parser.succeed File
            |= Parser.int
        ]


type alias Filesystem =
    Dict String ( List String, List Int )


initFilesystem : List String -> List (Either Command Output) -> Filesystem -> Filesystem
initFilesystem cwd eco filesystem =
    let
        key : String
        key =
            String.join "/" cwd
    in
    case eco of
        [] ->
            filesystem

        x :: xs ->
            case x of
                Left command ->
                    case command of
                        CdIn str ->
                            initFilesystem (str :: cwd) xs filesystem

                        CdOut ->
                            initFilesystem (List.drop 1 cwd) xs filesystem

                        Ls ->
                            initFilesystem cwd xs filesystem

                Right output ->
                    case output of
                        File filesize ->
                            addFileAtKey filesize key filesystem
                                |> initFilesystem cwd xs

                        Directory directory ->
                            addDirectoryAtKey directory key filesystem
                                |> initFilesystem cwd xs


addFileAtKey : Int -> String -> Filesystem -> Filesystem
addFileAtKey filesize key filesystem =
    Dict.get key filesystem
        |> Maybe.map (\( directories, filesizes ) -> ( directories, filesize :: filesizes ))
        |> Maybe.withDefault ( [], List.singleton filesize )
        |> (\value -> Dict.insert key value filesystem)


addDirectoryAtKey : String -> String -> Filesystem -> Filesystem
addDirectoryAtKey directory key filesystem =
    Dict.get key filesystem
        |> Maybe.map (\( directories, filesizes ) -> ( directory :: directories, filesizes ))
        |> Maybe.withDefault ( List.singleton directory, [] )
        |> (\value -> Dict.insert key value filesystem)


sumDirectory : String -> Filesystem -> Int
sumDirectory key filesystem =
    case Dict.get key filesystem |> Maybe.withDefault ( [], [] ) of
        ( [], files ) ->
            Util.sum files

        ( directories, files ) ->
            Util.sum files
                + (List.map
                    (\d -> sumDirectory (d ++ "/" ++ key) filesystem)
                    directories
                    |> Util.sum
                  )


sumAllDirectories : Filesystem -> List Int
sumAllDirectories filesystem =
    Dict.keys filesystem
        |> List.map (\key -> sumDirectory key filesystem)
