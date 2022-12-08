module Day07 exposing (..)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser)
import Util exposing (Either(..))


eval : String -> String
eval input =
    Util.template part1 part2 input


part2 : String -> String
part2 input =
    let
        filesystem : Filesystem
        filesystem =
            runInputParser input
                |> (\ecos -> initFilesystem [] ecos Dict.empty)

        allDirectorySizes : List Int
        allDirectorySizes =
            sumAllDirectories filesystem

        outermostDirectorySize : Int
        outermostDirectorySize =
            getOutermostDirectorySize allDirectorySizes
                |> Maybe.withDefault 0

        currentFreeSpace : Int
        currentFreeSpace =
            totalFilesystemSize - outermostDirectorySize

        candidateDirectorySizes : List Int
        candidateDirectorySizes =
            List.filter (\size -> size >= neededFreeSpace - currentFreeSpace) allDirectorySizes

        smallestDirectorySize : Int
        smallestDirectorySize =
            List.sort candidateDirectorySizes |> List.head |> Maybe.withDefault 0
    in
    smallestDirectorySize |> Debug.toString


getOutermostDirectorySize : List Int -> Maybe Int
getOutermostDirectorySize directories =
    List.sort directories |> List.reverse |> List.head


totalFilesystemSize : Int
totalFilesystemSize =
    70000000


neededFreeSpace : Int
neededFreeSpace =
    30000000



{---------------------------------------- ----------------------------------------}


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
            let
                step : Filesystem -> String -> Int -> Int
                step fs k acc =
                    acc + sumDirectory (k ++ "/" ++ key) fs
            in
            List.foldl (step filesystem) (List.sum files) directories


sumAllDirectories : Filesystem -> List Int
sumAllDirectories filesystem =
    Dict.keys filesystem
        |> List.map (\key -> sumDirectory key filesystem)
