module Day01 exposing (..)


eval : String -> String
eval input =
    parse input |> always "todo: finish"


type alias Elf =
    List Int


parse : String -> List Elf
parse input =
    String.split "\n" input
        |> Debug.log "split strings"
        |> always []
