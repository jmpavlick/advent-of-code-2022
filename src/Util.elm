module Util exposing (..)


lines : String -> List String
lines =
    String.split "\n"


words : String -> List String
words =
    String.split " "


values : List (Maybe a) -> List a
values =
    List.filterMap Basics.identity


tupleValues : ( Maybe a, Maybe a ) -> Maybe ( a, a )
tupleValues ( ma, mb ) =
    Maybe.map2 (\x y -> ( x, y )) ma mb


joinWords : List String -> String
joinWords =
    String.join " "


listToMaybe : List a -> Maybe a
listToMaybe list =
    case list of
        [ a ] ->
            Just a

        _ ->
            Nothing
