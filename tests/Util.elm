module Util exposing (sanitize)


sanitize : String -> String
sanitize =
    String.dropLeft 1
        >> String.lines
        >> (\xs -> List.take (List.length xs - 1) xs)
        >> List.map String.trimLeft
        >> String.join "\n"
