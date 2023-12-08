module Puzzle exposing (Puzzle)

import Parser


type alias Puzzle =
    { validate : String -> Result String String
    , calculatePart1 : String -> Result String Int
    , calculatePart2 : String -> Result String Int
    }
