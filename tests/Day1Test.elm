module Day1Test exposing (..)

import Day1 exposing (calculate)
import Expect
import Test exposing (..)


suite : Test
suite =
    test "Example calculation result" <|
        \_ ->
            let
                input =
                    """
                    1abc2
                    pqr3stu8vwx
                    a1b2c3d4e5f
                    treb7uchet
                    """
            in
            Expect.equal (Ok 142) (calculate input)
