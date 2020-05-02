module DiceTests exposing (..)

import Dice exposing (oneDie, val)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


oneDieSuite : Test
oneDieSuite =
    describe "The OneDie type" <|
        [ test "Input values less than 1 become 1" <|
            \_ ->
                oneDie 0
                    |> val
                    |> Expect.equal 1
        , test "Input values greater than 6 become 6" <|
            \_ ->
                oneDie 7
                    |> val
                    |> Expect.equal 6
        , test "Die values 1 through 6 survive unchanged" <|
            \_ ->
                let
                    oneToSix =
                        List.range 1 6
                in
                List.map oneDie oneToSix
                    |> List.map val
                    |> Expect.equal oneToSix
        ]
