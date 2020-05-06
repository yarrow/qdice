module AppTests exposing (..)

-- import Random

import App exposing (Msg(..), initialModel, main, update, view)
import Dice exposing (oneDie)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


initialModelSuite : Test
initialModelSuite =
    describe "Properties of initial model" <|
        [ test "We start with no dice" <|
            \_ ->
                initialModel.dice
                    |> Expect.equal Nothing
        ]


updateSuite : Test
updateSuite =
    describe "Properties of update" <|
        [ test "RollDice doesn't change the model" <|
            \_ ->
                update RollDice initialModel
                    |> Tuple.first
                    |> Expect.equal initialModel
        , test "(GotDice someDice) installs (Just someDice) as the model's .dice value" <|
            \_ ->
                let
                    someDice =
                        [ oneDie 1, oneDie 4, oneDie 6 ]
                in
                update (GotDice someDice) initialModel
                    |> Tuple.first
                    |> .dice
                    |> Expect.equal (Just someDice)
        ]
