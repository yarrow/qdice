module AppTests exposing (..)

import App exposing (Model, Msg(..), initialModel, main, update, view)
import Dice
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Random
import Test exposing (..)


initialModelSuite : Test
initialModelSuite =
    describe "Properties of initial model" <|
        [ test "We start with no dice" <|
            \_ ->
                initialModel.dice
                    |> Expect.equal Nothing
        ]


dice_5 : List Dice.OneDie
dice_5 =
    Random.step Dice.fiveDice (Random.initialSeed 0) |> Tuple.first


withDice : Model
withDice =
    update (GotDice dice_5) initialModel |> Tuple.first


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
                update (GotDice dice_5) initialModel
                    |> Tuple.first
                    |> .dice
                    |> Expect.equal (Just dice_5)
        ]
