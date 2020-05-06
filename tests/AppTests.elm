module AppTests exposing (..)

import App exposing (Model, Msg(..), initialModel, main, update, view)
import Dice
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html.Attributes as Attr
import Random
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, id, tag, text)


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


diceOf : Model -> List Dice.OneDie
diceOf model =
    Maybe.withDefault [] model.dice


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
                diceOf withDice
                    |> Expect.equal dice_5
        , test "Initially, all dice have onRoll == Keep" <|
            \_ ->
                List.all (\d -> Dice.onRoll d == Dice.Keep) (diceOf withDice)
                    |> Expect.true "Initially, all dice have an OnRoll of Keep"
        ]


viewSuite : Test
viewSuite =
    describe "Properties of view" <|
        [ test "The app has a 'Roll Dice' button" <|
            \_ ->
                view initialModel
                    |> Query.fromHtml
                    |> Query.find [ tag "button", id "roll-dice" ]
                    |> Query.has [ text "Roll Dice" ]
        , test "We start with no dice" <|
            \_ ->
                view initialModel
                    |> Query.fromHtml
                    |> Query.findAll [ tag "img" ]
                    |> Query.count (Expect.equal 0)
        , test "When there are dice, there are five dice" <|
            \_ ->
                view withDice
                    |> Query.fromHtml
                    |> Query.findAll [ tag "img" ]
                    |> Query.count (Expect.equal 5)
        , test "There are five dice rows" <|
            \_ ->
                view withDice
                    |> Query.fromHtml
                    |> Query.findAll [ tag "tr", attribute <| Attr.class "dice-row" ]
                    |> Query.count (Expect.equal 5)
        , test "There are five dice rows in the initial Model too" <|
            \_ ->
                view initialModel
                    |> Query.fromHtml
                    |> Query.findAll [ tag "tr", attribute <| Attr.class "dice-row" ]
                    |> Query.count (Expect.equal 5)
        ]
