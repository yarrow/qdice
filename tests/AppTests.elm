module AppTests exposing (..)

import App exposing (Model, Msg(..), initialModel, main, update, view)
import Dice exposing (NextRoll(..))
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


randomDice : List Dice.OneDie
randomDice =
    Random.step Dice.fiveDice (Random.initialSeed 0) |> Tuple.first


modelWithDice : List Dice.OneDie -> Model
modelWithDice dice =
    update (GotDice dice) initialModel |> Tuple.first


modelWithRandomDice : Model
modelWithRandomDice =
    modelWithDice randomDice


diceOf : Model -> List Dice.OneDie
diceOf model =
    Maybe.withDefault [] model.dice


updateSuite : Test
updateSuite =
    describe "Properties of update" <|
        let
            keep =
                ( 1, Keep )

            reroll =
                ( 1, Reroll )
        in
        [ test "RollDice doesn't change the model" <|
            \_ ->
                update RollDice initialModel
                    |> Tuple.first
                    |> Expect.equal initialModel
        , test "In the initial model, (GotDice someDice) installs (Just someDice) as the model's .dice value" <|
            \_ ->
                diceOf modelWithRandomDice
                    |> Expect.equal randomDice
        , test "Initially, all dice have nextRoll == Keep" <|
            \_ ->
                List.all (\d -> Dice.nextRoll d == Keep) randomDice
                    |> Expect.true "Initially, all dice have an NextRoll of Keep"
        , test "`DieFlipped 0` causes the 0th die to flip its NextRoll status" <|
            \_ ->
                let
                    keepAll =
                        Dice.makeDice [ keep, keep, keep, keep, keep ]

                    rerollFirst =
                        Dice.makeDice [ reroll, keep, keep, keep, keep ]
                in
                update (DieFlipped 0) (modelWithDice keepAll)
                    |> Tuple.first
                    |> diceOf
                    |> Expect.equalLists rerollFirst
        , test "Incoming dice replace dice to be rerolled" <|
            \_ ->
                let
                    startingDice =
                        Dice.makeDice [ keep, reroll, keep, reroll, keep ]

                    incomingDice =
                        Dice.makeDice [ ( 2, Keep ), ( 3, Keep ) ]

                    resultingDice =
                        Dice.makeDice [ keep, ( 2, Keep ), keep, ( 3, Keep ), keep ]
                in
                update (GotDice incomingDice) (modelWithDice startingDice)
                    |> Tuple.first
                    |> diceOf
                    |> Expect.equalLists resultingDice
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
                view modelWithRandomDice
                    |> Query.fromHtml
                    |> Query.findAll [ tag "img" ]
                    |> Query.count (Expect.equal 5)
        , test "There are five dice rows" <|
            \_ ->
                view modelWithRandomDice
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
