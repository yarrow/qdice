module App exposing (Model, Msg(..), initialModel, main, update, view)

import Browser
import Dice exposing (DiceBoard(..), DiceList)
import Die exposing (Die)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random


type Msg
    = RollDice
    | GotDice DiceList
    | DieFlipped Int


rerollCount : Model -> Int
rerollCount model =
    case model.dice of
        Nothing ->
            5

        Just dice ->
            Dice.rerollCount dice


rollAllowed : Model -> Bool
rollAllowed model =
    model.remainingRolls > 0 && Maybe.withDefault False (Maybe.map Dice.hasRerolls model.dice)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RollDice ->
            let
                cmd =
                    if rollAllowed model then
                        Random.generate GotDice (Dice.diceRoller (rerollCount model))

                    else
                        Cmd.none
            in
            ( model, cmd )

        GotDice incomingDice ->
            let
                newModel =
                    { model
                        | dice = Dice.mergeDice incomingDice model.dice
                        , remainingRolls = model.remainingRolls - 1
                    }
            in
            ( newModel, Cmd.none )

        DieFlipped j ->
            let
                newDice =
                    case model.dice of
                        Nothing ->
                            Nothing

                        Just dice ->
                            Just (Dice.flipNth j dice)
            in
            ( { model | dice = newDice }, Cmd.none )


tdDie : Die -> Html msg
tdDie d =
    td [ class "dice" ] [ img [ src (Die.url d) ] [] ]


dieRow : Int -> Die -> Html Msg
dieRow j d =
    tr [ class "dice-row", onClick (DieFlipped j) ] <|
        case Die.nextRoll d of
            Die.Keep ->
                [ tdBlank, tdDie d ]

            Die.Reroll ->
                [ tdDie d, tdBlank ]


tdBlank : Html msg
tdBlank =
    td [ class "dice" ] []


blankRow : Html msg
blankRow =
    tr [ class "dice-row" ] [ tdBlank, tdBlank ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Quarantine Dice" ]
        , table [ class "dice", id "dice" ] <|
            [ caption [] [ text (String.fromInt model.remainingRolls ++ " rolls remaining") ]
            , tr [] [ th [] [ text "Reroll" ], th [] [ text "Keep" ] ]
            ]
                ++ (case model.dice of
                        Nothing ->
                            List.repeat 5 blankRow

                        Just (DiceBoard theDice) ->
                            List.indexedMap dieRow theDice
                   )
        , button [ id "roll-dice", onClick RollDice ] [ text "Roll Dice" ]
        ]


type alias Model =
    { dice : Maybe DiceBoard
    , remainingRolls : Int
    }


initialModel : Model
initialModel =
    { dice = Nothing, remainingRolls = 3 }


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
