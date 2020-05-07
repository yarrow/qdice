module App exposing (Model, Msg(..), initialModel, main, update, view)

import Browser
import Dice
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random


type Msg
    = RollDice
    | GotDice (List Dice.OneDie)
    | DieFlipped Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RollDice ->
            ( model, Random.generate GotDice (Dice.diceRoller 5) )

        GotDice dice ->
            ( { model | dice = Just dice }, Cmd.none )

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


tdDie : Int -> Dice.OneDie -> Html msg
tdDie _ d =
    td [ class "dice" ] [ img [ src (Dice.url d) ] [] ]


dieRow : Int -> Dice.OneDie -> Html msg
dieRow j d =
    tr [ class "dice-row" ]
        [ tdBlank
        , tdDie j d
        ]


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
            case model.dice of
                Nothing ->
                    List.repeat 5 blankRow

                Just theDice ->
                    List.indexedMap dieRow theDice
        , button [ id "roll-dice", onClick RollDice ] [ text "Roll Dice" ]
        ]


type alias Model =
    { dice : Maybe (List Dice.OneDie)
    }


initialModel : Model
initialModel =
    { dice = Nothing }


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
