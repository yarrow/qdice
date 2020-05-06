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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RollDice ->
            ( model, Random.generate GotDice (Dice.diceRoller 5) )

        GotDice dice ->
            ( { model | dice = Just dice }, Cmd.none )


rowWith : List (Html msg) -> Html msg
rowWith stuff =
    tr [ class "dice-row" ]
        [ td [ class "dice", align "center" ] stuff
        , td [ class "dice" ] []
        ]


rowWithDie : Dice.OneDie -> Html msg
rowWithDie d =
    rowWith [ img [ src (Dice.url d) ] [] ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Quarantine Dice" ]
        , table [ class "dice", id "dice" ] <|
            case model.dice of
                Nothing ->
                    List.repeat 5 (rowWith [])

                Just theDice ->
                    List.map rowWithDie theDice
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
