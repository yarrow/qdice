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


type alias DiceList =
    List Dice.OneDie


rerollCount : Model -> Int
rerollCount model =
    case model.dice of
        Nothing ->
            5

        Just dice ->
            List.length (List.filter (\die -> Dice.nextRoll die == Dice.Reroll) dice)


mergeDice : DiceList -> Maybe DiceList -> Maybe DiceList
mergeDice incoming current =
    case current of
        Nothing ->
            Just incoming

        Just oldDice ->
            Just (refreshDice incoming oldDice)


refreshDice : DiceList -> DiceList -> DiceList
refreshDice incoming current =
    case ( incoming, current ) of
        ( [], _ ) ->
            current

        ( _, [] ) ->
            []

        ( new :: tailIncoming, old :: tailCurrent ) ->
            case Dice.nextRoll old of
                Dice.Keep ->
                    old :: refreshDice incoming tailCurrent

                Dice.Reroll ->
                    new :: refreshDice tailIncoming tailCurrent


rollAllowed : Model -> Bool
rollAllowed model =
    model.remainingRolls > 0 && (rerollCount model > 0)


gotDiceIfRollAllowed : Model -> Cmd Msg
gotDiceIfRollAllowed model =
    if rollAllowed model then
        Random.generate GotDice (Dice.diceRoller (rerollCount model))

    else
        Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RollDice ->
            ( model, gotDiceIfRollAllowed model )

        GotDice incomingDice ->
            let
                newModel =
                    { model
                        | dice = mergeDice incomingDice model.dice
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


tdDie : Dice.OneDie -> Html msg
tdDie d =
    td [ class "dice" ] [ img [ src (Dice.url d) ] [] ]


dieRow : Int -> Dice.OneDie -> Html Msg
dieRow j d =
    tr [ class "dice-row", onClick (DieFlipped j) ] <|
        case Dice.nextRoll d of
            Dice.Keep ->
                [ tdBlank, tdDie d ]

            Dice.Reroll ->
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

                        Just theDice ->
                            List.indexedMap dieRow theDice
                   )
        , button [ id "roll-dice", onClick RollDice ] [ text "Roll Dice" ]
        ]


type alias Model =
    { dice : Maybe (List Dice.OneDie)
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
