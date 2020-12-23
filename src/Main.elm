module Main exposing (Model, Msg(..), Page(..), main, update, view)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Game
import Html exposing (Html, a, footer, h1, li, nav, text, ul)
import Html.Attributes exposing (classList, href)
import Html.Lazy exposing (lazy)
import Url exposing (Url)


type alias Model =
    { page : Page }


type Page
    = GamePage Game.Model
    | NotFound


type Msg
    = GameMsg Game.Msg
    | ClickedLink Browser.UrlRequest
    | ChangedUrl Url


view : Model -> Document Msg
view model =
    let
        content =
            case model.page of
                GamePage game ->
                    Game.view game |> Html.map GameMsg

                NotFound ->
                    text "Not Found"
    in
    { title = "Quarantine Dice"
    , body =
        [ lazy viewHeader model.page
        , content
        ]
    }


viewHeader : Page -> Html Msg
viewHeader page =
    let
        logo =
            h1 [] [ text "Quarantine Dice" ]
    in
    nav [] [ logo ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msgIn model =
    case msgIn of
        ClickedLink _ ->
            ( model, Cmd.none )

        ChangedUrl _ ->
            ( model, Cmd.none )

        GameMsg msg ->
            case model.page of
                GamePage game ->
                    toGame model (Game.update msg game)

                _ ->
                    ( model, Cmd.none )


toGame : Model -> ( Game.Model, Cmd Game.Msg ) -> ( Model, Cmd Msg )
toGame model ( game, cmd ) =
    ( { model | page = GamePage game }
    , Cmd.map GameMsg cmd
    )


main : Program () Model Msg
main =
    Browser.application
        { init = \_ _ _ -> ( { page = GamePage Game.initialModel }, Cmd.none )
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }
