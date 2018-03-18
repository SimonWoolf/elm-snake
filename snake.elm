module Main exposing (..)

import Html exposing (Html, Attribute, button, div, text, h2)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Keyboard exposing (KeyCode)
import Char


main = Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


-- Types & model


type alias Model = { snake : List ( Int, Int ), fruit : Maybe ( Int, Int ), lastDirection : InputEnum }


type InputEnum
    = Up
    | Down
    | Left
    | Right
    | StartStop


type Msg
    = Input InputEnum
    | Tick


init : ( Model, Cmd Msg )
init = ( Model [] Nothing StartStop, Cmd.none )


-- Updates and subscriptions


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
        Input input -> ( { model | lastDirection = input }, Cmd.none )

        Tick -> ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model = Keyboard.downs parseKeyCode


parseKeyCode : KeyCode -> Msg
parseKeyCode code = -- todo make it a maybe input, so can ignore if not recognised
    let
        _ = Debug.log "code" code

        _ = Debug.log "char" (Char.fromCode code)
    in
        case ( code, Char.fromCode code ) of
            -- Trad gaming directions
            ( _, 'W' ) -> Input Up

            ( _, 'A' ) -> Input Left

            ( _, 'S' ) -> Input Down

            ( _, 'D' ) -> Input Right

            -- Arrow keys
            ( 38, _ ) -> Input Up

            ( 37, _ ) -> Input Left

            ( 40, _ ) -> Input Down

            ( 39, _ ) -> Input Right

            -- Vim
            ( _, 'H' ) -> Input Left

            ( _, 'J' ) -> Input Down

            ( _, 'K' ) -> Input Up

            ( _, 'L' ) -> Input Right

            ( _, other ) -> Input Up


-- View


view : Model -> Html Msg
view model = Html.body []
        [ h2 [] [ text "snake" ]
        , div [] [ text (toString model) ]
        ]
