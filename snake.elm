module Main exposing (..)

import Html exposing (Html, Attribute, button, div, text, h2)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Keyboard exposing (KeyCode)
import Char
import Time
import Collage exposing (filled, square)
import Element
import Color exposing (black)


main = Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


-- Types & model


type alias Model = { snake : List ( Int, Int ), fruit : Maybe ( Int, Int ), lastDirection : InputEnum, lastTick : Maybe Time.Time }


type InputEnum
    = Up
    | Down
    | Left
    | Right
    | StartStop


type Msg
    = Input InputEnum
    | Tick Time.Time


init : ( Model, Cmd Msg )
init = ( Model [] Nothing StartStop Nothing, Cmd.none )


-- Updates and subscriptions


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
        Input input -> ( { model | lastDirection = input }, Cmd.none )

        Tick time -> ( { model | lastTick = Just time }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch
        [ Keyboard.downs parseKeyCode
        , Time.every Time.second Tick
        ]


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


-- Graphics


gameSize : Int
gameSize = 500


background : Collage.Form
background = filled black (square (toFloat gameSize))


canvas : model -> Element.Element
canvas model = Collage.collage gameSize gameSize [ background ]


-- View


view : Model -> Html Msg
view model = Html.body []
        [ h2 [] [ text "snake" ]
        , div [] [ text (toString model) ]
        , div [] [ Element.toHtml (canvas model) ]
        ]
