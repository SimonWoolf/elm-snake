port module Main exposing (..)

import Html.Styled as Html exposing (Html, Attribute, button, div, text, h1)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick, onInput)
import Keyboard exposing (KeyCode)
import Char
import Time
import Collage exposing (filled, square, move)
import Element
import Text
import Color exposing (black, rgb)
import Random
import Css exposing (margin, px)
import Maybe exposing (andThen)
import List exposing (head, tail, map, map2)


type Msg
    = Tick Time.Time


main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


type alias Model =
    { times : List Time.Time }


init : ( Model, Cmd Msg )
init =
    ( { times = [] }, Cmd.none )



-- Updates and subscriptions


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            ( { model | times = time :: model.times }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (100 * Time.millisecond) Tick
        ]



-- View


view : Model -> Html Msg
view model =
    Html.body [ css [ margin (px 50) ] ]
        [ h1 [] [ text "Snake" ]
        , div [] [ Maybe.map (map2 (\i j -> j - i) model.times) (tail model.times) |> toString |> text ]
        ]
