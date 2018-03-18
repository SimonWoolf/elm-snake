module Main exposing (..)

import Html exposing (Html, Attribute, button, div, text, h2)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Keyboard exposing (KeyCode)
import Char
import Time
import Collage exposing (filled, square, move)
import Element
import Color exposing (black, rgb)

main = Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


-- constants
-- Coordinate system: origin in the center, increasing up (y) and right (x).
-- Want snake to start in the center at 0,0, so make boardSize an odd number

boardSize : Int
boardSize = 21

gameSize : Int
gameSize = 500

useableGameSize : Int
useableGameSize = 480

snakePartSize : Float
snakePartSize = (toFloat useableGameSize) / (toFloat boardSize)

snakePartVisibleSize : Float
snakePartVisibleSize = snakePartSize - 2

yellow : Color.Color
yellow = Color.rgb 249 161 30


-- Types & model

type alias Model = { snake : List ( Int, Int ), fruit : Maybe ( Int, Int ), lastDirection : InputEnum, lastTick : Maybe Time.Time, paused : Bool }

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
init = ( { snake = [ ( 1, 1 ), ( 2, 2 ), ( 3, 3 ), ( 10, 10 ) ]
      , fruit = Nothing
      , lastDirection = StartStop
      , lastTick = Nothing
      , paused = True
      }
    , Cmd.none
    )


-- Updates and subscriptions

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
        Input StartStop -> ( { model | paused = not model.paused }, Cmd.none )

        Input direction -> ( { model | lastDirection = direction }, Cmd.none )

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
            -- control
            ( _, ' ' ) -> Input StartStop

            ( _, 'P' ) -> Input StartStop

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

background : List Collage.Form
background = [ filled black (square (toFloat gameSize)) ]

setSnakePartPosition : Int -> Int -> Collage.Form -> Collage.Form
setSnakePartPosition x y = move ( (toFloat x) * snakePartSize, (toFloat y) * snakePartSize )

snakePart : ( Int, Int ) -> Collage.Form
snakePart ( x, y ) = filled yellow (square snakePartVisibleSize)
        |> setSnakePartPosition x y

snake : Model -> List Collage.Form
snake model = List.map snakePart model.snake

canvas : Model -> Element.Element
canvas model = Collage.collage gameSize gameSize (background ++ (snake model))


-- View

view : Model -> Html Msg
view model = Html.body []
        [ h2 [] [ text "snake" ]
        , div [] [ text (toString model) ]
        , div [] [ Element.toHtml (canvas model) ]
        ]
