module Main exposing (..)

import Html exposing (Html, Attribute, button, div, text, h2)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Keyboard exposing (KeyCode)
import Char
import Time
import Collage exposing (filled, square, move)
import Element
import Text
import Color exposing (black, rgb)
import Random

main = Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


-- constants

{-| Coordinate system: origin in the center, increasing up (y) and right (x).
Want snake to start in the center at 0,0, so make boardSize an odd number
-}
boardSize : Int
boardSize = 21

maxCoord : Int
maxCoord = (boardSize // 2)

bound : Int -> Int
bound x = clamp (negate maxCoord) maxCoord x

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

type alias Coord = ( Int, Int )

{-| The head of the list is the head of the snake
-}
type alias Snake = List Coord

type alias Model = { snake : Snake, fruit : Coord, lastDirection : Direction, lastTick : Maybe Time.Time, paused : Bool }

type Direction
    = Up
    | Down
    | Left
    | Right

type Msg
    = Direction Direction
    | StartStop
    | Tick Time.Time
    | NewFruitPosition Coord

init : ( Model, Cmd Msg )
init = ( { snake = [ ( 1, 1 ), ( 2, 2 ), ( 3, 3 ), ( 10, 10 ) ]
      , fruit = ( 0, 0 )
      , lastDirection = Right
      , lastTick = Nothing
      , paused = True
      }
    , Cmd.none
    )


-- Updates and subscriptions

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
        StartStop -> ( { model | paused = not model.paused }, Cmd.none )

        Direction direction -> ( { model | lastDirection = direction }, Cmd.none )

        Tick time -> onTick model

        NewFruitPosition coord -> ( { model | fruit = coord }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch
        [ Keyboard.downs parseKeyCode
        , Time.every (500 * Time.millisecond) Tick
        ]

parseKeyCode : KeyCode -> Msg
parseKeyCode code = -- todo make it a maybe input, so can ignore if not recognised
    let
        _ = Debug.log "code" code

        _ = Debug.log "char" (Char.fromCode code)
    in
        case ( code, Char.fromCode code ) of
            -- control
            ( _, ' ' ) -> StartStop

            ( _, 'P' ) -> StartStop

            -- Trad gaming directions
            ( _, 'W' ) -> Direction Up

            ( _, 'A' ) -> Direction Left

            ( _, 'S' ) -> Direction Down

            ( _, 'D' ) -> Direction Right

            -- Arrow keys
            ( 38, _ ) -> Direction Up

            ( 37, _ ) -> Direction Left

            ( 40, _ ) -> Direction Down

            ( 39, _ ) -> Direction Right

            -- Vim
            ( _, 'H' ) -> Direction Left

            ( _, 'J' ) -> Direction Down

            ( _, 'K' ) -> Direction Up

            ( _, 'L' ) -> Direction Right

            ( _, other ) -> Direction Up

onTick : Model -> ( Model, Cmd Msg )
onTick model = case model.paused of
        True -> ( model, Cmd.none )

        False -> advanceSnake model

advanceSnake : Model -> ( Model, Cmd Msg )
advanceSnake model = model
        |> extendHead
        |> retractTailUnlessFruit

extendHead : Model -> Model
extendHead model = let
        newHead = case ( List.head model.snake, model.lastDirection ) of
                ( Just ( x, y ), Up ) -> Just ( x, y + 1 )

                ( Just ( x, y ), Down ) -> Just ( x, y - 1 )

                ( Just ( x, y ), Left ) -> Just ( x - 1, y )

                ( Just ( x, y ), Right ) -> Just ( x + 1, y )

                ( Nothing, _ ) -> Nothing
    in
        case newHead of
            Just coord -> { model | snake = (wrap coord) :: model.snake }

            Nothing -> model

wrap : Coord -> Coord
wrap ( x, y ) = if abs x > maxCoord then
        ( negate (bound x), y )
    else if abs y > maxCoord then
        ( x, negate (bound y) )
    else
        ( x, y )

retractTailUnlessFruit : Model -> ( Model, Cmd Msg )
retractTailUnlessFruit model = if (List.head model.snake) == Just model.fruit then
        ( model, newFruitCmd )
    else
        ( { model | snake = (dropLast model.snake) }, Cmd.none )

dropLast : List a -> List a
dropLast list = list |> List.reverse |> List.drop 1 |> List.reverse

newFruitCmd : Cmd Msg
newFruitCmd = Random.generate NewFruitPosition
        (Random.pair
            (Random.int -maxCoord maxCoord)
            (Random.int -maxCoord maxCoord)
        )


-- Graphics

background : Collage.Form
background = filled black (square (toFloat gameSize))

setPosition : Coord -> Collage.Form -> Collage.Form
setPosition ( x, y ) = move ( (toFloat x) * snakePartSize, (toFloat y) * snakePartSize )

snakePart : ( Int, Int ) -> Collage.Form
snakePart coord = filled yellow (square snakePartVisibleSize)
        |> setPosition coord

snake : Model -> List Collage.Form
snake model = List.map snakePart model.snake

cherry : Model -> Collage.Form
cherry model = Text.fromString "🍒"
        |> Text.color Color.white
        |> Element.leftAligned
        |> Collage.toForm
        |> setPosition model.fruit
        -- get the emoji to visually sit squarely inside a square
        |> move ( -5, 7 )

canvas : Model -> Element.Element
canvas model = Collage.collage gameSize gameSize (background :: (cherry model) :: (snake model))


-- View

view : Model -> Html Msg
view model = Html.body []
        [ h2 [] [ text "snake" ]
        , div [] [ text (toString model) ]
        , div [] [ Element.toHtml (canvas model) ]
        ]
