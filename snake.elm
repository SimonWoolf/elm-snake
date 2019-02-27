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

{-| Point of having little and big ticks is so we can respond immediately when
a button is pressed, to feel responsive, but still wait ~bigTickMs before the
move after that
-}
bigTickMilliseconds : Int
bigTickMilliseconds = 100

littleTickMilliseconds : Int
littleTickMilliseconds = 10

littleTicksPerBigTick : Int
littleTicksPerBigTick = bigTickMilliseconds // littleTickMilliseconds


-- Types & model

type alias Coord = ( Int, Int )

{-| The head of the list is the head of the snake
-}
type alias Snake = List Coord

type alias UserId = String

type alias Model = { snake : Snake, fruit : Coord, lastDirection : Direction, lastTick : Maybe Time.Time, paused : Bool, gameOver : Bool, instructions : Maybe String, littleTickCounter : Int, userId : UserId }

type alias Event = { userId : UserId, time : Maybe Time.Time, action : String, coord : Maybe Coord, direction : Maybe String }

type Action
    = NewUserAction
    | DirectionAction Direction
    | DeathAction
    | NewFruitAction Coord

type Direction
    = Up
    | Down
    | Left
    | Right

type Msg
    = Direction Direction
    | RawDirection String
    | StartStop
    | LittleTick Time.Time
    | NewFruitPosition Coord
    | NewUserId UserId

init : ( Model, Cmd Msg )
init = ( { snake = [ ( 0, 0 ) ]
      , fruit = ( 0, 0 )
      , lastDirection = Right
      , lastTick = Nothing
      , paused = True
      , gameOver = False
      , instructions = Just "Press space to start\nwasd/arrows/hjkl to move"
      , littleTickCounter = 0
      , userId = ""
      }
    , Cmd.batch [ newFruitCmd, generateUserId ]
    )

makeAndSendEvent : UserId -> Action -> Maybe Time.Time -> Cmd msg
makeAndSendEvent userId action time = (case action of
        NewUserAction -> Event userId time "newUser" Nothing Nothing

        DirectionAction direction -> Event userId time "changeDirection" Nothing (Just (toString direction))

        DeathAction -> Event userId time "death" Nothing Nothing

        NewFruitAction coord -> Event userId time "newFruit" (Just coord) Nothing
    )
        |> sendEvent


-- Updates and subscriptions

port rawInput : (String -> msg) -> Sub msg

port sendEvent : Event -> Cmd msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
        StartStop -> case ( model.gameOver, model.paused ) of
                ( False, False ) -> ( { model | paused = True, instructions = Just "Paused\nPress space to unpause" }, Cmd.none )

                ( False, True ) -> ( { model | paused = False, instructions = Nothing }, Cmd.none )

                ( True, _ ) -> ( { model | paused = False, gameOver = False, snake = [ ( 0, 0 ) ], instructions = Nothing }, newFruitCmd )

        Direction direction -> if (doublingBack model.snake direction) || direction == model.lastDirection then
                ( model, Cmd.none )
            else
                onTick { model | littleTickCounter = 0, lastDirection = direction } |> broadcast (DirectionAction direction)

        RawDirection string -> updateRaw string model

        LittleTick time -> if model.littleTickCounter == littleTicksPerBigTick then
                onTick { model | littleTickCounter = 0 }
            else
                ( { model | littleTickCounter = model.littleTickCounter + 1 }, Cmd.none )

        NewFruitPosition coord -> updateFruitPos coord model

        NewUserId userId -> ( { model | userId = userId }, Cmd.none )

updateFruitPos : Coord -> Model -> ( Model, Cmd msg )
updateFruitPos coord model = if List.member coord model.snake then
        updateFruitPos (wrappedIncrement coord) model
    else
        ( { model | fruit = coord }, Cmd.none ) |> broadcast (NewFruitAction coord)

broadcast : Action -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
broadcast action ( model, cmd ) = ( model, Cmd.batch [ cmd, makeAndSendEvent "foo" action model.lastTick ] )

wrappedIncrement : Coord -> Coord
wrappedIncrement ( x, y ) = if x == maxCoord then
        wrap ( -maxCoord, y + 1 )
    else
        ( x + 1, y )

updateRaw : String -> Model -> ( Model, Cmd Msg )
updateRaw str model = let
        maybeMsg = case str of
                "Up" -> Just (Direction Up)

                "Down" -> Just (Direction Down)

                "Left" -> Just (Direction Left)

                "Right" -> Just (Direction Right)

                "StartStop" -> Just StartStop

                _ -> Nothing
    in
        case maybeMsg of
            Just msg -> update msg model

            Nothing -> ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch
        [ Keyboard.downs parseKeyCode
        , Time.every (10 * Time.millisecond) LittleTick
        , rawInput RawDirection
        ]

parseKeyCode : KeyCode -> Msg
parseKeyCode code = -- todo make it a maybe input, so can ignore if not recognised
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
        |> checkCollision

extendHead : Model -> Model
extendHead model = let
        newHead = case ( List.head model.snake, model.lastDirection ) of
                ( Just coord, direction ) -> Just (applyDirection coord direction)

                ( Nothing, _ ) -> Nothing
    in
        case newHead of
            Just coord -> { model | snake = (wrap coord) :: model.snake }

            Nothing -> model

doublingBack : Snake -> Direction -> Bool
doublingBack snake direction = case snake of
        head :: neck :: rest -> (applyDirection head direction) == neck

        _ -> False

applyDirection : Coord -> Direction -> Coord
applyDirection ( x, y ) direction = case direction of
        Up -> ( x, y + 1 )

        Down -> ( x, y - 1 )

        Left -> ( x - 1, y )

        Right -> ( x + 1, y )

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

generateUserId : Cmd Msg
generateUserId = Random.generate (NewUserId << String.fromList)
        ((Random.int 97 122)
            |> (Random.map Char.fromCode)
            |> (Random.list 6)
        )

checkCollision : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
checkCollision ( model, cmd ) = case model.snake of
        head :: tail -> if List.any (\a -> a == head) tail then
                ( { model | gameOver = True, paused = True, instructions = Just "Game over\nPress space to start again" }, Cmd.none ) |> broadcast DeathAction
            else
                ( model, cmd )

        _ -> ( model, cmd )


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

instructions : Model -> List Collage.Form
instructions model = case model.instructions of
        Nothing -> []

        Just str -> [ Text.fromString str
                |> Text.color Color.white
                |> Text.height 30
                |> Element.centered
                |> Element.width useableGameSize
                |> Collage.toForm
                |> move ( 0, toFloat (gameSize // 4) )

            --|> setPosition ()
            ]

canvas : Model -> Element.Element
canvas model = Collage.collage gameSize gameSize (background :: (cherry model) :: (snake model) ++ (instructions model))


-- View

view : Model -> Html Msg
view model = Html.body [ css [ margin (px 50) ] ]
        [ h1 [] [ text model.userId ]
        , div [] [ Element.toHtml (canvas model) |> Html.fromUnstyled ]
        ]
