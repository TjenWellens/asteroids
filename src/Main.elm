module Main exposing (main)

import Data.Bullet as Bullet exposing (Bullet)
import Data.Rotation exposing (Rotation(..))
import Data.Momentum as Momentum exposing (Momentum)
import Data.Position as Position exposing (Position)
import Data.SpaceShuttle as SpaceShuttle exposing (SpaceShuttle)
import Data.Universe as Universe exposing (Universe)
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Keyboard exposing (KeyCode)
import Views.Clock exposing (clock)
import Views.SpaceShuttle exposing (drawSpaceShuttle)
import Views.Bullet exposing (bullets)



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL
type alias Model =
    { time: Time
    , spaceShuttle: SpaceShuttle
    , bullets: List Bullet
    , universe: Universe
    }


init : (Model, Cmd Msg)
init =
  (Model 0
  (SpaceShuttle (Position 50 50) Momentum.none Momentum.none)
  []
  (Universe 200 200)
  , Cmd.none)



-- UPDATE


type Msg
  = Tick Time
  | KeyDown KeyCode
  | FireBullet
  | RotateLeft
  | RotateRight
  | Thrust
  | UpdateBullets
  | NOOP


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime -> (tick newTime model, Cmd.none)

    KeyDown keycode -> update (keyDown keycode) model

    FireBullet -> (fire model, Cmd.none)

    UpdateBullets -> (filterLiveBullets model, Cmd.none)

    RotateLeft -> (do (SpaceShuttle.rotate CounterClockwise) model, Cmd.none)
    RotateRight -> (do (SpaceShuttle.rotate Clockwise) model, Cmd.none)
    Thrust -> (do SpaceShuttle.thrust model, Cmd.none)

    NOOP -> (model, Cmd.none)

tick: Time -> Model -> Model
tick newTime model =
    model
        |> tickTime newTime
        |> dobs Bullet.move
        |> filterLiveBullets
        |> do SpaceShuttle.move
        |> do (Universe.reappear model.universe)
        |> do SpaceShuttle.accelerate
        |> dobs (Universe.reappear model.universe)

do: (SpaceShuttle -> SpaceShuttle) -> Model -> Model
do mapper model =
    {model|spaceShuttle = mapper model.spaceShuttle}

dobs: (Bullet -> Bullet) -> Model -> Model
dobs mapper model =
    {model|bullets = List.map mapper model.bullets}

moveSpaceShuttle: Model -> Model
moveSpaceShuttle model =
    {model | spaceShuttle = SpaceShuttle.move model.spaceShuttle}

tickTime: Time -> Model -> Model
tickTime newTime model =
    { model | time = newTime }

keyDown: KeyCode -> Msg
keyDown keyCode =
    case keyCode of
    --  Space
        32 -> FireBullet

    --  Arrow left
        37 -> RotateLeft

    --  Arrow up
        38 -> Thrust

    --  Arrow right
        39 -> RotateRight

        _ -> NOOP

fire: Model -> Model
fire model =
    let
        bullet = SpaceShuttle.fire model.spaceShuttle
    in
        { model | bullets = bullet :: model.bullets }

filterLiveBullets: Model -> Model
filterLiveBullets model =
    let
        bullets = model.bullets
            |> List.filter Bullet.alive
    in
        { model | bullets = bullets }


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every second Tick
        , Keyboard.downs KeyDown
        ]



-- VIEW


view : Model -> Html Msg
view model =
    svg [ viewBox "0 0 200 200", width "600px" ]
        (
        []
        ++ clock model.time
        ++ drawSpaceShuttle model.spaceShuttle
        ++ bullets model.bullets
        )
