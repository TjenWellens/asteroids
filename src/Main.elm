module Main exposing (main)

import Data.Astroid as Astroid exposing (Astroid)
import Data.Bullet as Bullet exposing (Bullet)
import Data.Collision as Collision exposing (Collision)
import Data.Heading as Heading exposing (Heading)
import Data.Model as Model exposing (Model)
import Data.Rotation exposing (Rotation(..))
import Data.Momentum as Momentum exposing (Momentum)
import Data.Position as Position exposing (Position)
import Data.SpaceShuttle as SpaceShuttle exposing (Livelyness(..), SpaceShuttle)
import Data.Universe as Universe exposing (Universe)
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Keyboard exposing (KeyCode)
import Views.Astroid exposing (drawAstroid)
import Views.Clock exposing (clock)
import Views.SpaceShuttle exposing (drawSpaceShuttle)
import Views.Bullet exposing (bullet)



main =
  Html.program
    { init = (Model.init, Cmd.none)
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL
init : (Model, Cmd Msg)
init = (Model.init, Cmd.none)



-- UPDATE


type Msg
  = Tick Time
  | KeyDown KeyCode
  | FireBullet
  | RotateLeft
  | RotateRight
  | Thrust
  | NOOP


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime -> (tick newTime model, Cmd.none)

    KeyDown keycode -> update (keyDown keycode) model

    FireBullet -> (Model.fire model, Cmd.none)

    RotateLeft -> (Model.spaceShuttle (SpaceShuttle.rotate CounterClockwise) model, Cmd.none)
    RotateRight -> (Model.spaceShuttle (SpaceShuttle.rotate Clockwise) model, Cmd.none)
    Thrust -> (Model.spaceShuttle SpaceShuttle.thrust model, Cmd.none)

    NOOP -> (model, Cmd.none)

tick: Time -> Model -> Model
tick newTime model =
    if Model.gameOver model then
        model
            |> Model.bullets Bullet.move
            |> Model.filterLiveBullets
            |> Model.bullets (Universe.reappear model.universe)
    else
        model
            |> Model.tickTime newTime
            |> Model.bullets Bullet.move
            |> Model.filterLiveBullets
            |> Model.spaceShuttle SpaceShuttle.move
            |> Model.astroids Astroid.move
            |> Model.spaceShuttle (Universe.reappear model.universe)
            |> Model.spaceShuttle SpaceShuttle.accelerate
            |> Model.bullets (Universe.reappear model.universe)
            |> Model.astroids (Universe.reappear model.universe)
            |> Model.doCollision

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
        ++ List.map drawAstroid model.astroids
        ++ drawSpaceShuttle model.spaceShuttle
        ++ List.map bullet model.bullets
        )
