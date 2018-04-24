module Main exposing (main)

import Data.Bullet as Bullet exposing (Bullet)
import Data.Position as Position exposing (Heading, Position, Velocity)
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Keyboard exposing (KeyCode)
import Views.Clock exposing (clock)
import Views.SpaceShuttle exposing (spaceShuttle)
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
    , bullets: List Bullet
    }


init : (Model, Cmd Msg)
init =
  (Model 0 [], Cmd.none)



-- UPDATE


type Msg
  = Tick Time
  | KeyDown KeyCode
  | FireBullet
  | UpdateBullets


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
        update UpdateBullets { model | time = newTime }

    KeyDown keycode -> keyDown keycode model

    FireBullet -> (fire model, Cmd.none)

    UpdateBullets -> (updateBullets model, Cmd.none)


keyDown: KeyCode -> Model -> (Model, Cmd Msg)
keyDown keyCode model =
    case keyCode of
    --  Space
        32 -> update FireBullet model

        _ -> (model, Cmd.none)

fire: Model -> Model
fire model =
    let
        bullet = Bullet (Position 50 40) (Heading 0 -1) 2 10
    in
        { model | bullets = bullet :: model.bullets }

updateBullets: Model -> Model
updateBullets model =
    { model | bullets = Bullet.updateAll model.bullets }


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
    svg [ viewBox "0 0 100 100", width "300px" ]
        (
        []
        ++ clock model.time
        ++ spaceShuttle model
        ++ bullets model.bullets
        )
