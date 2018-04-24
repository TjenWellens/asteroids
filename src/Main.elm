module Main exposing (main)

import Data.Bullet as Bullet exposing (Bullet)
import Data.Position exposing (Heading, Position)
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Keyboard
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
  | KeyMsg Keyboard.KeyCode
  | FireBullet
  | UpdateBullets


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
        update UpdateBullets { model | time = newTime }

    KeyMsg keycode ->
        update FireBullet model

    FireBullet -> (fire model, Cmd.none)

    UpdateBullets -> (updateBullets model, Cmd.none)

fire: Model -> Model
fire model =
    let
        bullet = Bullet (Position 50 40) (Heading 0 -1) 10
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
        , Keyboard.downs KeyMsg
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
