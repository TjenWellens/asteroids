module Main exposing (main)

import Data.Bullet as Bullet exposing (Bullet)
import Data.Position as Position exposing (Heading, Position, Velocity)
import Data.SpaceShuttle exposing (SpaceShuttle, gun)
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
    }


init : (Model, Cmd Msg)
init =
  (Model 0 (SpaceShuttle (Position 50 50) (Heading 0 0) 0) [], Cmd.none)



-- UPDATE


type Msg
  = Tick Time
  | KeyDown KeyCode
  | FireBullet
  | RotateLeft
  | MoveUp
  | RotateRight
  | MoveDown
  | UpdateBullets


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
        update UpdateBullets { model | time = newTime }

    KeyDown keycode -> keyDown keycode model

    FireBullet -> (fire model, Cmd.none)

    UpdateBullets -> (updateBullets model, Cmd.none)

    RotateLeft -> (rotate model (Heading -1 0), Cmd.none)
    MoveUp -> (rotate model (Heading 0 -1), Cmd.none)
    RotateRight -> (rotate model (Heading 1 0), Cmd.none)
    MoveDown -> (rotate model (Heading 0 1), Cmd.none)



rotate: Model -> Heading -> Model
rotate model heading =
    {model|spaceShuttle = (rotateSpaceShuttle model.spaceShuttle heading)}

rotateSpaceShuttle: SpaceShuttle -> Heading -> SpaceShuttle
rotateSpaceShuttle spaceShuttle heading =
    {spaceShuttle|heading=heading}

keyDown: KeyCode -> Model -> (Model, Cmd Msg)
keyDown keyCode model =
    case keyCode of
    --  Space
        32 -> update FireBullet model

    --  Arrow left
        37 -> update RotateLeft model

    --  Arrow up
        38 -> update MoveUp model

    --  Arrow right
        39 -> update RotateRight model

    --  Arrow down
        40 -> update MoveDown model

        _ -> (model, Cmd.none)

fire: Model -> Model
fire model =
    let
        position = gun model.spaceShuttle
        heading = model.spaceShuttle.heading
        bullet = Bullet position heading 2 10
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
    svg [ viewBox "0 0 200 200", width "600px" ]
        (
        []
        ++ clock model.time
        ++ drawSpaceShuttle model.spaceShuttle
        ++ bullets model.bullets
        )
