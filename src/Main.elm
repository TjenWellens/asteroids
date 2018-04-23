module Main exposing (main)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Keyboard
import Views.Clock exposing (clock)



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL
type alias Bullet =
    { x: Int
    , y: Int
    , dx: Int
    , dy: Int
    , range: Int
    }

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
        bullet = Bullet 50 40 0 -1 10
    in
        { model | bullets = bullet :: model.bullets }

updateBullets: Model -> Model
updateBullets model =
    let
        bullets = model.bullets
            |> List.map updateBullet
            |> List.filter (\b -> b.range > 0)
    in
        { model | bullets = bullets }

updateBullet: Bullet -> Bullet
updateBullet bullet =
    let
        dx = bullet.dx
        dy = bullet.dy
        x = bullet.x + dx
        y = bullet.y + dy
        range = bullet.range - 1
    in
        Bullet x y dx dy range


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
  let
    angle =
      turns (Time.inMinutes model.time)

    handX =
      toString (50 + 40 * cos angle)

    handY =
      toString (50 + 40 * sin angle)
  in
    svg [ viewBox "0 0 100 100", width "300px" ]
        (
        []
        ++ clock model.time
        ++ spaceShuttle model
        ++ bullets model
        )


spaceShuttle model =
    [ polygon [ points "50,44 47,53 53,53", stroke "#FFFFFF" ] [] ]

bullets : Model -> List (Svg msg)
bullets model =
    List.map bullet model.bullets

bullet : Bullet -> Svg msg
bullet bullet =
    let
        x_1 = toString bullet.x
        y_1 = toString bullet.y
        x_2 = toString (bullet.x - bullet.dx)
        y_2 = toString (bullet.y - bullet.dy)
    in
        line [x1 x_1, y1 y_1, x2 x_2, y2 y_2, stroke "#FFFFFF"] []