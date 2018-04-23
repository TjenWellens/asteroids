module Main exposing (main)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)



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
    }

type alias Model =
    { time: Time
    , bullets: List Bullet
    }


init : (Model, Cmd Msg)
init =
  (Model 0 [ Bullet 50 40 0 -1], Cmd.none)



-- UPDATE


type Msg
  = Tick Time
  | UpdateBullets


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ( { model
        | time = newTime
        , bullets = updateBullets model.bullets
        }
      , Cmd.none
      )

    UpdateBullets ->
      ({ model | bullets = updateBullets model.bullets }, Cmd.none)

updateBullets: List Bullet -> List Bullet
updateBullets bullets =
    List.map updateBullet bullets

updateBullet: Bullet -> Bullet
updateBullet bullet =
    let
        dx = bullet.dx
        dy = bullet.dy
        x = bullet.x + dx
        y = bullet.y + dy
    in
        Bullet x y dx dy


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick



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
        ++ clock model
        ++ spaceShuttle model
        ++ bullets model
        )

clock model =
    [ clockCircle model
    , clockHand model
    ]

clockCircle model =
    circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []

clockHand model =
    let
      angle =
        turns (Time.inMinutes model.time)

      handX =
        toString (50 + 40 * cos angle)

      handY =
        toString (50 + 40 * sin angle)
    in
      line [ x1 "50", y1 "50", x2 handX, y2 handY, stroke "#023963" ] []

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