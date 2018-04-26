module Main exposing (main)

import Data.Astroid as Astroid exposing (Astroid)
import Data.Bullet as Bullet exposing (Bullet)
import Data.Collision exposing (Collision)
import Data.Heading as Heading exposing (Heading)
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
    , astroids: List Astroid
    }


init : (Model, Cmd Msg)
init =
  (Model 0
  (SpaceShuttle (Position 50 50) Momentum.none Momentum.none Heading.n Alive)
  []
  (Universe 200 200)
  [ Astroid (Position 20 20) (Momentum.toE) Astroid.Big
  , Astroid (Position 100 100) (Momentum.toS) Astroid.Medium
  , Astroid (Position 40 100) (Momentum.toW) Astroid.Small
  ]
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
        |> doas Astroid.move
        |> do (Universe.reappear model.universe)
        |> do SpaceShuttle.accelerate
        |> dobs (Universe.reappear model.universe)
        |> doas (Universe.reappear model.universe)
        |> doCollision

doCollision: Model -> Model
doCollision ({bullets, astroids, spaceShuttle} as model) =
    let
        newBullets = bullets
            |> List.filterMap (explodeBulletIfCollidesAstroids astroids)
        newAstroids = astroids
            |> List.concatMap (explodeAstroidIfCollidesBullets bullets)
            |> List.concatMap (explodeAstroidIfCollidesSpaceShuttles [spaceShuttle])
        newSpaceShuttle = spaceShuttle
            |> explodeSpaceShuttleIfCollidesAstroids astroids
    in
        {model|bullets=newBullets, astroids=newAstroids, spaceShuttle=newSpaceShuttle}

explodeIfCollides: (obstacle -> Collision) -> (b -> Collision -> Bool) -> (Bool -> b -> result) -> List obstacle -> b -> result
explodeIfCollides obstacleToCollision collide explode obstacles b =
    let
        shouldExplode = obstacles
            |> List.map obstacleToCollision
            |> List.any (collide b)
    in
        explode shouldExplode b

explodeBulletIfCollidesAstroids: List Astroid -> Bullet -> Maybe Bullet
explodeBulletIfCollidesAstroids = explodeIfCollides Astroid.toCollision Bullet.collides Bullet.explodeIf

explodeAstroidIfCollidesBullets: List Bullet -> Astroid -> List Astroid
explodeAstroidIfCollidesBullets = explodeIfCollides Bullet.toCollision Astroid.collides Astroid.explodeIf

explodeAstroidIfCollidesSpaceShuttles: List SpaceShuttle -> Astroid -> List Astroid
explodeAstroidIfCollidesSpaceShuttles = explodeIfCollides SpaceShuttle.toCollision Astroid.collides Astroid.explodeIf

explodeSpaceShuttleIfCollidesAstroids: List Astroid -> SpaceShuttle -> SpaceShuttle
explodeSpaceShuttleIfCollidesAstroids = explodeIfCollides Astroid.toCollision SpaceShuttle.collides SpaceShuttle.explodeIf

do: (SpaceShuttle -> SpaceShuttle) -> Model -> Model
do mapper model =
    {model|spaceShuttle = mapper model.spaceShuttle}

dobs: (Bullet -> Bullet) -> Model -> Model
dobs mapper model =
    {model|bullets = List.map mapper model.bullets}

doas: (Astroid -> Astroid) -> Model -> Model
doas mapper model =
    {model|astroids = List.map mapper model.astroids}

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
        ++ List.map drawAstroid model.astroids
        ++ drawSpaceShuttle model.spaceShuttle
        ++ List.map bullet model.bullets
        )
