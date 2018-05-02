module Data.Model exposing (..)

import Data.Astroid as Astroid exposing (Astroid)
import Data.Bullet as Bullet exposing (Bullet)
import Data.Collision as Collision exposing (Collision)
import Data.Heading as Heading exposing (Heading)
import Data.Rotation exposing (Rotation(..))
import Data.Momentum as Momentum exposing (Momentum)
import Data.Position as Position exposing (Position)
import Data.SpaceShuttle as SpaceShuttle exposing (Livelyness(..), SpaceShuttle)
import Data.Universe as Universe exposing (Universe)
import Time exposing (Time, second)

type alias Model =
    { time: Time
    , spaceShuttle: SpaceShuttle
    , bullets: List Bullet
    , universe: Universe
    , astroids: List Astroid
    , actions: Actions
    }

type alias Actions =
    { thrusting: Bool
    , firing: Bool
    , rotating: Maybe Rotation
    }

thrusting: Bool -> Actions -> Actions
thrusting flag actions= {actions|thrusting = flag}

firing: Bool -> Actions -> Actions
firing flag actions = {actions|firing = flag}

rotating: Bool -> Rotation -> Actions -> Actions
rotating flag rotation actions =
    let
        newRotating =
            if flag then
                Just rotation
            else
                Nothing
    in
        {actions|rotating = newRotating}

updateActions: Model -> (Actions -> Actions) -> Model
updateActions model update = {model|actions=(update model.actions)}

thrustIfThrusting: Model -> Model
thrustIfThrusting model =
    if model.actions.thrusting then
        spaceShuttle SpaceShuttle.thrust model
    else
        model

fireIfFiring: Model -> Model
fireIfFiring model =
    if model.actions.firing then
        fire model
    else
        model

rotateIfRotating: Model -> Model
rotateIfRotating model =
    case model.actions.rotating of
        Nothing -> model
        Just rotation -> spaceShuttle (SpaceShuttle.rotate rotation) model

init =
    Model 0
      (SpaceShuttle (Position 50 50) Momentum.none Momentum.none Heading.n Alive)
      []
      (Universe 200 200)
      [ Astroid (Position 20 20) (Momentum.toE) Astroid.Big
      , Astroid (Position 100 100) (Momentum.toS) Astroid.Medium
      , Astroid (Position 40 100) (Momentum.toW) Astroid.Small
      ]
      (Actions False False Nothing)

gameOver model = model.spaceShuttle.livelyness == Dead || List.isEmpty model.astroids

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

explodeBulletIfCollidesAstroids: List Astroid -> Bullet -> Maybe Bullet
explodeBulletIfCollidesAstroids = Collision.explodeIfCollides Astroid.toCollision Bullet.collides Bullet.explodeIf

explodeAstroidIfCollidesBullets: List Bullet -> Astroid -> List Astroid
explodeAstroidIfCollidesBullets = Collision.explodeIfCollides Bullet.toCollision Astroid.collides Astroid.explodeIf

explodeAstroidIfCollidesSpaceShuttles: List SpaceShuttle -> Astroid -> List Astroid
explodeAstroidIfCollidesSpaceShuttles = Collision.explodeIfCollides SpaceShuttle.toCollision Astroid.collides Astroid.explodeIf

explodeSpaceShuttleIfCollidesAstroids: List Astroid -> SpaceShuttle -> SpaceShuttle
explodeSpaceShuttleIfCollidesAstroids = Collision.explodeIfCollides Astroid.toCollision SpaceShuttle.collides SpaceShuttle.explodeIf

moveSpaceShuttle: Model -> Model
moveSpaceShuttle model =
    {model | spaceShuttle = SpaceShuttle.move model.spaceShuttle}

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

spaceShuttle: (SpaceShuttle -> SpaceShuttle) -> Model -> Model
spaceShuttle mapper model =
    {model|spaceShuttle = mapper model.spaceShuttle}

bullets: (Bullet -> Bullet) -> Model -> Model
bullets mapper model =
    {model|bullets = List.map mapper model.bullets}

astroids: (Astroid -> Astroid) -> Model -> Model
astroids mapper model =
    {model|astroids = List.map mapper model.astroids}

tickTime: Time -> Model -> Model
tickTime newTime model =
    { model | time = newTime }
