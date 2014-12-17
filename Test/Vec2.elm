module Test.Vec2 where

import Unit (..)
import Util.Function (..)
import Vec2

vectorSuite : Suite
vectorSuite =
  [ ("Vector Arithmetic",
    [ run ("sum", uncurry Vec2.sum,
        [ ((1, 2), (3, 4)) :- (4, 6)
        , ((-1, -2), (-3, -4)) :- (-4, -6)
        , ((-2, 3), (-4, 5)) :- (-6, 8) ])
    , run ("difference", uncurry Vec2.difference,
        [ ((1, 2), (3, 4)) :- (-2, -2)
        , ((-1, -2), (-3, -4)) :- (2, 2)
        , ((-2, 3), (-4, 5)) :- (2, -2) ]) 
    , run ("product", uncurry Vec2.product,
        [ ((1, 2), (3, 4)) :- (3, 8)
        , ((-1, -2), (-3, -4)) :- (3, 8)
        , ((-2, 3), (-4, 5)) :- (8, 15) ])
    , runWith vfequal ("quotient", uncurry Vec2.quotient,
        [ ((1, 2), (3, 4)) :- (1/3, 0.5)
        , ((-1, -2), (-3, -4)) :- (1/3, 0.5)
        , ((-2, 3), (-4, 5)) :- (0.5, 3/5) ])
    , run ("dot", uncurry Vec2.dot,
        [ ((1, 2), (3, 4)) :- 11
        , ((-1, -2), (-3, -4)) :- 11
        , ((-2, 3), (-4, 5)) :- 23 ])
    , run ("invert", Vec2.invert,
        [ (1, 1) :- (-1, -1)
        , (3, 4) :- (-3, -4)
        , (-5, 2) :- (5, -2)
        , (10, 3) :- (-10, -3)
        , Vec2.zero :- Vec2.zero ])
    , runWith vfequal ("reciprocate", Vec2.reciprocate,
        [ (1, 1) :- (1, 1)
        , (3, 4) :- (1/3, 1/4)
        , (2, -2) :- (1/2, -1/2)
        , (-3, -2) :- (-1/3, -1/2) ]) ])
  , ("Vector Scaling and Length",
    [ runWith fequal ("length", Vec2.length,
        [ (4, 3) :- 5
        , (1, 1) :- (sqrt 2)
        , (-1, 1) :- (sqrt 2)
        , (-4, -3) :- 5 ])
    , run ("lengthSquared", Vec2.lengthSquared,
        [ (4, 3) :- 25
        , (1, 1) :- 2
        , (-1, 1) :- 2
        , (-4, -3) :- 25 ])
    , run ("scale", uncurry Vec2.scale,
        [ (1, (2, 2)) :- (2, 2)
        , (2, (2, 2)) :- (4, 4)
        , (3, (3, 3)) :- (9, 9)
        , (0.5, (4, 4)) :- (2, 2) ])
    , run ("scaleInverse", uncurry Vec2.scaleInverse,
        [ (1, (2, 2)) :- (2, 2)
        , (2, (4, 2)) :- (2, 1)
        , (0.5, (4, 4)) :- (8, 8)
        , (4, (2, 6)) :- (0.5, 6 / 4) ])
    , runWith vfequal ("normalize", Vec2.normalize,
        [ (10, 0) :- (1, 0)
        , (0, 10) :- (0, 1)
        , (-10, 0) :- (-1, 0)
        , (0, -10) :- (0, -1)
        , (2, 2) :- (sqrt 0.5, sqrt 0.5) ])
    , runWith vfequal ("withLength", uncurry Vec2.withLength,
        [ ((0, 6), 2) :- (0, 2)
        , ((4, 0), 3) :- (3, 0)
        , ((3, 6), 1) :- Vec2.normalize (3, 6) ]) ])
  , ("Vector Rotation",
    [ runWith vfequal ("rotateLeft", Vec2.rotateLeft,
        [ (3, 4) :- (-4, 3)
        , (1, 0) :- (0, 1)
        , (1, 1) :- (-1, 1)
        , (-3, -4) :- (4, -3) ])
    , runWith vfequal ("rotateRight", Vec2.rotateRight,
        [ (3, 4) :- (4, -3)
        , (1, 0) :- (0, -1)
        , (1, 1) :- (1, -1)
        , (-3, -4) :- (-4, 3) ])
    , runWith vfequal ("rotate", uncurry Vec2.rotate,
        [ ((degrees 45), (sqrt 0.5, sqrt 0.5)) :- (0, 1)
        , ((degrees 180), (1, 1)) :- (-1, -1)
        , ((degrees -45), (sqrt 0.5, sqrt 0.5)) :- (1, 0)
        , ((degrees 45), (sqrt 2, sqrt 2)) :- (0, 2) ])
    , runWith vfequal ("rotateAround", uncurry3 Vec2.rotateAround,
        [ ((1, 1), (degrees 90), (3, 4)) :- (-2, 3) ]) 
    ]) ]

vfequal : Vec2.Vec2 -> Vec2.Vec2 -> Bool
vfequal (x, y) (x2, y2) = (fequal x x2) && (fequal y y2)

{-binaryScalarVectorToVector = runTestsWith vfequal ("Binary Scalar Vector to Vector Operations",
rotateAroundTests = runTestsWith vfequal ("Rotation Around Points",
  [ ("rotateAround", uncurry3 Vec2.rotateAround,
      [ ((1, 1), (degrees 90), (3, 4)) ],
      [ (-2, 3) ])
  ])

-}

