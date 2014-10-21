module Physics where

import Vec2 (..)
import List (..)

{-| A library for basic 2D Physics

# Types
@docs Shape
@docs Collider

# Point Set Operations
@docs rotatePoints

# Creation and manipulation
@docs points, psides, aabbsides, createpoly, createAABB, createCircle, rotate

# Transforms

@docs translate

-}

rotatePoint : Vec2 -> Vec2 -> Float -> Vec2
rotatePoint (x, y) (x', y') angle =
    ( x' + (x - x') * (cos angle) - (y - y') * (sin angle)
    , y' + (x - x') * (sin angle) + (y - y') * (cos angle) )

rotatePoints : [Vec2] -> Vec2 -> Float -> [Vec2]
rotatePoints points center angle = map (\v -> rotatePoint v center angle) points

translatePoints : [Vec2] -> Vec2 -> [Vec2]
translatePoints points translation = map (\v -> v @+ translation) points

data Shape
    = Circle Float
    | AABB Vec2
    | Poly [Vec2]
    | InvalidStruct

{- Gets the absolute coordinates of each point on the shape assuming the first point lies as (0,0) -}
points : Shape -> [Vec2]
points s =
    case s of
        Circle r -> [(0, 0)]
        AABB (x, y) -> [(0, 0), (x, 0), (x, y), (0, y)]
        Poly sides -> scanl (@+) (0, 0) (take ((length sides) - 1) sides)
        InvalidStruct -> []

{- Gets a polygon given its points in order -}
psides : [Vec2] -> Shape
psides points = Poly ((tail (scanl (@-) (0, 0) (tail points))) ++ [((0, 0) @- (last points))])

{- Gets an AABB given its points with the bottom left corner being (0, 0) -}
aabbsides : [Vec2] -> Shape
aabbsides [_, _, extents, _] = AABB extents

{- Converts an AABB to a polygon -}
aabbToPoly : Shape -> Shape
aabbToPoly (AABB (x, y)) = Poly [(x, 0), (0, y), (-1 * x, 0), (0, -1 * y)]

{- Creates the specified Polygon or InvalidStruct if the shape can't be created -}
createPoly : [Vec2] -> Shape
createPoly sides = if ((foldl (@+) (0, 0) sides) /= (0, 0)) then InvalidStruct else (Poly sides)

createAABB : Vec2 -> Shape
createAABB sides = if sides == (0, 0) then InvalidStruct else (AABB sides)

createCircle : Float -> Shape
createCircle r = if r == 0 then InvalidStruct else (Circle (abs r))

{- Rotates the shape around the point relative to the starting point of the polygon (AABBs will give back their polygon equivalents rotated -}
rotateShape : Shape -> Vec2 -> Float -> Shape
rotateShape shape offset angle =
    let pset = (points shape) in
    case shape of
        Circle r -> Circle r
        AABB extents -> rotateShape (aabbToPoly (AABB extents)) offset angle
        Poly sides -> psides (translatePoints (rotatePoints (translatePoints pset offset) (0, 0) angle) (neg2 offset))
        InvalidStruct -> InvalidStruct

{- A collider made up of a location, an offset and a shape. The offset is the vector from the location to the point where the shape starts.
 -}
type Collider = (Vec2, Vec2, Shape)

translate : Collider -> Vec2 -> Collider
translate (location, offset, shape) v = ((location @+ v), offset, shape)

rotate : Collider -> Float -> Collider
rotate (location, offset, shape) angle = (location, (rotatePoint offset (0, 0) angle), (rotateShape shape offset angle))

