module Physics.PShape where

{-| A library for the creation and manipulation of physics shapes.

#Types
@docs PShape

#Creation
@docs pcircle, paabb, ppoly

#Conversion
@docs aabbToPoly, psides, sidesToPoly, toShape

#Manipulation
@docs rotatePShape

-}

import Vec2 (zero, rotateAround, invert, lengthSquared)
import Vec2.Infix (..)
import Util.List (mapnl, mapnr)

-- #Types

{-| An abstract representation of a shape. Can be a circle, axis aligned
bounding box, or an arbitrary polygon. PShapes all should start at (0, 0)
as they are an abstract representation of a shape, not a shape at a
particular location. -}
data PShape
  = Circle Float  -- Circle
  | AABB Vec2     -- Axis Aligned Bounding Box
  | Poly [Vec2]   -- Arbitrary polygon

{-| Creates a PShape of the type cirlce with radius r -}
pcircle : Float -> PShape
pcircle r = if | r > 0 -> Circle r

{-| Creates an AABB (axis aligned bounding box) with extents described by a vector. -}
paabb : Vec2 -> PShape
paabb (x, y) = if | (x > 0 || y > 0) -> AABB (x, y)

{-| Creates an arbitrary polygon from a list of its points -}
ppoly : [Vec2] -> PShape
ppoly points = Poly points

{-| Creates a polygon from an AABB. -}
aabbToPoly : PShape -> PShape
aabbToPoly (AABB (x, y)) = ppoly [(0, 0), (x, 0), (x, y), (0, y)]

{-| Gets a polygon's sides as a list of vectors -}
psides : PShape -> [Vec2]
psides (Poly points) = mapnr (!-) (head points) points

{-| Creates a polygon (starting at 0,0) from a list of vectors describing its sides -}
sidesToPoly : [Vec2] -> PShape
sidesToPoly sides
  = Poly (scanl (!+) (0, 0) sides |> (take ((length sides) - 1)))

{-| Rotates a shape around a point relative to the origin (where the shape
starts at the origin) -}
rotatePShape : PShape -> Vec2 -> Float -> PShape
rotatePShape shape offset angle
  = case shape of
    Circle r -> Circle r
    AABB extents -> rotatePShape (aabbToPoly shape) offset angle
    Poly points -> Poly (map (rotateAround offset angle) points)

toShape : PShape -> Shape
toShape shape =
  case shape of
    Circle r -> circle r
    AABB (width, height) -> rect width height
    Poly points -> polygon points





