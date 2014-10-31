module PShape where

{-| A library for the creation and manipulation of physics shapes.

#Types
@docs Shape

#Creation
@docs createCircle, createAABB, createPoly, aabbToPoly

#Conversion
@docs points, psides

#Manipulation
@docs rotate

-}

import Vec2 (zero, rotate, invert)
import Vec2.Infix (..)

-- #Types

{-| An abstract representation of a shape. Can be a circle, axis aligned
bounding box, arbitrary polygon, or an invalid structure. -}
data PShape
	= Circle Float  -- Circle
	| AABB Vec2     -- Axis Aligned Bounding Box
	| Poly [Vec2]   -- Arbitrary polygon

-- #Creation

{-| Creates a cirlce with radius r or InvalidStruct if the radius is <= 0. -}
createCircle : Float -> PShape
createCircle r = if | r > 0 -> Circle r

{-| Creates an AABB with extends described by a vector. -}
createAABB : Vec2 -> PShape
createAABB (x, y) = if | (x > 0 || y > 0) -> AABB (x, y)

{-| Creates an arbitrary polygon with sides described by a list of vectors. -}
createPoly : [Vec2] -> PShape
createPoly sides
	= if | foldl (!+) (0, 0) sides == (0, 0) -> Poly sides 

{-| Creates a polygon from an AABB. -}
aabbToPoly : Shape -> Shape
aabbToPoly (AABB (x, y)) = Poly [(x, 0), (0, y), (-1 * x, 0), (0, -1 * y)]

-- #Conversion

{-| Gets the absolute coordinates of each point on a shape assuming the origin
point is (0, 0). -}
points : Shape -> [Vec2]
points s =
	case s of
		Circle r -> [(0, 0)]
		AABB (x, y) -> [(0, 0), (x, 0), (x, y), (0, y)]
		Poly sides -> scanl (!+) (0, 0) (take ((length sides) - 1) sides)

{-| Creates a polygon given its points in order -}
psides : [Vec2] -> Shape
psides points
	= Poly ((tail (scanl (!-) (0, 0) (tail points))) ++ [((0, 0) !- (last points))])

-- #Manipulation

{-| Rotates a shape around a point relative to the origin (where the shape
starts at the origin) -}
rotateShape : Shape -> Vec2 -> Float -> Shape
rotateShape shape offset angle =
	let pset = points shape in
		tP points offset = map (\v -> v !+ offset) points
		rP a points = map (\p -> rotate a p) points
	case shape of
		Circle r -> Circle r
		AABB extents -> rotateShape (aabbToPoly shape) offset angle
		Poly sides -> psides (tP(rP (tP pset offset) (0, 0) angle) (invert offset))





