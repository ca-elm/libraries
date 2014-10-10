module Vec2 where

type Vec2 = (Float, Float)

zero2 = (0,0)

bin2 f (x,y) (x',y') = (f x x', f y y')
(@+) = bin2 (+)
(@-) = bin2 (-)
(@*) = bin2 (*)
(@/) = bin2 (/)
(x,y) @. (x',y') = x*x' + y*y'

scalei2 u (x, y) = (x/u, y/u)
scale2 u (x,y) = (x*u, y*u)

map2 f (x,y) = (f x, f y)
lensqr2 u = u @. u
len2 = sqrt << lensqr2
unit2 v = scalei2 (len2 v) v
withlen2 v l = unit2 v |> scale2 l

rotl2 (x,y) = (-y,x)
rotr2 (x,y) = (y,-x)
neg2 (x,y) = (-x,-y)
inv2 (x,y) = (1/x,1/y)
rot2 a (x,y) =
  let (c,s) = (cos a,sin a) in
  (c*x - s*y, s*x + c*y)

fliph2 (x,y) = (-x,y)
flipv2 (x,y) = (x,-y)

angle2 (x,y) = atan2 y x

arrow r p q =
  let d = p @- q |> unit2
      e = r `scale2` rot2 (degrees -45) d
      f = r `scale2` rot2 (degrees 45) d in
  path [p, q, q @+ e, q, q @+ f]
