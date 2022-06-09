module Geometry where

type Point = (Float, Float)

type LineSegment = (Point, Point)

distanceBetweenPoints :: Point -> Point -> Float
distanceBetweenPoints (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

lengthOfLineSegment :: LineSegment -> Float
lengthOfLineSegment = uncurry distanceBetweenPoints

-- A point and a slope
data Line = Line Point (Float, Float)
  deriving (Show, Eq)

-- Center and Radius
data Circle = Circle Point Float
  deriving (Show, Eq)

-- a b c
data Triangle = Triangle LineSegment LineSegment LineSegment
  deriving (Show, Eq)

data Square = Square LineSegment LineSegment LineSegment LineSegment
  deriving (Show, Eq)

-- Circle and Height
data Cylinder = Cylinder Circle Float
  deriving (Show, Eq)

createCylinderWithVolumeAndRadius :: Float -> Float -> Cylinder
createCylinderWithVolumeAndRadius volume radius =
  Cylinder (Circle (0, 0) radius) (volume / (pi * radius ^ 2))

data Cube = Cube Square Square Square Square Square Square
  deriving (Show, Eq)

class Shape a where
  area :: a -> Float
  perimeter :: a -> Float

class Shape3d a where
  volume :: a -> Float

instance Shape Circle where
  area (Circle _ r) = pi * (r ^ 2)
  perimeter (Circle _ r) = 2 * pi * r

instance Shape Triangle where
  -- hernons traingle area formula
  area t@(Triangle s1 s2 s3) =
    sqrt
      ( semiPerimeter
          * (semiPerimeter - head lengths)
          * (semiPerimeter - lengths !! 1)
          * (semiPerimeter - lengths !! 2)
      )
    where
      semiPerimeter = perimeter t / 2
      lengths = map lengthOfLineSegment [s1, s2, s3]

  perimeter (Triangle l1 l2 l3) = foldr ((+) . lengthOfLineSegment) 0 [l1, l2, l3]

instance Shape Square where
  area (Square l1 l2 _ _) = lengthOfLineSegment l1 * lengthOfLineSegment l2
  perimeter (Square l1 l2 l3 l4) =
    foldr ((+) . lengthOfLineSegment) 0 [l1, l2, l3, l4]

instance Shape Cylinder where
  area (Cylinder (Circle _ r) h) = 2 * pi * r * h + 2 * pi * r ^ 2
  perimeter (Cylinder (Circle _ r) h) = 2 * pi * r * h

instance Shape Cube where
  area (Cube s1 s2 s3 s4 s5 s6) = foldr ((+) . area) 0 [s1, s2, s3, s4, s5, s6]
  perimeter (Cube (Square s1 _ _ _) _ _ _ _ _) = 12 * lengthOfLineSegment s1

instance Shape3d Cube where
  volume (Cube (Square s1 _ _ _) _ _ _ _ _) = lengthOfLineSegment s1 ^ 3

instance Shape3d Cylinder where
  volume (Cylinder (Circle _ r) h) = pi * r ^ 2 * h

triangleHeight :: Triangle -> Float
triangleHeight t@(Triangle s1 _ _) = area t / (lengthOfLineSegment s1 * 0.5)
