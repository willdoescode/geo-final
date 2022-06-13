module Geometry
  ( Shape (..),
    Shape3d (..),
    Point (..),
    LineSegment (..),
    Line (..),
    Circle (..),
    Triangle (..),
    Square (..),
    Cylinder (..),
    Cube (..),
    Cone (..),
    Sphere (..),
    Tetrahedron (..),
    distanceBetweenPoints,
    lengthOfLineSegment,
    createCylinderWithVolumeAndRadius,
    triangleSides,
    constructRightTriangleWithoutHype,
    isTriangleRight,
    findAnglesOfTriangle,
  )
where

import Data.List

class Shape a where
  area :: a -> Float
  perimeter :: a -> Float

class Shape3d a where
  volume :: a -> Float

type Point = (Float, Float)

data LineSegment
  = Points
      (Point, Point)
  | Length Float
  deriving (Show, Eq)

data Line
  = Line
      Point -- Point
      (Float, Float) -- Slope
  deriving (Show, Eq)

data Circle
  = Circle
      Point -- Center
      Float -- Radius
  deriving (Show, Eq)

instance Shape Circle where
  area (Circle _ r) = pi * (r ^ 2)
  perimeter (Circle _ r) = 2 * pi * r

data Triangle
  = Triangle
      LineSegment -- a
      LineSegment -- b
      LineSegment -- c
  deriving (Show, Eq)

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

  perimeter (Triangle l1 l2 l3) =
    foldr ((+) . lengthOfLineSegment) 0 [l1, l2, l3]

data Square
  = Square
      LineSegment
      LineSegment
      LineSegment
      LineSegment
  deriving (Show, Eq)

instance Shape Square where
  area (Square l1 l2 _ _) =
    lengthOfLineSegment l1 * lengthOfLineSegment l2
  perimeter (Square l1 l2 l3 l4) =
    foldr
      ( (+) . lengthOfLineSegment
      )
      0
      [l1, l2, l3, l4]

data Cylinder
  = Cylinder
      Circle -- Circle
      Float -- Height
  deriving (Show, Eq)

instance Shape Cylinder where
  area (Cylinder (Circle _ r) h) =
    2 * pi * r * h + 2 * pi * r ^ 2
  perimeter (Cylinder (Circle _ r) h) =
    2 * pi * r * h

instance Shape3d Cylinder where
  volume (Cylinder (Circle _ r) h) =
    pi * r ^ 2 * h

data Cube
  = Cube
      Square
      Square
      Square
      Square
      Square
      Square
  deriving (Show, Eq)

instance Shape Cube where
  area (Cube s1 s2 s3 s4 s5 s6) =
    foldr
      ((+) . area)
      0
      [s1, s2, s3, s4, s5, s6]
  perimeter (Cube (Square s1 _ _ _) _ _ _ _ _) =
    12 * lengthOfLineSegment s1

instance Shape3d Cube where
  volume (Cube (Square s1 _ _ _) _ _ _ _ _) =
    lengthOfLineSegment s1 ^ 3

data Cone
  = Cone
      Circle -- Base
      Float -- Height
  deriving (Show, Eq)

instance Shape Cone where
  area (Cone (Circle _ r) h) =
    pi * r * (r + sqrt (h ^ 2 + r ^ 2))
  perimeter (Cone (Circle _ r) h) =
    2 * pi * r * (r + sqrt (h ^ 2 + r ^ 2))

instance Shape3d Cone where
  volume (Cone (Circle _ r) h) =
    (1 / 3) * pi * r ^ 2 * h

data Sphere
  = Sphere
      Circle -- Circle
      Float -- Radius
  deriving (Show, Eq)

instance Shape Sphere where
  area (Sphere (Circle _ r) _) =
    4 * pi * r ^ 2

  perimeter (Sphere (Circle _ r) _) =
    2 * pi * r

instance Shape3d Sphere where
  volume (Sphere (Circle _ r) _) =
    (4 / 3) * pi * r ^ 3

data Tetrahedron
  = Tetrahedron
      Triangle -- Base
      Triangle -- Top
      Triangle -- Side
      Triangle -- Side
  deriving (Show, Eq)

instance Shape Tetrahedron where
  area (Tetrahedron t1 t2 t3 t4) =
    foldr
      ((+) . area)
      0
      [t1, t2, t3, t4]

  perimeter (Tetrahedron t1 t2 t3 t4) =
    foldr
      ( (+) . perimeter
      )
      0
      [t1, t2, t3, t4]

instance Shape3d Tetrahedron where
  volume (Tetrahedron t1 _ _ _) =
    -- a ^ 3 / 6 * sqrt(2)
    ( lengthOfLineSegment
        (head (triangleSides t1))
        ^ 3
    )
      / (6 * sqrt 2)

distanceBetweenPoints ::
  Point ->
  Point ->
  Float
distanceBetweenPoints (x1, y1) (x2, y2) =
  sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

lengthOfLineSegment ::
  LineSegment -> -- Line
  Float -- Length
lengthOfLineSegment (Points (p1, p2)) =
  distanceBetweenPoints p1 p2
lengthOfLineSegment (Length l) = l

createCylinderWithVolumeAndRadius ::
  Float -> -- Volume
  Float -> -- Radius
  Cylinder
createCylinderWithVolumeAndRadius volume radius =
  Cylinder
    (Circle (0, 0) radius)
    (volume / (pi * radius ^ 2))

triangleSides ::
  Triangle ->
  [LineSegment]
triangleSides (Triangle s1 _ _) = [s1]

constructRightTriangleWithoutHype ::
  LineSegment -> -- Side a
  LineSegment -> -- Side b
  Triangle
constructRightTriangleWithoutHype s1 s2 =
  Triangle
    s1
    s2
    ( Length
        ( sqrt
            ( lengthOfLineSegment s1 ^ 2
                + lengthOfLineSegment s2 ^ 2
            )
        )
    )

isTriangleRight ::
  Triangle ->
  Bool
isTriangleRight (Triangle s1 s2 s3) =
  sqrt (head sides + (sides !! 1))
    == (sides !! 2)
  where
    -- sort sides to get hypotenuse just in case out of order
    sides =
      sort
        ( map
            ( (^ 2)
                . lengthOfLineSegment
            )
            [s1, s2, s3]
        )

findAnglesOfTriangle' ::
  Float -> -- a
  Float -> -- b
  Float -> -- c
  Float -- Angle
findAnglesOfTriangle' a b c =
  acos
    ( (b ^ 2 + c ^ 2 - a ^ 2)
        / (2 * b * c)
    )

findAnglesOfTriangle ::
  Triangle ->
  ( Float, -- Angle a
    Float, -- Angle b
    Float -- Angle c
  )
findAnglesOfTriangle (Triangle s1 s2 s3) =
  ( findAnglesOfTriangle' a b c,
    findAnglesOfTriangle' b c a,
    findAnglesOfTriangle' c a b
  )
  where
    [a, b, c] = map lengthOfLineSegment [s1, s2, s3]
