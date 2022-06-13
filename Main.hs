module Main where

import Geometry
  ( Circle (Circle),
    Cone (Cone),
    Cube (Cube),
    Cylinder (Cylinder),
    LineSegment (Length),
    Point,
    Shape (area, name, perimeter),
    Shape3d (volume),
    Sphere (Sphere),
    Square (Square),
    Tetrahedron (Tetrahedron),
    Triangle (Triangle),
    centerOfCircle,
    constructRightTriangleWithoutHype,
    createCylinderWithVolumeAndRadius,
    distanceBetweenPoints,
    findAnglesOfTriangle,
    isTriangleRight,
  )

main :: IO ()
main =
  do
    let p = (1, 2) :: Point
    let p' = (3, 4) :: Point

    putStrLn
      "The distance between p and p' is:"
      >> print (distanceBetweenPoints p p')

    let c = Circle (0, 0) 5

    putStrLn
      "\nThe distance between p and the center of c is:"
      >> print (distanceBetweenPoints p (centerOfCircle c))

    printShapeStats c

    let t = Triangle (Length 3) (Length 4) (Length 5)

    printShapeStats t

    putStrLn "\nt is a right triangle: "
      >> print (isTriangleRight t)

    putStrLn "\nThe angles of triangle t are: "
      >> print (findAnglesOfTriangle t)

    let t' = constructRightTriangleWithoutHype (Length 3) (Length 4)

    printShapeStats t'

    putStrLn "\nt' is a right triangle: "
      >> print (isTriangleRight t)

    putStrLn "\nThe angles of triangle t' are: "
      >> print (findAnglesOfTriangle t)

    let cylinder = Cylinder c 10

    printShapeStats cylinder
    printShape3dStats cylinder

    let cylinder' = createCylinderWithVolumeAndRadius 785.3982 5

    let (Cylinder _ height) = cylinder'

    putStrLn "\nThe height of cylinder' is:"
      >> print height

    let s = Square (Length 5) (Length 5) (Length 5) (Length 5)

    printShapeStats s

    let cube = Cube s s s s s s

    printShapeStats cube
    printShape3dStats cube

    let cone = Cone c 10

    printShapeStats cone
    printShape3dStats cone

    let sphere = Sphere c

    printShapeStats sphere
    printShape3dStats sphere

    let tt = Triangle (Length 3) (Length 3) (Length 3)
    let tetrahedron = Tetrahedron tt tt tt tt

    printShapeStats tetrahedron
    printShape3dStats tetrahedron

printShapeStats :: Shape a => a -> IO ()
printShapeStats s =
  putStrLn ("\nThe area of the " ++ name s ++ " is:")
    >> print (area s)
    >> putStrLn ("\nThe perimeter of the " ++ name s ++ " is:")
    >> print (perimeter s)

printShape3dStats :: Shape3d a => a -> IO ()
printShape3dStats s =
  putStrLn ("\nThe volume of the " ++ name s ++ " is:")
    >> print (volume s)
