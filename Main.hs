module Main where

import Geometry (Circle (Circle), LineSegment (Length), Point, Shape (area), Shape3d (volume), Triangle (Triangle), centerOfCircle, distanceBetweenPoints, findAnglesOfTriangle, isTriangleRight)

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
      "The distance between p and the center of c is:"
      >> print (distanceBetweenPoints p (centerOfCircle c))

    putStrLn "The area of c is:"
      >> print (area c)

    let t = Triangle (Length 3) (Length 4) (Length 5)

    putStrLn "The area of t is:"
      >> print (area t)

    putStrLn "t is a right triangle: "
      >> print (isTriangleRight t)

    putStrLn "The angles of triangle t are: "
      >> print (findAnglesOfTriangle t)
