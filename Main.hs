module Main where

import Control.Monad (liftM2)
import Geometry (Point)

-- distanceBetweenPoints :: Point -> Point -> Float
-- distanceBetweenPoints = (sqrt .) . (sum .) . (map (^2) .) . zipWith (-)

-- length' :: Line -> Float
-- length' = liftM2 distanceBetweenPoints fst snd

-- perimeter :: Shape a => a -> Float
-- perimeter = sum . map length' . segments

main :: IO ()
main = print ((1, 2) :: Point)

-- main :: IO ()
-- main = print $ perimeter $ Triangle
--   { seg =
--     [
--       ([0, 0], [0, 4])
--     , ([0, 4], [4, 0])
--     , ([4, 0], [0, 0])
--     ]
--   }
