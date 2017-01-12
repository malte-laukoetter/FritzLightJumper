module LevelGen where
import Network.MateLight.Simple
import TypeDefs
import Utilities

-- A prebuild level
level :: [Area]
level = [
  ((10, 9), (13,12)),
  ((20, 9), (23,12)),
  ((30, 9), (33,12)),
  ((40, 9), (43,12)),
  ((50, 9), (53,12))]

-- CONTRACT
genVisAreas :: Time -> [Area]

-- PURPOSE
-- Generates a pseudo random area list where the player has to jump over

-- DEFINITION
genVisAreas time = [((xPos x, yPos x - size), (xPos x + size, yPos x)) | x <- [cX-2..cX+(5*maxBlo)]] ++ [bott x | x <- [cX-2..cX+(5*maxBlo)]]
  where aX      | time >= 30 = getScrollAlpha $ time - 30
                | otherwise  = 0
        cX      = floor $ utiDiv aX 10
        maxGap  | time < 2000 = 7
                | otherwise   = 3
        maxBlo  = (floor $ utiDiv time 2500) + 1 
        alpha   = floor $ utiDiv 10 (realToFrac maxBlo)
        size    = 2
        gap x'  | x' >= 5   = (x' `mod` ((x' `mod` maxGap)+1))
                | otherwise = 75 -- distance to first block
        xPos x' = x'*alpha + (gap x')
        yPos x' | ((tan (utiDiv (3*x') 7.23234))-0.4) > 0.2 = 9-size
                | ((tan (utiDiv (2*x') 3.34344))-0.5) > 0   = 12-size
                | otherwise                                 = 12
        bott x' | (sin $ utiDiv x' 10) > 0 || time < 40 || (tan $ utiDiv x' 24) > 0.1  = ((x'*alpha, 11), (x'*alpha+alpha, 12))
                | otherwise                                                            = ((0,0), (0,0))