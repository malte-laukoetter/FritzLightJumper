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

-- We will probably build a whole new module out of this
-- 3 Elements visible at a time?
genVisAreas :: Time -> [Area]
genVisAreas time = [((xPos x, yPos x - size), (xPos x + size, yPos x)) | x <- [cX-2..cX+5]]
  where aX      | time >= 30 = getScrollAlpha $ time - 30
                | otherwise  = 0
        cX      = floor $ utiDiv aX 10
        maxGap  = 2
        size    = 2
        gap x'  | x' >= 5   = (x' `mod` ((x' `mod` maxGap)+1))
                | otherwise = 75 -- distance to first block
        xPos x' = x'*10 + (gap x') + 5
        yPos x' | ((sin (utiDiv (2*x') 2.34))-0.5) > 0.5 = 10-size
                | ((sin (utiDiv (2*x') 2.34))-0.5) > 0   = 12-size
                | otherwise                              = 12
        --bottom = [((0, 11), (150, 12))]
        --bottom = []
        --bottom = [((x', 11), (x',12)) | x' <- [aX..fst dim-1+aX], (sin $ utiDiv x' 10) > 0]