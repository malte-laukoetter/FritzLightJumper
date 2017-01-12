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
genVisAreas time = [((xPos x, yPos x - size), (xPos x + size, yPos x)) | x <- [cX-2..cX+5]] ++ [bott x | x <- [cX-2..cX+5]]
  where aX      | time >= 30 = getScrollAlpha $ time - 30
                | otherwise  = 0
        cX      = floor $ utiDiv aX 10
        maxGap  | time < 2000 = 3
                | otherwise   = 1
        size    = 2
        gap x'  | x' >= 5   = (x' `mod` ((x' `mod` maxGap)+1)) - 1
                | otherwise = 75 -- distance to first block
        xPos x' = x'*10 + (gap x') + 5
        yPos x' | ((tan (utiDiv (3*x') 7.23234))-0.4) > 0.2 = 9-size
                | ((tan (utiDiv (2*x') 3.34344))-0.5) > 0   = 12-size
                | otherwise                                 = 12
        bott x' | (sin $ utiDiv x' 10) > 0 || time < 40 || (tan $ utiDiv x' 24) > 0.1  = ((x'*10, 11), (x'*10+10, 12))
                | otherwise                                                            = ((0,0), (0,0))
        --bottom = []
        --bottom = [((x', 11), (x',12)) | x' <- [aX..fst dim-1+aX], (sin $ utiDiv x' 10) > 0]