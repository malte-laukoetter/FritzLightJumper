module Area where
import Network.MateLight.Simple
import TypeDefs
import Utilities

inArea :: Area -> Pos -> Bool
inArea ((x1, y1), (x2, y2)) (x, y) = x>=x1 && x<=x2 && y>=y1 && y<=y2

inAreas :: [Area] -> Pos -> Bool
inAreas xs p = filter ((flip inArea) p) xs /= []

drawAreasCanvas :: [Area] -> Time -> Canvas
drawAreasCanvas areas time = [[if inAreasTime areas time (x, y) then Pixel 255 255 0 else Pixel 0 0 0 | x <- [0..(fst dim)-1]] | y <- [0..(snd dim)-1]]

inAreasTime :: [Area] -> Time -> Pos -> Bool
inAreasTime areas time (x,y) = inAreas areas (x+aX, y)
  where aX = getScrollAlpha time;
