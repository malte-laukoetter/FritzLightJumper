module Player where
import Network.MateLight.Simple
import TypeDefs
import Utilities

drawPlayerToCanvas :: Player -> Canvas -> Canvas
-- PURPOSE
-- draws the player to the canvas
drawPlayerToCanvas player canvas = replaceCanvasPos player canvas (Pixel 0 0 255)
