module Player where
import Network.MateLight.Simple
import TypeDefs
import Utilities
import Area

drawPlayerToCanvas :: Player -> Canvas -> Canvas
-- PURPOSE
-- draws the player to the canvas
drawPlayerToCanvas (pos, _) canvas = replaceCanvasPos pos canvas (Pixel 255 0 0)

playerEvents :: [Event String] -> GameState -> Player
-- PURPOSE
-- handels the player events
-- in particular it lets the player jump if it is on the floor and a keypress is recognised
playerEvents [] (_,_,player)                     = player
playerEvents (Event mode _:_) state@(_,_,player) | mode == "KEYBOARD" && isPlayerOnFloor state = jumpPlayer player
playerEvents _ (_,_,player)                      = player

jumpPlayer :: Player -> Player
jumpPlayer (pos, a) = (pos, 5)

playerTick :: GameState -> Player
playerTick state@(_,_,p@((x,y), 0))   | isPlayerOnFloor state = p
                                      | otherwise             = ((x,y+1), 0)
playerTick state@(_,_,((x, y), jump)) = ((x, y-1), jump-1)

isPlayerOnFloor :: GameState -> Bool
-- PURPOSE
-- tests if the player is currendly on the floor of the level or on top of an area
isPlayerOnFloor (_, _, ((_,11),_)) = True
isPlayerOnFloor (a,t,((x,y), _))  = inAreasTime a t (x,y+1)

isPlayerInAreas :: GameState -> Bool
-- PURPOSE
-- tests if the player is currendly in one of the areas
isPlayerInAreas (a,t,((x,y), _))  = inAreasTime a t (x,y)
