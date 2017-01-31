module Player where
import Network.MateLight.Simple
import TypeDefs
import Utilities
import LevelGen

-- CONTRACT
drawPlayerToCanvas :: Player -> Canvas -> Canvas

-- PURPOSE
-- draws the player to the canvas

-- DEFINITION
drawPlayerToCanvas (pos, _) canvas = replaceCanvasPos pos canvas (Pixel 255 0 0)

-- CONTRACT
playerEvents :: [Event String] -> GameState -> Player

-- PURPOSE
-- handels the player events
-- in particular it lets the player jump if it is on the floor and a keypress is recognised

-- DEFINITION
playerEvents [] (_,_,player,_,_)                     = player
playerEvents (Event mode _:_) state@(_,_,player,_,_) | mode == "KEYBOARD" && isPlayerOnFloor state = jumpPlayer player
playerEvents _ (_,_,player,_,_)                      = player

-- CONTRACT
jumpPlayer :: Player -> Player

-- PURPOSE
-- Issues that the player jumps

-- DEFINITION
jumpPlayer (pos, a) = (pos, 5)

-- CONTRACT
playerTick :: GameState -> Player

-- PURPOSE
-- Issues the movement of the player

-- DEFINITION
playerTick state@(_,_,p@((x,y), 0),_,_)   | isPlayerOnFloor state = p
                                      | otherwise             = ((x,y+1), 0)
playerTick state@(_,_,((x, y), jump),_,_) = ((x, y-1), jump-1)

-- CONTRACT
isPlayerOnFloor :: GameState -> Bool

-- PURPOSE
-- tests if the player is currendly on the floor of the level or on top of an area

-- DEFINITION
isPlayerOnFloor (_, _, ((_,11),_),_,_) = True
isPlayerOnFloor (a,t,((x,y), _),_,_)  = inAreasTime a t (x,y+1)

-- CONTRACT
isPlayerInAreas :: GameState -> Bool

-- PURPOSE
-- tests if the player is currendly in one of the areas

-- DEFINITION
isPlayerInAreas (a,t,((x,y), _),_,_)  = inAreasTime a t (x,y)
