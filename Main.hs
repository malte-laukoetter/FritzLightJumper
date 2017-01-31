module Main where
import Network.MateLight.Simple

import Data.Maybe
import qualified Network.Socket as Sock

import TypeDefs
import Utilities
import LevelGen
import Player
import GameOver
import qualified Menu as Menu

-- CONTRACT
initGameState :: GameState

-- PURPOSE
-- The default gamestate

-- DEFINITION
initGameState = ([], 0, ((3, 2), 0), [(1, 9),(6,5),(20,8),(14,4),(7,3),(2,6),
                                                                                                                             (14,4),(14,5),(14,6),(14,7),(13,5),(12,6),
                                                                                                                             (12,7),(11,5),(10,4),(10,5),(10,6),(10,7),
                                                                                                                             (7,4),(8,5),(8,6),(7,7),(6,4),(6,7),(5,6),(5,5),
                                                                                                                             (0,4),(1,4),(2,4),(3,4),(4,4),(2,5),(2,6),(2,7),
                                                                                                                             (29,4),(29,5),(29,6),(29,7),(28,5),(27,6),
                                                                                                                             (27,7),(26,5),(25,4),(25,5),(25,6),(25,7),
                                                                                                                             (22,4),(23,5),(23,6),(22,7),(21,4),(21,7),(20,6),(20,5),
                                                                                                                             (15,4),(16,4),(17,4),(18,4),(19,4),(17,5),(17,6),(17,7)], True)

-- CONTRACT
drawCanvas :: GameState -> Canvas
-- PURPOSE
-- draws the canvas

-- DEFINITION
drawCanvas (areas, time, player, _, _) | time >= 30 = drawPlayerToCanvas player $ drawNumber (reverse (digs (time-30))) 0 (1,1) $ drawAreasCanvas areas time
                                 | otherwise  = drawCountDown $ time - 30

-- CONTRACT
eventHandler :: GameState -> [Event String] -> GameState

-- PURPOSE
-- Handles events and performs player events(?)

-- DEFINITION
eventHandler state@(areas, time, player, a, b) events = (areas, time, playerEvents events state, a, b)

-- CONTRACT
increaseTime :: GameState -> GameState

-- PURPOSE
-- increases the current time of the gamestate by 1

-- DEFINITION
increaseTime (a,t,p, b,c) = (a, t + 1, p, b,c)

-- CONTRACT
toFrame :: [Event String] -> GameState -> (ListFrame, GameState)

-- PURPOSE
-- Converts gamestate to the needed ListFrame object and is the main function that executes all other in some form

-- DEFINITION
toFrame events state@(a, t, p, b, True) = Menu.eventTest events state
toFrame events (a, t, p, b,c) = (ListFrame (drawCanvas gameState), gameState)
  where
    time      = t+1
    areas     = genVisAreas time 100 -- Add seed number variable here!
    player    = playerTick (areas, time, p, b, c)
    gameState | not (isPlayerInAreas (areas, time, p, b, c)) = eventHandler (areas, time, player, b, c) events
              | otherwise                              = initGameState -- Game over | You gotta work with the time variable and states to prevent other events from being triggered
-- CONTRACT
main :: IO ()

-- PURPOSE
-- Fire that baby!

-- DEFINITION
main = do
  highS <- loadHighscore -- This is from type Int now. You can use it for the game over for example
  Sock.withSocketsDo $ runMate (Config (fromJust $ parseAddress "127.0.0.1") 1337 dim (Just 33000) True []) toFrame initGameState
--main = do
  --highS <- loadHighscore -- This is from type Int now. You can use it for the game over for example
  --Sock.withSocketsDo $ runMate (Config (fromJust $ parseAddress "134.28.70.172") 1337 dim (Just 33000) True []) toFrame initGameState
