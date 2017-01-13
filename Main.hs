module Main where
import Network.MateLight.Simple

import Data.Maybe
import qualified Network.Socket as Sock

import TypeDefs
import Utilities
import LevelGen
import Player
import GameOver
import GameMenu

-- CONTRACT
initGameState :: GameState

-- PURPOSE
-- The default gamestate

-- DEFINITION
initGameState = (genVisAreas 0, 0, ((0, 2), 0))

-- CONTRACT
drawCanvas :: GameState -> Canvas
-- PURPOSE
-- draws the canvas

-- DEFINITION
drawCanvas (areas, time, player) | time >= 30 = drawPlayerToCanvas player $ drawNumber (reverse (digs (time-30))) 0 (1,1) $ drawAreasCanvas areas time
                                 | otherwise  = drawCountDown $ time - 30

-- CONTRACT
eventHandler :: GameState -> [Event String] -> GameState

-- PURPOSE
-- Handles events and performs player events(?)

-- DEFINITION
eventHandler state@(areas, time, player) events = (areas, time, playerEvents events state)

-- CONTRACT
increaseTime :: GameState -> GameState

-- PURPOSE
-- increases the current time of the gamestate by 1

-- DEFINITION
increaseTime (a,t,p) = (a, t + 1, p)

-- CONTRACT
toFrame :: [Event String] -> GameState -> (ListFrame, GameState)

-- PURPOSE
-- Converts gamestate to the needed ListFrame object and is the main function that executes all other in some form

-- DEFINITION
toFrame events (a, t, p) = (ListFrame (drawCanvas gameState), gameState)
  where
    time      = t+1
    areas     = genVisAreas time
    player    = playerTick (areas, time, p)
    gameState | not (isPlayerInAreas (areas, time, p)) = eventHandler (areas, time, player) events
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
