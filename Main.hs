module Main where
import Network.MateLight.Simple

import Data.Maybe
import qualified Network.Socket as Sock

import TypeDefs
import Utilities
import LevelGen
import Player
import Area

drawCanvas :: GameState -> Canvas
-- PURPOSE
-- draws the canvas
drawCanvas (areas, time, player) = drawPlayerToCanvas player $ drawAreasCanvas areas time

increaseTime :: GameState -> GameState
-- PURPOSE
-- increases the current time of the gamestate by 1
increaseTime (a,t,b) = (a, t + 1, b)

toFrame :: [Event String] -> GameState -> (ListFrame, GameState)
toFrame _ (a, b, p) = (ListFrame (drawCanvas gameState), gameState)
  where
    vA        = genVisAreas (b+1)
    gameState = (vA, b+1, p)

main :: IO ()
--main = Sock.withSocketsDo $ runMate (Config (fromJust $ parseAddress "127.0.0.1") 1337 dim (Just 33000) False []) toFrame (level, 0)
main = Sock.withSocketsDo $ runMate (Config (fromJust $ parseAddress "134.28.70.172") 1337 dim (Just 33000) False []) toFrame (genVisAreas 0, 0, (0, 8))
