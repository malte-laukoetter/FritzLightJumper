module GameOver where
import System.IO.Unsafe
import Network.MateLight.Simple
import System.IO
import TypeDefs
import Utilities
import LevelGen

-- CONTRACT
gameOver_initGameState :: GameState

-- PURPOSE
-- The default gamestate

-- DEFINITION
gameOver_initGameState = ([], 0, ((3, 2), 0), [(1, 9),(6,5),(20,8),(14,4),(7,3),(2,6),
                                                                                                                             (14,4),(14,5),(14,6),(14,7),(13,5),(12,6),
                                                                                                                             (12,7),(11,5),(10,4),(10,5),(10,6),(10,7),
                                                                                                                             (7,4),(8,5),(8,6),(7,7),(6,4),(6,7),(5,6),(5,5),
                                                                                                                             (0,4),(1,4),(2,4),(3,4),(4,4),(2,5),(2,6),(2,7),
                                                                                                                             (29,4),(29,5),(29,6),(29,7),(28,5),(27,6),
                                                                                                                             (27,7),(26,5),(25,4),(25,5),(25,6),(25,7),
                                                                                                                             (22,4),(23,5),(23,6),(22,7),(21,4),(21,7),(20,6),(20,5),
                                                                                                                             (15,4),(16,4),(17,4),(18,4),(19,4),(17,5),(17,6),(17,7)], True)





-- CONTRACT
-- Note you can't go to Int only
loadHighscore :: IO Int

-- PURPOSE
-- Loads the current Highscore from the save document

-- DEFINITION
loadHighscore = do
    s <- readFile "highscore.db"
    let [l] = lines s
    let hs  = read (init l)
    return hs

-- CONTRACT
saveHighscore :: Int -> IO()

-- PURPOSE
-- Saves the Highscore to the save document

-- DEFINITION
saveHighscore time = writeFile "highscore.db" $ unwords [show time]

--CONTRACT
drawHighscore :: Time -> Canvas

--PURPOSE
--Draws the actual Highscore

--DEFINITION
drawHighscore time
     | time >1 =  drawNumber (reverse(digs(time-30))) 0 (14,3) (drawText "HS" 0 (1,3) genEmpty)
     | otherwise                =  drawNumber (reverse(digs(time-30))) 0 (14,3) genEmpty

-- CONTRACT
drawGameOver :: [Event String]-> GameState -> (ListFrame, GameState)

-- PURPOSE
-- Generates the game over screen nd saves the new highscore if the player succeded
-- Redirects on button press back to game menu

-- DEFINITION

drawGameOver (Event "KEYBOARD" _:_) (a,t,p,b,c) = (ListFrame(genEmpty), gameOver_initGameState)
drawGameOver _ (a,t,p,b,c) = (ListFrame(drawHighscore t) , (a,t,p,b,c) )
