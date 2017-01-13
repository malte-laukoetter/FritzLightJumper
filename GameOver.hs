module GameOver where
import Network.MateLight.Simple
import System.IO
import TypeDefs
import Utilities

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

-- CONTRACT
drawGameOver :: Time -> Canvas

-- PURPOSE
-- Generates the game over screen and saves the new highscore if the player succeded
-- Redirects on button press back to game menu

-- DEFINITION
drawGameOver = undefined
