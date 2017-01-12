module GameOver where
import Network.MateLight.Simple
import TypeDefs
import Utilities

-- CONTRACT
loadHighscore :: IO() -> Int

-- PURPOSE
-- Loads the current Highscore from the save document

-- DEFINITION
loadHighscore = undefined


-- CONTRACT
saveHighscore :: Int -> IO()

-- PURPOSE
-- Saves the Highscore to the save document

-- DEFINITION
saveHighscore = undefined

-- CONTRACT
drawGameOver :: Time -> Canvas

-- PURPOSE
-- Generates the game over screen and saves the new highscore if the player succeded
-- Redirects on button press back to game menu

-- DEFINITION
drawGameOver = undefined