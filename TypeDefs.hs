module TypeDefs where
import Network.MateLight.Simple

-- Defining all necessary variables and types

dim :: (Int, Int)
dim = (30, 12)

type Time = Int

type Pos = (Int, Int)
type Area = (Pos, Pos)
type Canvas = [[Pixel]]

type Player = (Pos, Int)
type GameState = ([Area], Time, Player)
