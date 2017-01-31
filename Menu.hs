module Menu where
import Network.MateLight.Simple

import Data.Maybe
import qualified Network.Socket as Sock
import TypeDefs

move :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
move (xdim, ydim) [(x1,y1),(x2,y2),(x3,y3),(x4,y4),(x5,y5),(x6,y6),(x7,y7),(x8,y8),(x9,y9),(x10,y10),(x11,y11),
                   (x12,y12),(x13,y13),(x14,y14),(x15,y15),(x16,y16),(x17,y17),(x18,y18),(x19,y19),(x20,y20),(x21,y21),(x22,y22),(x23,y23),(x24,y24),(x25,y25),(x26,y26),(x27,y27),(x28,y28),(x29,y29),(x30,y30),(x31,y31),(x32,y32),(x33,y33),(x34,y34),(x35,y35),(x36,y36),(x37,y37),(x38,y38),(x39,y39),
                   (x40,y40),(x41,y41),(x42,y42),(x43,y43),
                   (x44,y44),(x45,y45),(x46,y46),(x47,y47),
                   (x48,y48),(x49,y49),(x50,y50),(x51,y51),(x52,y52),(x53,y53),(x54,y54),
                   (x55,y55),(x56,y56),(x57,y57),(x58,y58),(x59,y59),(x60,y60),(x61,y61),(x62,y62)] = [((x1-1) `mod` xdim ,(y1-1) `mod` ydim),
                                                                               ((x2+2) `mod` xdim ,(y2-1) `mod` ydim),
                                                                               ((x3-1) `mod` xdim,(y3+2) `mod` ydim),
                                                                               ((x4+1)`mod` xdim,(y4+1)`mod` ydim),
                                                                               ((x5-4) `mod` xdim ,(y5-1) `mod` ydim),
                                                                               ((x6+1) `mod` xdim ,(y6-4) `mod` ydim),
                                                                               (x7,y7),(x8,y8),(x9,y9),(x10,y10),(x11,y11),
                                                                               (x12,y12),(x13,y13),(x14,y14),(x15,y15),
                                                                               (x16,y16),(x17,y17),(x18,y18),(x19,y19),
                                                                               (x20,y20),(x21,y21),(x22,y22),(x23,y23),(x24,y24),(x25,y25),(x26,y26),
                                                                               (x27,y27),(x28,y28),(x29,y29),(x30,y30),(x31,y31),(x32,y32),(x33,y33),(x34,y34),
                                                                               (x35,y35),(x36,y36),(x37,y37),(x38,y38),(x39,y39),
                                                                               (x40,y40),(x41,y41),(x42,y42),(x43,y43),
                                                                               (x44,y44),(x45,y45),(x46,y46),(x47,y47),
                                                                               (x48,y48),(x49,y49),(x50,y50),(x51,y51),(x52,y52),(x53,y53),(x54,y54),
                                                                               (x55,y55),(x56,y56),(x57,y57),(x58,y58),(x59,y59),(x60,y60),(x61,y61),(x62,y62)]



toFrame :: (Int, Int) -> [(Int, Int)] -> ListFrame
toFrame (xdim, ydim) z = ListFrame $ map (\y -> map (\x -> color z (x,y)) [0 .. xdim - 1]) [0 .. ydim - 1]

draw :: [(Int, Int)] -> (Int, Int) -> Pixel
draw z (x,y) | (x `mod` 2)/=0 && (y `mod` 2)==0 && check z (x,y)  = Pixel 0 0 0xff
             | (x `mod` 2)/=0 && (y `mod` 2)/=0 && check z (x,y)  = Pixel 0xff 0 0
             | (x `mod` 2)==0 && (y `mod` 2)==0 && check z (x,y)  = Pixel 0 0xff 0
             | check z (x,y)  = Pixel 0xff 0xff 0xff
             | otherwise      = Pixel 0 0 0

color :: [(Int, Int)] -> (Int, Int) -> Pixel
color z (x,y) | check (drop 6 z) (x,y) = Pixel 0xff 0xff 0
              | otherwise     = draw z (x,y)

check :: [(Int, Int)] -> (Int, Int) -> Bool
check [] _ = False
check (x:xs) (y, z) | fst x == y && snd x == z = True
                    | otherwise = check xs (y,z)

eventTest :: [Event String] -> GameState -> (ListFrame, GameState)
eventTest _ (a,b,c,pixel, False) = (toFrame dim pixel, (a,b,c,pixel, False))
eventTest (Event "KEYBOARD" _:_) (a,b,c,pixel, status) = (toFrame dim pixel, (a,b,c,pixel, False))
eventTest _ (a,b,c,pixel, status) = (toFrame dim pixel', (a,b,c,pixel', True))
  where pixel' = move dim pixel
