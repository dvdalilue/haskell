module Pixels (main)
       where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Graphics.HGL
import System.IO

data Pixels = Pixels { color :: Color, dots :: [[Pixels]] }
              
data Pixel = Pixel { on :: Bool }
             deriving Show

data Effects = Say String
             | Up
             | Down
             | Left
             | Right
             | Backwards
             | UpsideDown
             | Negative
             | Delay Integer
             | Color Color
             | Repeat Integer [Effects]
             | Forever [Effects]

lee = do
  f <- openFile "fontBeatMap.in" ReadMode
  readFont f

--lol :: (Map Char Pixel) -> IO Pixel
--lol m = return $ font
  
readFont :: Handle -> IO (Map Char Pixel)
readFont _ = return $ Map.singleton 'c' $ Pixel True

font :: Map Char Pixels -> Char -> Pixels
font bm c = bm Map.! c

main :: IO ()
main = runGraphics $ do
  w <- openWindow "Hello World Window" (300, 300)
  drawInWindow w (text (100, 100) "Hello")
  drawInWindow w (text (100, 150) "World")
  getKey w
  closeWindow w
