module Pixels (
  
  Pixels(Pixels), Pixel(Pixel),

  color, dots,

  on,

  font

  ) where

import Graphics.HGL (Color)
import qualified Data.Map as Map

data Pixels = Pixels { color :: Color, dots :: [[Pixel]] }
              deriving Show
              
data Pixel = Pixel { on :: Bool }
             deriving Show

font :: Map.Map Char Pixels -> Char -> Pixels
font bm c = bm Map.! c
