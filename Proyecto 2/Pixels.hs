module Pixels (
  
  Pixels(Pixels), Pixel(Pixel),

  color, dots,

  on

  ) where

import Graphics.HGL (Color)

data Pixels = Pixels { color :: Color, dots :: [[Pixel]] }
              deriving Show
              
data Pixel = Pixel { on :: Bool }
             deriving Show
