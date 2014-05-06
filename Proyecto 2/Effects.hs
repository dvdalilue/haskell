module Effects (

  Effects (
     Say,
     Up, Down, Left, Right,
     UpsideDown, Backwards,
     Negative,
     Delay,
     Color,
     Repeat,
     Forever
     )

  ) where

import qualified Graphics.HGL as HGL (Color)
import Pixels
import Data.Typeable

data Effects = Say String
             | Up
             | Down
             | Left
             | Right
             | Backwards
             | UpsideDown
             | Negative
             | Delay Integer
             | Color HGL.Color
             | Repeat Integer [Effects]
             | Forever [Effects]
             deriving (Show, Read)
