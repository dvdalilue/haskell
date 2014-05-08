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
     ),
  
  evalE

  ) where

import qualified Graphics.HGL as HGL (Color)
import Pixels as Pix

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
             deriving (Show,Read)

evalE :: Effects -> Pixels -> Pixels
evalE (Say s) p = p
evalE Up p = Pix.up p
evalE Down p = Pix.down p
evalE Effects.Left p = Pix.left p
evalE Effects.Right p = Pix.right p
evalE Backwards p = Pix.backwards p
evalE UpsideDown p = Pix.upsideDown p
evalE Negative p = Pix.negative p
evalE (Color c) p = p { color = c }
