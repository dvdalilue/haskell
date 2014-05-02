module Effects (

  Effects

  ) where

import Graphics.HGL (Color)

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
