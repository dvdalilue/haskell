{- |
     Implementacion de una estructura Effects,
     con distintos efectos especiales para los Pixels
-}

module Effects (

  -- * El tipo @Effects@
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
  ) where

import qualified Graphics.HGL as HGL (Color)
import Control.Concurrent (threadDelay)
import Pixels as Pix

-- | Tipo de datos para Effects
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
