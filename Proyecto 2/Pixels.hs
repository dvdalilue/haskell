module Pixels (
  
  Pixel(Pixel),

  on,

  Pixels(Pixels),
  
  color, dots,
  
  up, down, left, right,

  upsideDown, backwards,

  negative, concatPixels
  
  ) where

import qualified Graphics.HGL as HGL (Color)

data Pixels = Pixels { color :: HGL.Color, dots :: [[Pixel]] }
              deriving Show
              
data Pixel = Pixel { on :: Bool }
             deriving Show

-- | 'up' dezplaza una hilera hacia arriba
up :: Pixels -> Pixels
up ps = let pts = dots ps in Pixels (color ps) (tail pts ++ [head pts])
--up wrd = tail wrd ++ [head wrd]
   
-- | 'down' dezplaza una hilera hacia abajo
down :: Pixels -> Pixels
down ps = let pts = dots ps in Pixels (color ps) (last pts : init pts)
-- down wrd = last wrd : init wrd

-- | 'left' dezplaza una columna hacia la izquierda.
left :: Pixels -> Pixels
left ps = let pts = dots ps in Pixels (color ps) [tail x ++ [head x] | x<-pts]
-- left wrd = [ tail x ++ [head x] | x <- wrd ]

-- | 'right' dezplaza una columna hacia la derecha.
right :: Pixels -> Pixels
right ps = let pts = dots ps in Pixels (color ps) [last x : init x | x<-pts]
-- right wrd = [ last x : init x | x <- wrd ]

-- | 'upsideDown' invierte el orden de las filas.
upsideDown :: Pixels -> Pixels
upsideDown ps = let pts = dots ps in Pixels (color ps) (reverse pts)
-- upsideDown wrd = tail wrd ++ [head wrd]

-- | 'backwards' invierte el orden de las columnas.
backwards :: Pixels -> Pixels
backwards ps = let pts = dots ps in Pixels (color ps) (map reverse pts)
-- backwards wrd = map reverse wrd

-- | 'negative' intercambia blancos por astericos y viceversa.  
negative :: Pixels -> Pixels
negative ps = let pts = dots ps in Pixels (color ps) (neg pts)
  where neg p = map (map (\c->Pixel (not (on c)))) p
-- negative wrd = [ [ if y=='*' then ' ' else '*' | y<-x ] | x<-wrd ]

concatPixels :: [Pixels] -> Pixels
concatPixels (p:ps) = Pixels (color p) $foldl mergePixels (dots p) (map dots ps)
  where mergePixels a b = map (\ps -> (fst ps)++(snd ps)) $ zip a b
        
--messageToPixels