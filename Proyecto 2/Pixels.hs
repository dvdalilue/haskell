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
up (Pixels{color=c,dots=d}) = Pixels c (tail d ++ [head d])
--up wrd = tail wrd ++ [head wrd]
   
-- | 'down' dezplaza una hilera hacia abajo
down :: Pixels -> Pixels
down (Pixels{color=c,dots=d}) = Pixels c (last d : init d)
-- down wrd = last wrd : init wrd

-- | 'left' dezplaza una columna hacia la izquierda.
left :: Pixels -> Pixels
left (Pixels{color=c,dots=d}) = Pixels c [tail x ++ [head x] | x<-d]
-- left wrd = [ tail x ++ [head x] | x <- wrd ]

-- | 'right' dezplaza una columna hacia la derecha.
right :: Pixels -> Pixels
right (Pixels{color=c,dots=d}) = Pixels c [last x : init x | x<-d]
-- right wrd = [ last x : init x | x <- wrd ]

-- | 'upsideDown' invierte el orden de las filas.
upsideDown :: Pixels -> Pixels
upsideDown (Pixels{color=c,dots=d}) = Pixels c (reverse d)
-- upsideDown wrd = tail wrd ++ [head wrd]

-- | 'backwards' invierte el orden de las columnas.
backwards :: Pixels -> Pixels
backwards (Pixels{color=c,dots=d}) = Pixels c (map reverse d)
-- backwards wrd = map reverse wrd

-- | 'negative' intercambia blancos por astericos y viceversa.  
negative :: Pixels -> Pixels
negative (Pixels{color=c,dots=d}) = Pixels c (neg d)
  where neg p = map (map (\c->Pixel (not (on c)))) p
-- negative wrd = [ [ if y=='*' then ' ' else '*' | y<-x ] | x<-wrd ]

concatPixels :: [Pixels] -> Pixels
concatPixels (p:ps) = Pixels (color p) $foldl mergePixels (dots p) (map dots ps)
  where mergePixels a b = map (\ps -> (fst ps)++(snd ps)) $ zip a b
        
--messageToPixels