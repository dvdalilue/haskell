{- |
     Implementacion de una estructura Pixels,
     que representa los carectares de la tabla ASCII
-}

module Pixels (
  -- * El tipo @Pixels@
  Pixels,

  -- ** Funciones sobre @Pixels@
  --  font, printa, asterisks, asterisks, intToBin, transpose,
  --  pixelsToString, pixelListToPixels, pixelListToString, concatPixels, mergePixels, messageToPixels, 
  --  up, down, left, right, upsideDown, backwards, negative,
                                                
  -- * Mapa de bit para los caracteres
  fontBitmap,
  ) where

import Data.List

type Pixels = [String]

fontBitmap =
  [
    [ 0x00, 0x00, 0x00, 0x00, 0x00 ], --  (space)
    [ 0x00, 0x00, 0x5F, 0x00, 0x00 ], --  !
    [ 0x00, 0x07, 0x00, 0x07, 0x00 ], --  "
    [ 0x14, 0x7F, 0x14, 0x7F, 0x14 ], --  #
    [ 0x24, 0x2A, 0x7F, 0x2A, 0x12 ], --  $
    [ 0x23, 0x13, 0x08, 0x64, 0x62 ], --  %
    [ 0x36, 0x49, 0x55, 0x22, 0x50 ], --  &
    [ 0x00, 0x05, 0x03, 0x00, 0x00 ], --  '
    [ 0x00, 0x1C, 0x22, 0x41, 0x00 ], --  (
    [ 0x00, 0x41, 0x22, 0x1C, 0x00 ], --  )
    [ 0x08, 0x2A, 0x1C, 0x2A, 0x08 ], --  *
    [ 0x08, 0x08, 0x3E, 0x08, 0x08 ], --  +
    [ 0x00, 0x50, 0x30, 0x00, 0x00 ], --  ,
    [ 0x08, 0x08, 0x08, 0x08, 0x08 ], --  -
    [ 0x00, 0x60, 0x60, 0x00, 0x00 ], --  .
    [ 0x20, 0x10, 0x08, 0x04, 0x02 ], --  /
    [ 0x3E, 0x51, 0x49, 0x45, 0x3E ], --  0
    [ 0x00, 0x42, 0x7F, 0x40, 0x00 ], --  1
    [ 0x42, 0x61, 0x51, 0x49, 0x46 ], --  2
    [ 0x21, 0x41, 0x45, 0x4B, 0x31 ], --  3
    [ 0x18, 0x14, 0x12, 0x7F, 0x10 ], --  4
    [ 0x27, 0x45, 0x45, 0x45, 0x39 ], --  5
    [ 0x3C, 0x4A, 0x49, 0x49, 0x30 ], --  6
    [ 0x01, 0x71, 0x09, 0x05, 0x03 ], --  7
    [ 0x36, 0x49, 0x49, 0x49, 0x36 ], --  8
    [ 0x06, 0x49, 0x49, 0x29, 0x1E ], --  9
    [ 0x00, 0x36, 0x36, 0x00, 0x00 ], --  :
    [ 0x00, 0x56, 0x36, 0x00, 0x00 ], --  ;
    [ 0x00, 0x08, 0x14, 0x22, 0x41 ], --  <
    [ 0x14, 0x14, 0x14, 0x14, 0x14 ], --  =
    [ 0x41, 0x22, 0x14, 0x08, 0x00 ], --  >
    [ 0x02, 0x01, 0x51, 0x09, 0x06 ], --  ?
    [ 0x32, 0x49, 0x79, 0x41, 0x3E ], --  @
    [ 0x7E, 0x11, 0x11, 0x11, 0x7E ], --  A
    [ 0x7F, 0x49, 0x49, 0x49, 0x36 ], --  B
    [ 0x3E, 0x41, 0x41, 0x41, 0x22 ], --  C
    [ 0x7F, 0x41, 0x41, 0x22, 0x1C ], --  D
    [ 0x7F, 0x49, 0x49, 0x49, 0x41 ], --  E
    [ 0x7F, 0x09, 0x09, 0x01, 0x01 ], --  F
    [ 0x3E, 0x41, 0x41, 0x51, 0x32 ], --  G
    [ 0x7F, 0x08, 0x08, 0x08, 0x7F ], --  H
    [ 0x00, 0x41, 0x7F, 0x41, 0x00 ], --  I
    [ 0x20, 0x40, 0x41, 0x3F, 0x01 ], --  J
    [ 0x7F, 0x08, 0x14, 0x22, 0x41 ], --  K
    [ 0x7F, 0x40, 0x40, 0x40, 0x40 ], --  L
    [ 0x7F, 0x02, 0x04, 0x02, 0x7F ], --  M
    [ 0x7F, 0x04, 0x08, 0x10, 0x7F ], --  N
    [ 0x3E, 0x41, 0x41, 0x41, 0x3E ], --  O
    [ 0x7F, 0x09, 0x09, 0x09, 0x06 ], --  P
    [ 0x3E, 0x41, 0x51, 0x21, 0x5E ], --  Q
    [ 0x7F, 0x09, 0x19, 0x29, 0x46 ], --  R
    [ 0x46, 0x49, 0x49, 0x49, 0x31 ], --  S
    [ 0x01, 0x01, 0x7F, 0x01, 0x01 ], --  T
    [ 0x3F, 0x40, 0x40, 0x40, 0x3F ], --  U
    [ 0x1F, 0x20, 0x40, 0x20, 0x1F ], --  V
    [ 0x7F, 0x20, 0x18, 0x20, 0x7F ], --  W
    [ 0x63, 0x14, 0x08, 0x14, 0x63 ], --  X
    [ 0x03, 0x04, 0x78, 0x04, 0x03 ], --  Y
    [ 0x61, 0x51, 0x49, 0x45, 0x43 ], --  Z
    [ 0x00, 0x00, 0x7F, 0x41, 0x41 ], --  [
    [ 0x02, 0x04, 0x08, 0x10, 0x20 ], --  \
    [ 0x41, 0x41, 0x7F, 0x00, 0x00 ], --  ]
    [ 0x04, 0x02, 0x01, 0x02, 0x04 ], --  ^
    [ 0x40, 0x40, 0x40, 0x40, 0x40 ], --  _
    [ 0x00, 0x01, 0x02, 0x04, 0x00 ], --  `
    [ 0x20, 0x54, 0x54, 0x54, 0x78 ], --  a
    [ 0x7F, 0x48, 0x44, 0x44, 0x38 ], --  b
    [ 0x38, 0x44, 0x44, 0x44, 0x20 ], --  c
    [ 0x38, 0x44, 0x44, 0x48, 0x7F ], --  d
    [ 0x38, 0x54, 0x54, 0x54, 0x18 ], --  e
    [ 0x08, 0x7E, 0x09, 0x01, 0x02 ], --  f
    [ 0x08, 0x14, 0x54, 0x54, 0x3C ], --  g
    [ 0x7F, 0x08, 0x04, 0x04, 0x78 ], --  h
    [ 0x00, 0x44, 0x7D, 0x40, 0x00 ], --  i
    [ 0x20, 0x40, 0x44, 0x3D, 0x00 ], --  j
    [ 0x00, 0x7F, 0x10, 0x28, 0x44 ], --  k
    [ 0x00, 0x41, 0x7F, 0x40, 0x00 ], --  l
    [ 0x7C, 0x04, 0x18, 0x04, 0x78 ], --  m
    [ 0x7C, 0x08, 0x04, 0x04, 0x78 ], --  n
    [ 0x38, 0x44, 0x44, 0x44, 0x38 ], --  o
    [ 0x7C, 0x14, 0x14, 0x14, 0x08 ], --  p
    [ 0x08, 0x14, 0x14, 0x18, 0x7C ], --  q
    [ 0x7C, 0x08, 0x04, 0x04, 0x08 ], --  r
    [ 0x48, 0x54, 0x54, 0x54, 0x20 ], --  s
    [ 0x04, 0x3F, 0x44, 0x40, 0x20 ], --  t
    [ 0x3C, 0x40, 0x40, 0x20, 0x7C ], --  u
    [ 0x1C, 0x20, 0x40, 0x20, 0x1C ], --  v
    [ 0x3C, 0x40, 0x30, 0x40, 0x3C ], --  w
    [ 0x44, 0x28, 0x10, 0x28, 0x44 ], --  x
    [ 0x0C, 0x50, 0x50, 0x50, 0x3C ], --  y
    [ 0x44, 0x64, 0x54, 0x4C, 0x44 ], --  z
    [ 0x00, 0x08, 0x36, 0x41, 0x00 ], --  {
    [ 0x00, 0x00, 0x7F, 0x00, 0x00 ], --  |
    [ 0x00, 0x41, 0x36, 0x08, 0x00 ]  --  }
  ]

-- | Convierte un caracter dado a se representacion en Pixels
font :: Char -> Pixels
font c = pixelate fontBitmap (fromEnum c - 32)

ascii :: Int -> Char
ascii n = toEnum n

-- | Imprime el Pixels como una pila, para su visualizacion
printStackPixels :: Pixels -> IO()
printStackPixels x = mapM_ print x

-- | Convierte a Pixels, el caracter que esta en la posicion pasada como argumento 
pixelate :: [[Integer]] -> Int -> Pixels
pixelate list c = transpose (map (asterisks . intToBin) (list !! c))

-- | Cambia la representacion de numeros, a caracteres '*' para prendido y cualquier otro con ' ' para apagado
asterisks :: [Integer] -> String
asterisks b = dale b []
  where dale xs acc
          | null xs      = acc
          | head xs == 1 = dale (tail xs) ('*' : acc) 
          | otherwise    = dale (tail xs) (' ' : acc)

-- | Transforma un entero a su representacion binaria
intToBin :: Integer -> [Integer]
intToBin h = descDiv h 7 []
  where descDiv _ 0 acc   = acc
        descDiv 0 cnt acc = descDiv 0 (cnt-1) (0 : acc)
        descDiv i cnt acc = descDiv (div i 2) (cnt-1) (mod i 2 : acc)

-- | Convierte el valor de un tipo Pixels a un String
pixelsToString :: Pixels -> String
pixelsToString [] = []
pixelsToString p  = init $ unlines p

-- | Convierte una lista de Pixels a una sola representacion de Pixels
pixelListToPixels :: [Pixels] -> Pixels
pixelListToPixels [] = []
pixelListToPixels ps = concat $ intersperse [""] ps

pixelListToString :: [Pixels] -> String
pixelListToString [] = []
pixelListToString ps = concat $ intersperse [toEnum 13] (map pixelsToString ps)

concatPixels :: [Pixels] -> Pixels
concatPixels p = dale p ["","","","","","",""]
  where dale xs acc
          | null xs   = acc
          | otherwise = dale (tail xs) (mergePixels acc (head xs) "")

mergePixels :: Pixels -> Pixels -> String -> Pixels
mergePixels a b int = dale a b int []
  where dale x y int acc
          | null y    = acc 
          | otherwise = dale (tail x) (tail y) int (acc ++ ([((head x) ++ int) ++ (head y)]))

messageToPixels :: String -> Pixels
messageToPixels msg = dale msg ["","","","","","",""]
  where dale s acc
          | null s    = acc
          | otherwise = dale (tail s) (mergePixels acc ((font . head) s) " ")

up :: Pixels -> Pixels
up wrd = tail wrd ++ [head wrd]
   
down :: Pixels -> Pixels
down wrd = last wrd : init wrd

left :: Pixels -> Pixels
left wrd = [ tail x ++ [head x] | x <- wrd ]

right :: Pixels -> Pixels
right wrd = [ last x : init x | x <- wrd ]

upsideDown :: Pixels -> Pixels
upsideDown wrd = tail wrd ++ [head wrd]

backwards :: Pixels -> Pixels
backwards wrd = map reverse wrd
  
negative :: Pixels -> Pixels
negative wrd = [ [ if y=='*' then ' ' else '*' | y<-x ] | x<-wrd ]
