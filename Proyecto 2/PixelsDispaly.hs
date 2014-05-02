module Pixels (main)
       where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Graphics.HGL
import System.IO

data Pixels = Pixels { color :: Color, dots :: [[Pixel]] }
              deriving Show
              
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


hGetNextLine :: Handle -> IO String
hGetNextLine handle = do
  b <- hIsEOF handle
  if b then return "EOF" else go handle
    where go h = do
            l <- hGetLine h
            if (null . words) l then hGetNextLine handle
              else return l

hGetNextLineTrim :: Handle -> IO [String]
hGetNextLineTrim handle = do
  l <- hGetNextLine handle
  return $ words l

hGetKey :: Handle -> IO Char
hGetKey handle = do
  c <- hGetNextLineTrim handle
  return $ head . tail $ head c

hGetValue :: Handle -> Int -> Int -> IO [[Pixel]]
hGetValue handle width height = dale handle width height []
  where dale h a l acc
          | length acc == l = return $ reverse acc
          | otherwise       = dao h a l acc
            where dao h a l acc = do
                    v <- hGetNextLine h
                    dale h a l $ (fun (fillS v a) a []):acc
                    where fun s w acc
                            | null s    = if length acc == w then reverse acc else error "\nMal Dimension De Columnas"
                            | otherwise = fun (tail s) w $ (astk (head s)) : acc
                            where astk k
                                    | k == '*'  = Pixel True
                                    | otherwise = Pixel False

fillS :: String -> Int -> String
fillS s n = if length s >= n then s else fll s n []
  where fll _ 0 acc  = reverse acc
        fll [] l acc = fll [] (l-1) $ ' ':acc
        fll s l acc  = fll (tail s) (l-1) $ (head s):acc

fCatch macc h (a:l:ss) = do -- Falta caso donde hay mas filas que las especificadas
  eof   <- hIsEOF h
  key   <- hGetKey h
  value <- hGetValue h (read a::Int) (read l::Int)
  if not eof && spc key value then fCatch (Map.insert key (Pixels White value) macc) h (a:l:ss) else return macc
    where spc k v
            | k == 'O' && (or $ map or $ map (map on) v) = True
            | k == 'O'  = False
            | otherwise = True

readFont :: Handle -> IO (Map Char Pixels) -- Definicion Intocable 
readFont h = do
  size <- hGetNextLineTrim h
  fCatch Map.empty h size

font :: Map Char Pixels -> Char -> Pixels -- Definicion Intocable 
font bm c = bm Map.! c

main = do -- runGraphics $ do
  f <- openFile "fontBeatMap.in" ReadMode
  e <- readFont f
  hClose f
  let l = Map.keys e
  runGraphics $ do
    w <- openWindow "Led Display" (300, 300)
    drawInWindow w (text (100, 100) [(head l)])
    drawInWindow w (text (100, 150) [(last l)])
    getKey w
    closeWindow w

