import P1Original
import Data.Char

listfont = map (\c -> pixelate c 7 []) fontBitmap

main = 
  do putStrLn " 5  7 "
     loop listfont 32

loop [] _ = putStr ""
loop (x:xs) i =
  do putStrLn $  "\"" ++ [chr i] ++ "\"\n" 
     putStrLn $ unlines x
     loop xs (i+1)