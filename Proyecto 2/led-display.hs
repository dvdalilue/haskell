import Pixels
import Effects
import Data.List
import System.IO
import System.Exit
import System.Environment
import qualified Data.Map as Map
import qualified Graphics.HGL as HGL

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
  if not eof && spc key value then fCatch (Map.insert key (Pixels HGL.White value) macc) h (a:l:ss) else return macc
    where spc k v
            | k == 'O' && (or $ map or $ map (map on) v) = True
            | k == 'O'  = False
            | otherwise = True

readFont :: Handle -> IO (Map.Map Char Pixels)
readFont h = do
  size <- hGetNextLineTrim h
  fCatch Map.empty h size

font :: Map.Map Char Pixels -> Char -> Pixels
font bm c = bm Map.! c

main :: IO ()
main = do
  argv <- getArgs
  f <- openFile (head argv) ReadMode
  e <- readFont f
  hClose f
  let l = Map.keys e
  HGL.runGraphics $ do
    w <- HGL.openWindow "Led Display" (300, 300)
    HGL.drawInWindow w (HGL.text (100, 100) [(head l)])
    HGL.drawInWindow w (HGL.text (100, 150) [(last l)])
    HGL.getKey w
    HGL.closeWindow w
