import Pixels
import Effects as E
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

-------------------------------------------------------------------------------

--ppc = 3
ppc = 10

lezip xs = concatMap removeNull $tablero 1 $map (map on) $dots xs
  where removeNull = filter (not . (\c->c == -1) . fst)
        tablero _ [] = []
        tablero i (x:xs) = (puntos i 1 x) : (tablero (i+1) xs )
          where puntos _ _ []     = []
                puntos x y (p:ps) = let a = if p then (x,y)
                                            else (-1,-1) in a : puntos x (y+1) ps

cells d t = map cell t 
  where cell (c,f) = HGL.withColor d $ HGL.ellipse (f*ppc,c*ppc) ((f+1)*ppc,(c+1)*ppc)
    -- where cell ((c1,f1),(c,f)) = HGL.overGraphic 
    --                             (HGL.withColor d ( HGL.regionToGraphic (HGL.rectangleRegion (f*ppc,c*ppc) ((f+1)*ppc,(c+1)*ppc))))
    --                             (HGL.withColor HGL.Blue ( HGL.regionToGraphic ( (HGL.rectangleRegion (((f*ppc)-1),((c*ppc)-1)) ((((f+1)*ppc)+2),(((c+1)*ppc)+2))))))
        
showP p = cells (color p) $ lezip $ p

test = do
  f <- openFile "font.txt" ReadMode
  e <- readFont f
  hClose f
  let x = [(font e 'X'),(font e 'A')]
  HGL.runGraphics $ do
    w <- HGL.openWindowEx
         "Led Display"
         Nothing
         --(ppc * 64, ppc * 64)
         (ppc * 5 * 20, ppc * 7 * 2)
         HGL.DoubleBuffered
         (Just 50)
    HGL.clearWindow w
    --life w a
    let p = concatPixels $map (\(a,b)-> evalE a b)  
            $zip [Up,Down,E.Right,E.Left,UpsideDown,Negative] $map (font e) "AAAAAAA"
        
--    HGL.setGraphic w $ HGL.overGraphics $ cells $ lezip $ evalE (Color HGL.Blue) p 
    HGL.setGraphic w $ HGL.overGraphics $ showP $ evalE (Color HGL.White) (font e 'A')
--    evalL w (Color HGL.Blue) p
--    check w (cycle [(Color HGL.Blue),Up,Up,(Color HGL.Red),Up,Up]) p
    HGL.getWindowTick w
    
    HGL.getKey w
    HGL.closeWindow w

--evalL :: Effects -> Pixels -> IO
-- check w (e:es) p = do
--   let d = evalE e p
--   evalL w e d 
--   check w es d
  
-- evalL w e p = HGL.setGraphic w $ HGL.overGraphics $ showP p