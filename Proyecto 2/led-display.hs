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

hGetNextChar :: Handle -> IO Char
hGetNextChar handle = do
  b <- hIsEOF handle
  if b then return '\0' else go handle
    where go handle = do
            c <- hGetChar handle
            if (c == ' ' || c == '\n' || c == '\t') then hGetNextChar handle else return c

hGetNext :: Handle -> IO String
hGetNext handle = do
  a <- hGetNextChar handle
  dale handle [a]
    where dale h acc = do
            c <- hGetChar h
            if (c == ' ' || c == '\n' || c == '\t') then return (reverse acc) else dale h (c:acc)
          
removeShit s = dale s []
  where dale [] acc     = reverse acc
        dale (s:ss) acc = if (s == '\n' || s == '\t') then dale ss acc else dale ss $ s:acc

hGetName :: Handle -> IO String
hGetName handle = do
  s <- hShow handle
  return $ tail $ dropWhile (/= '=') $ takeWhile (/= ',') s

hShowError :: Handle -> String -> IO b
hShowError h s = do
  fileName <- hGetName h
  error $ fileName ++ ": " ++ s

headE :: [Effects] -> (Effects,[Effects])
headE [] = error "headE: Lista Vacia"
headE e  = dale (head e) $ tail e
  where dale (Forever (e:es)) _ = (e, es)
        dale e es               = (e, es)

hGetInt = hGetNext

hGetString :: Handle -> IO String
hGetString handle = do
  c <- hGetNextChar handle
  if c == '\"' then dale handle [c] else hShowError handle "Algun Say no tiene String"
    where dale h acc = do
            b <- hIsEOF h
            a <- hGetChar h
            if b then (hShowError handle "Algun Say no tiene su String cerrado con comillas") else if a == '\"' then return (reverse (a:acc)) else dale h (a:acc)

hGetColor = hGetColor

hGetArray :: Handle -> IO String
hGetArray handle = do
  c <- hGetNextChar handle
  if c == '[' then dale handle [c] 1 else hShowError handle "Algun Repeat o Forever no tiene arreglo"
    where dale h acc count = do
            b <- hIsEOF h
            a <- hGetChar h
            if b then (hShowError handle "Algun Repeat o Forever no tiene su arreglo cerrado")
              else if a == '[' then dale h (a:acc) $ count + 1
                   else if a == ']' && count == 1 then return (reverse (a:acc))
                        else if a == ']' then dale h (a:acc) $ count - 1 
                               else dale h (a:acc) count
 
--readDisplayInfo :: Handle -> IO [Effects]
--readDisplayInfo handle = dale handle []
--  where dale h acc = do
--          b <- hIsEOF h
--          if b then return []
--            else other h acc
--                 where other h acc = do
--                         g <- hGetNext h
--                         if g == "Forever" then other h $ (g ++ (hGetArray h)):acc
--                           else if g == "Say" then other h $ (g ++ (hGetString h)):acc
--                                else if g == "Delay" then other h $ (g ++ (' ':(hGetInt h))):acc

-- Creo que es mejor leer todo el string y analizarlo, ya tengo las funciones para entender cada tipo(Integer, Array, String, Color
-- Dado que no puedo sacar cosas del IO en cada condicional. Mejor sacarlo una vez, una vez y devolverlo.

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

ppc = 30

lezip xs = concatMap removeNull $tablero 0 $map (map on) $dots xs
  where removeNull = filter (not . (\c->c == -1) . fst)
        tablero _ [] = []
        tablero i (x:xs) = (puntos i 0 x) : (tablero (i+1) xs )
          where puntos _ _ []     = []
                puntos x y (p:ps) = let a = if p then (x,y)
                                            else (-1,-1) in a : puntos x (y+1) ps

cells t = map cell t
  where cell (c,f) = HGL.withColor HGL.White $ HGL.ellipse (f*ppc,c*ppc) ((f+1)*ppc,(c+1)*ppc)

test = do
  f <- openFile "font.txt" ReadMode
  e <- readFont f
  hClose f
  let a = font e 'X'
  HGL.runGraphics $ do
    w <- HGL.openWindowEx
         "Led Display"
         Nothing
         (ppc * 5*5, ppc * 7*5)
         HGL.DoubleBuffered
         (Just 50)
    HGL.clearWindow w
    HGL.setGraphic w $ HGL.overGraphics $ cells $ lezip $ a
    HGL.getWindowTick w
    
    HGL.getKey w
    HGL.closeWindow w
