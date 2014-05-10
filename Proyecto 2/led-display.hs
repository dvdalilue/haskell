import Pixels
import Effects as E
import Data.List
import System.IO
import System.Exit
import System.Environment
import qualified Data.Map as Map
import qualified Graphics.HGL as HGL
import Control.Concurrent

--hGetNextLine :: Handle -> IO String
hGetNextLine handle = do
  b <- hIsEOF handle
  if b then return "EOF" else go handle
    where go h = do
            l <- hGetLine h
            if (null . words) l then hGetNextLine handle
              else return l

-- hGetNextLineTrim :: Handle -> IO [String]
-- hGetNextLineTrim handle = do
--   l <- hGetNextLine handle
--   return $ words l

hGetKey :: Handle -> IO Char
hGetKey handle = do
  c <- hGetNextLine handle
  let d = reverse $ dropWhile (== ' ') c
  let e = reverse $ dropWhile (== ' ') d
  return $ (head . tail) e

hGetValue :: Handle -> Int -> Int -> IO [[Pixel]]
hGetValue handle width height = dale handle width height []
  where dale h a l acc
          | length acc == l = return $ reverse acc
          | otherwise       = dao h a l acc
            where dao h a l acc = do
                    b <- hIsEOF h
                    if b then dale h a l $ take l $ repeat [(Pixel False)]
                      else go h a l acc
                      where go h a l acc = do
                              v <- hGetLine h
                              dale h a l $ (fun (fillS v a) a []):acc
                              where fun s w acc
                                      | null s    = if length acc == w then reverse acc
                                                    else error "\nMal Dimension De Columnas"
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
  if eof then return macc else go macc h (a:l:ss)
    where go macc h (a:l:ss) = do 
            key   <- hGetKey h
            value <- hGetValue h (read a::Int) (read l::Int)
            if spc key value then fCatch (Map.insert key (Pixels HGL.White value) macc) h (a:l:ss)
              else fCatch macc h (a:l:ss) 
              where spc k v
                      | k /= 'O' = True
                      | k == 'O' = if (or $ map or $ map (map on) v) then True else False

hGetNextChar :: Handle -> IO Char
hGetNextChar handle = do
  b <- hIsEOF handle
  if b then return '\0' else go handle
    where go handle = do
            c <- hGetChar handle
            if c == ' '
               || c == '\n'
               || c == '\t' then hGetNextChar handle
              else return c

hGetNext :: Handle -> IO String
hGetNext handle = do
  a <- hGetNextChar handle
  if a == '\0' then return "Final" else dale handle [a]
    where dale h acc = do
            c <- hGetChar h
            if c == ' '
               || c == '\n'
               || c == '\t' then return (reverse acc)
              else dale h (c:acc)
          
removeShit s = dale s []
  where dale [] acc     = reverse acc
        dale (s:ss) acc = if (s == '\t') then dale ss acc else dale ss $ s:acc

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
  where dale (Forever (e:es)) _   = (e, es)
        dale (Repeat i (e:es)) ss = (e, es ++ ss)
        dale e es                 = (e, es)

hGetString :: Handle -> IO String
hGetString handle = do
  c <- hGetNextChar handle
  if c == '\"' then dale handle [c] else hShowError handle "Algun Say no tiene String"
    where dale h acc = do
            b <- hIsEOF h
            a <- hGetChar h
            if b then (hShowError handle "Algun Say no tiene su String cerrado con comillas")
              else if a == '\"' then return (reverse (a:acc))
                   else dale h (a:acc)

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

getCharStr :: String -> (Char,String)
getCharStr s = dale s '\0'
  where dale s o
          | null s    = (o,s)
          | otherwise = ((head s), (tail s)) 

getString :: String -> (String,String)
getString st = if null st then error "No hay palabra" else dale (getCharStr st) []
  where dale (c,s) acc
          | null s                = (c:acc,s)
          | c == ' ' || c == '\n' = ((reverse acc), s)
          | otherwise             = dale (getCharStr s) $ c:acc

sexy :: [String] -> [Effects]
sexy s = dale s []
  where dale s acc
          | null s    = reverse acc
          | otherwise = dale (tail s) $ (read (head s) :: Effects):acc

pla :: [Effects] -> Int
pla es = dale es 0
  where dale efs acc
          | null efs  = acc
          | otherwise = manage (head efs) (tail efs) acc
          where manage (Say s) es acc         = if length s > acc then dale es $ length s
                                                else dale es acc 
                manage (Forever oths) es acc  = dale oths acc
                manage (Repeat _ oths) es acc = dale (oths ++ es) acc
                manage _ es acc               = dale es acc

listEffects :: [FilePath] -> IO [Effects]
listEffects ss = 
   do
     xs <- mapM (\c->openFile c ReadMode) ss
     ys <- mapM readDisplayInfo xs
     mapM hClose xs
     return $ concat ys

readFont :: Handle -> IO (Map.Map Char Pixels)
readFont h = do
  size <- hGetNextLineTrim h
  let a = read (head size) :: Int
      l = read (last size) :: Int
      b = Pixels HGL.White $ take l $ repeat $ take a $ repeat $ Pixel True
  fCatch (Map.singleton '\0' b) h size
    where
      hGetNextLineTrim handle = 
        do
          l <- hGetNextLine handle
          return $ words l


readDisplayInfo :: Handle -> IO [Effects]
readDisplayInfo handle = dale handle []
  where dale h acc = do
          b <- hIsEOF h
          if b then return $ sexy $ reverse acc
            else other h acc
                 where other h acc = do
                         g <- hGetNext h
                         if g == "Forever" then forever h g acc
                           else if g == "Say" then say h g acc 
                                else if g == "Delay" || g == "Color" then deco h g acc
                                     else if g == "Repeat" then repeat h g acc 
                                          else if g == "Up"
                                                  || g == "Down"
                                                  || g == "Left"
                                                  || g == "Rigth"
                                                  || g == "Backwards"
                                                  || g == "UpsideDown"
                                                  || g == "Negative" then dale h $ g:acc
                                               else if g == "Final" then dale h acc
                                                 else error "Hay un efecto desconocido"
                           where forever h g acc = do
                                   a <- hGetArray h
                                   dale h $ (g ++ " " ++ a):acc
                                 say h g acc     = do
                                   c <- hGetString h
                                   dale h $ (g ++ " " ++ c):acc
                                 deco h g acc    = do
                                   d <- hGetNext h
                                   dale h $ (g ++ " " ++ d):acc
                                 repeat h g acc  = do
                                   e <- hGetNext h
                                   i <- hGetArray h
                                   dale h $ (g ++ " " ++ e ++ " " ++ i):acc

----------------------------------------
-- Main Led-Display
----------------------------------------

ppc = 5

main :: IO ()
main = do
  argv <- getArgs
  if length argv > 1 then begin argv
    else putStrLn "La cantidad de argumentos pasados por comando no es suficiente "
    where begin argv = 
            do
              f    <- openFile (head argv) ReadMode          -- Archivo de Fonts
              e    <- readFont f                             -- Map Char Pixels
              
              hClose f
              f    <- openFile (head argv) ReadMode          -- Archivo de Fonts(AGAIN)
              hClose f
              
              g <- listEffects (tail argv)
              
              ledDisplay e g

-- | 'ledDisplay' funcion encargada de la impresion de los Pixels segun los font
-- especificados
ledDisplay e g =
  do
    let max_word = pla g -- Tamaño maximo de palabra
        p_col = length $head $dots $e Map.! '\0'
        p_fil = length $dots $e Map.! '\0'
        p = concatPixels $ take max_word $ repeat $ allOff $ e Map.! '\0'
        es = evalL g

    HGL.runGraphics $ do
      w <- HGL.openWindowEx
           "Led Display"
           Nothing
           (ppc * p_col * max_word, ppc * p_fil)
           HGL.DoubleBuffered
           (Just 50)
      HGL.clearWindow w
      id <- forkIO $leD w p es e max_word
      checkEsc id w
  where checkEsc id w =       
          do
            e <- HGL.getKey w
            if not (HGL.isEscapeKey e) then checkEsc id w
              else
              do
                killThread id
                HGL.closeWindow w
                
        leD w p [] _ _ = 
          do
            HGL.setGraphic w $ HGL.overGraphics $ showP p
            HGL.getKey w
            HGL.closeWindow w
    
        leD w p ((Delay i):fx) e m =
          do
            HGL.setGraphic w $ HGL.overGraphics $ showP p
            HGL.getWindowTick w
            sleepDelay (fromInteger i ::Int)
            leD w p fx e m
              where sleepDelay ms = do
                      threadDelay $ 1000 * ms
            
        leD w p (f:fx) e m = 
          do
            HGL.setGraphic w $ HGL.overGraphics $ showP p
            HGL.getWindowTick w
            leD w (evalE f p e m) fx e m


        showP p = cells (color p) $ lezip $ p
          where lezip xs = concatMap removeNull $tablero 0 $ map (map on) $dots xs
                  where removeNull = filter (not . (\c->c == -1) . fst)
                        tablero _ [] = []
                        tablero i (x:xs) = (puntos i 0 x) : (tablero (i+1) xs )
                          where puntos _ _ []     = []
                                puntos x y (p:ps) = let a = if p then (x,y)
                                                            else (-1,-1) in a :
                                                                            puntos x (y+1) ps

        cells d t = map cell t 
          where cell (c,f) = HGL.overGraphic
                             (HGL.withColor d ( HGL.regionToGraphic ( HGL.rectangleRegion ((f*ppc)+1,(c*ppc)+1) ((((f+1)*ppc)-1),(((c+1)*ppc)-1)))))
                             (HGL.withColor HGL.Black ( HGL.regionToGraphic ( HGL.rectangleRegion (f*ppc,c*ppc) ((f+1)*ppc,(c+1)*ppc))))
