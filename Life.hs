import qualified Data.List as DL
import qualified Graphics.HGL as G
import qualified System.Random as R
import qualified Control.Monad as CM

type Posicion = (Int,Int)
type Tablero  = [Posicion]

altura  = 64
anchura = 64
ppc     = 8

estaVivo :: Tablero -> Posicion -> Bool
estaVivo t p = elem p t

estaMuerto :: Tablero -> Posicion -> Bool
estaMuerto b p = not (estaVivo b p)

vecindad :: Posicion -> [Posicion]
vecindad (x,y) =  map bordes [(x-1,y-1), (x,y-1),
                              (x+1,y-1), (x-1,y),
                              (x+1,y)  , (x-1,y+1),
                              (x,y+1)  , (x+1,y+1)] 
 
bordes :: Posicion -> Posicion
bordes (x,y) = (((x-1) `mod` anchura) + 1, ((y-1) `mod` altura + 1))

vecinosVivos :: Tablero -> Posicion -> Int
vecinosVivos b =  length . filter (estaVivo b) . vecindad

sobrevivientes :: Tablero -> [Posicion]
sobrevivientes b =  [p | p <- b, elem (vecinosVivos b p) [2,3]]

nacimientos b =  [p | p <- DL.nub (concat (map vecindad b)),
                      estaMuerto b p,
                      vecinosVivos b p == 3]

generacion :: Tablero -> Tablero
generacion b =  sobrevivientes b ++ nacimientos b


-- Dibujitos coloridos

cell :: Posicion -> G.Graphic
cell (c,f) = G.withColor G.White $ G.ellipse (f*ppc,c*ppc) ((f+1)*ppc,(c+1)*ppc)

cells :: Tablero -> [G.Graphic]
cells t = map cell t

main = do
          G.runGraphics $ do
          w <- G.openWindowEx
                 "Game of Life"
                 Nothing
                 (ppc * anchura, ppc * altura)
                 G.DoubleBuffered
                 (Just 50)
          G.clearWindow w
          start <- initialState
          life w start
          G.closeWindow w

life w t = do
              G.setGraphic w $ G.overGraphics $ cells t
              G.getWindowTick w
              life w $ generacion t

-- Algunos tableros iniciales
-- Comenzar con glider, luego beacon y dejar gliderGun para el final.

initialState = randomState

glider :: IO Tablero
glider = 
  return $
    [ (4,2),(2,3),(4,3),(3,4),(4,4) ]

beacon :: IO Tablero
beacon = 
  return $ 
    [ 
      (2,2), (2,3), (3,2), (3,3),
      (4,4), (4,5), (5,4), (5,5) 
    ]

gliderGun :: IO Tablero
gliderGun = 
  return $ 
    [ 
      (2,26),
      (3,24), (3,26),
      (4,14), (4,15), (4,22), (4,23), (4,36), (4,37),
      (5,13), (5,17), (5,22), (5,23), (5,36), (5,37),
      (6,2),  (6,3),  (6,12), (6,18), (6,22), (6,23),
      (7,2),  (7,3),  (7,12), (7,16), (7,18), (7,19), (7,24), (7,26),
      (8,12), (8,18), (8,26),
      (9,13), (9,17),
      (10,14), (10,15) 
    ]

randomState :: IO Tablero
randomState = do
  celdas        <- R.randomRIO (100,500)
  let filaAlAzar    = R.randomRIO (1,altura)
      columnaAlAzar = R.randomRIO (1,anchura)
  CM.replicateM celdas (CM.liftM2 (,) filaAlAzar columnaAlAzar)
  
