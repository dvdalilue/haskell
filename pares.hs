-- main
main = do
  putStrLn $ "Hola" ++ show (miembro impar 3)

-- types
type Conjunto a = a -> Bool
type Par a = (a,Bool)
type Impar = Int

-- declarations
miembro :: Conjunto a -> a -> Bool
miembro = id
---------------------------------
-- Soluciones igual de validas --
-- miembro u v = u v           --
-- miembro u = u               --
-- miembro u = id u            --
---------------------------------

vacio :: Conjunto a
vacio = const False -- No devuelve nada, asi es, nada(vacio)

singleton :: (Eq a) => a -> Conjunto a
singleton = (==)

impar :: Impar -> Bool
impar 0 = False
impar 1 = True
impar u = impar (rem u 2)

esPar :: Par Int -> Bool
esPar (0,b) = True
esPar (1,b) = False
esPar (a,b) = esPar (rem a 2,b)
