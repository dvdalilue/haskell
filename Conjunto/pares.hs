-- main
main = do
  putStrLn $ "Hola" ++ show (miembro impar 3)

-- types
type Conjunto a = a -> Bool
type Par a = (a,Bool)
type Impar = Int

-- declarations
miembro :: Conjunto a -> a -> Bool
miembro u v = u v

impar :: Impar -> Bool
impar 0 = False
impar 1 = True
impar u = impar (rem u 2)

esPar :: Par Int -> Bool
esPar (0,b) = True
esPar (1,b) = False
esPar (a,b) = esPar (rem a 2,b)
