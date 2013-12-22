--main :: IO()
main = do
  putStrLn "Introdusca un numero: " 
  n <- getInt
  putStrLn $ "Factorial igual a: " ++ show (fac_cola n 1)
  -- print n

fac_cola :: Int -> Int -> Int
fac_cola 1 b = b
fac_cola n b = fac_cola ( n-1) b*n

fac :: Int -> Int
fac 1 = 1
fac n = n*fac(n-1)

getInt :: IO Int
getInt = do
  -- s <- fmap read getLine
  s <- getLine
  -- return (read s) 
  return (read s :: Int)
