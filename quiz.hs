-- Dada esta estructura recursiva

data Inception a = Dream a | Deeper [Inception a]
                             deriving Show

-- Implantar una funcion que devuelva una lista con los valores de cada suenio
-- Ex. Deeper [Dream 74, Deeper [Dream 7, Dream 2, Dream 9], Dream 42]
-- Output. [74,7,2,9,42]

dreams :: Inception a -> [a]
dreams l = reverse $ dreamlist l []
  where dreamlist (Dream n) acc = n:acc
        dreamlist (Deeper ds) acc = agrupar ds acc
          where agrupar [] acc     = acc
                agrupar (x:xs) acc = agrupar xs $ dreamlist x acc

-- Escribir una funcion de map y fold para el tipo de datos

mapI :: (a -> b) -> [Inception a] -> [Inception b]
mapI f l = dale f l []
  where dale f [] acc     = reverse acc
        dale f (x:xs) acc = dale f xs (lol f x acc)
          where lol f (Dream n) acc   = Dream (f n) : acc
                lol f (Deeper ds) acc = Deeper (mapI f ds) : acc

-- mapI f l = map (dale f) l
--   where dale f (Dream n)   = Dream (f n)
--         dale f (Deeper ds) = Deeper (mapI f ds)

foldI :: (a -> b -> a) -> a -> [Inception b] -> a
foldI f b []     = b
foldI f b (x:xs) = foldI f (dale f b x) xs
  where dale f b (Dream n)   = f b n
        dale f b (Deeper ds) = foldI f b ds

-- Compacta los elementos repetidos contiguos(recursion de cola, funciones de orden superior)
-- Ex. [1,1,1,1,2,3,3,3,4,4,4,5,5,5,6,6,7,7,8]
-- Output. [[1,1,1,1],[2],[3,3,3],[4,4,4],[5,5,5],[6,6],[7,7],[8]]

groups :: Eq a => [a] -> [[a]]
groups l =  dale l [] $ span (== (head l)) l
  where dale l acc (f,s)
          | null l    = reverse acc
          | otherwise = dale s (f:acc) $ span (== (head s)) s
