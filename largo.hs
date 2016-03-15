largo :: [a] -> Int
-- largo [] = 0
-- largo (x:xs) = dale 1 xs
--   where dale l (x:xs) = dale (l + 1) xs
--         dale l [] = l

largo xs = dale 0 xs
  where dale acc xs
          | null xs    = acc
          | otherwise = dale (acc+1) (tail xs)
