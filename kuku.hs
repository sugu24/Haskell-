kuku n = putStr s
  where
    s = foldr (++) "" [foldr marge "\n" $ map show $ map (x*) [1..n] | x <- [1..n]]
    marge x y = if length(x) < 2 then ("  " ++ x ++ y) else (" " ++ x ++ y) 

main = kuku 9