-- skips :: [a] -> [[a]]
-- skips [t] = [[t]]
-- skips [] = []
-- skips xs = skip_iter xs 1
--     where
--         skip_iter :: [a] -> Int ->[[a]]
--         skip_iter ss n | n == (length ss) = [[last ss]]
--                        | otherwise = (map snd $ filter (\(i,s) -> (i `mod` n ==0)) $ zip [1..] ss) : (skip_iter ss (n + 1))




localMaxima :: [Integer] -> [Integer]
localMaxima xs = map snd $ filter 
                        (\(a,b)-> (if a == 0 || a == ((length xs) - 1) 
                                then False 
                                    else (xs !! (a - 1)) < b && b > (xs !! (a + 1))))  $ zip [0..] xs


histogram :: [Integer] -> String
histogram xs = go res ++ "==========\n0123456789\n"
    where 
        f i xs =  foldl (+) 0 ( map fst $ filter (\(a,x) -> x == i) $ zip [1,1..] xs)
        res = map (\a -> f a xs) [0..9]
        all_zero xs = foldl (&&) True $ map (\x -> if (x <= 0) then True else False ) xs
        go res = if (all_zero res) then "" else (go (map (\x -> (x -1)) res)) 
                    ++ (map (\x -> if x > 0 then '*' else ' ') res) ++ "\n" 
                          
        