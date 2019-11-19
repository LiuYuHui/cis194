fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> (x - 2)) . filter even 

fun2' :: Integer -> Integer
fun2' = sum . takeWhile (> 1) . iterate (\x -> if (even x) then x `div` 2 else 3 * x + 1)


data Tree a = Leaf 
            |  Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree xs = foldr insert Leaf xs
    where
        insert :: a -> Tree a -> Tree a
        insert x Leaf = Node 0 Leaf x Leaf
        insert x (Node h lst x' rst) 
            | getHeight lst < getHeight rst = updateH (Node h (insert x lst) x' rst) 
            | otherwise = updateH (Node h lst x' (insert x rst))
        
        updateH :: Tree a -> Tree a
        updateH Leaf = Leaf
        updateH (Node h lst a rst) = (Node h' l' a r')
            where
                h' = 1 + max (getHeight l') (getHeight r')
                l' = updateH lst
                r' = updateH rst

        getHeight x = case x of 
                    Leaf ->  -1
                    (Node h _ _ _) -> h


xor :: [Bool] -> Bool
xor = odd . length . filter (\x -> x) 

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a b -> f(a) : b) []
--map' f = foldr ((:) . f)  []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\a b -> f b a) base $ reverse xs

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1) . (*2)) $ filter (\x -> not . any (==x) $ xs) $ [1..n] 
    where
        xs =  filter (<= n) $ [ i + j + 2*i*j |  j <- [1..n],i <- [1..j]]

