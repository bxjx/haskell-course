module Exercise4 where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs


fun1' :: [Integer] -> Integer
fun1' = foldl (*) 1 . map (-2 +) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3*n + 1)

-- had to look this up!
fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate calc
  where calc x = if even x then x `div` 2 else 3 * x + 1

data FTree a = FLeaf | FNode Integer (FTree a) a (FTree a)
  deriving (Show, Eq)

insertNode :: a -> FTree a -> FTree a
insertNode a FLeaf = FNode 0 FLeaf a FLeaf
insertNode a (FNode _ lnode i rnode)
  | fheight rnode <= fheight lnode = FNode (fheight newRight + 1) lnode i newRight
  | otherwise = FNode (fheight newLeft + 1) newLeft i rnode
  where 
    newRight = insertNode a rnode
    newLeft = insertNode a lnode

fheight :: FTree a -> Integer
fheight FLeaf = -1
fheight (FNode h _ _ _) = h


foldTree :: [a] -> FTree a
foldTree xs = foldr insertNode FLeaf xs

xor :: [Bool] -> Bool
xor xs = foldr (/=) False xs
