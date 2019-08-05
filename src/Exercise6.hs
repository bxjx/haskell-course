module Exercise6 where
-- import Debug.Trace

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
-- fib n = trace ("n: " ++ show n) $ fib (n - 1) + fib (n - 2)
fib n = sum $ map fib [(n - 1), (n - 2)]
