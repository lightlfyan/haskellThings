module Main(f, main) where

-- play
-- f1 ["10", "+", "(", "3", "+", "4", ")", "*", "2"]

f :: Int -> [String] -> Int
f a (op:"(":rest) = let h = takeWhile (/=")") rest
                        (_:t) = dropWhile (/=")") rest
                     in f a $ op:(show $ f1 h):t
f a ("+":b:"*":rest) = a + (f (read b) ("*":rest))
f a ("-":b:"*":rest) = a - (f (read b) ("*":rest))
f a ("+":b:rest) = f (a + read b) rest
f a ("*":b:rest) = f (a * read b) rest
f a [] = a

f1 :: [String] -> Int
f1 (a:rest) = f (read a) rest

main :: IO()
main = putStrLn "ok"
