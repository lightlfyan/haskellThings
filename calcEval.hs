module Main(f, main) where

-- play

f :: Int -> [String] -> Int
f a (op:"(":rest) = f a (op:(show (f1 rest)):[])
f a (")":rest) = a
f a ("+":b:"*":rest) = a + (f (read b) ("*":rest))
f a ("+":b:rest) = f (a + read b) rest
f a ("*":b:rest) = f (a * read b) rest
f a [] = a

f1 :: [String] -> Int
f1 (a:rest) = f (read a) rest

main :: IO()
main = putStrLn "ok"
