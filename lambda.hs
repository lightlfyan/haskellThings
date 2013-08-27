module Main where

main = putStrLn "lambda calcaulate"

n0 = \s z -> z
n1 = \s z -> s(z)
n2 = \s z -> s(s(z))
n3 = \s z -> s(s(s(z)))

add = \w y x -> y ( w y x)

mul = \x y z -> x (y z)


true = \x y -> x
false = \x y -> y

iand = \x y -> x y false
ior = \x y -> x true y
inot = \x -> x false true

tmp_end = putStrLn "be cured"