module Tree where

data Tree a = Branch (Tree a) (Tree a) | Leaf a --deriving Show


infixl 3 !
(!) ::  Integer -> Integer
(!) n = foldl (*) 1 [1..n]


p1 f 0 = f
p1 f n = f >> p1 f (n-1)

t1 :: Tree Int
t1 = Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3)

showTree :: (Show a) => Tree a -> String
showTree (Leaf a) = show a
showTree (Branch l r) = "<"++ showTree l ++"|"++ showTree r++">"


showTree2 :: Tree String -> String -> String
showTree2 (Leaf a) s = shows a s
showTree2 (Branch l r) s = '<':showTree2 l ('|': showTree2 r ('>':s))

showTree3 :: (Show a) => Tree a -> ShowS
showTree3 (Leaf a) = shows a
showTree3 (Branch l r) = ('<':) . showTree3 l . ('|':) . showTree3 r . ('>':)

readTree :: (Read a) => ReadS (Tree a)
readTree ('<':s) = [(Branch l r, u) | (l, '|':t) <- readTree s, (r, '>':u) <- readTree t]
readTree s = [(Leaf x, t) | (x, t) <- reads s]


readTree2 :: (Read a) => ReadS (Tree a)
readTree2 s = [(Branch l r, x) | ("<", t) <- lex s,
                                  (l, u) <- readTree2 t,
                                  ("|", v) <- lex u,
                                  (r, w) <- readTree2 v,
                                  (">", x) <- lex w
                                ]
                                ++
                                [(Leaf x, t) | (x, t) <- reads s]

instance Show a => Show (Tree a) where
    show x = showTree3 x ""
