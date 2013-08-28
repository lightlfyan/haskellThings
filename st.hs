module St where

newtype State s a = St (s -> (a, s))

type Thing = Int
type Things = [Thing]
type ThingHole = State Things Thing

killone :: Thing -> ThingHole
killone c = St $ \s -> (c, c:s)


sshow :: State s a -> s -> (a, s)
sshow (St f) = f

instance Monad (State s) where
    return v = St $ \s -> (v, s)
    St s1 >>= f = St $ \s ->
        let (v, s') = s1 s
            St s2 = f v
        in s2 s'

test :: ThingHole
test = do
    _ <- killone 1
    _ <- killone 2
    killone 3
