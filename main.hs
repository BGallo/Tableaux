source = "-> a b"

b = words source

data Tree = Leaf String Bool | Node String Bool Tree Tree deriving (Show)

main = putStrLn (b!!2)