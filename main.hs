data Tree = Leaf String Bool | Branch String Bool Tree Tree deriving Show

buildTree :: String -> Tree
buildTree formula = head (foldl pmatcher [] (reverse (words (formula))))
    where pmatcher(x:y:ys) "^" = (Branch "^" True x y):ys
          pmatcher(x:y:ys) "v" = (Branch "v" True x y):ys
          pmatcher(x:y:ys) "->" = (Branch "->" True x y):ys
          pmatcher xs operand = (Leaf operand True):xs

negateTree :: Tree -> Tree

evaluateTree :: Tree -> IO ()

printTree :: Tree -> IO ()

main = do
    let formula = "v ^ a b -> a b"
    print(buildTree formula)