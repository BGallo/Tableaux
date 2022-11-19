data Tree = Leaf {op :: String, eval :: Bool} | Branch {op :: String, eval :: Bool, left :: Tree, right :: Tree} | SBranch {op :: String, eval :: Bool, left :: Tree} deriving Show

buildTree :: String -> Tree
buildTree formula = head (foldl pmatcher [] (reverse (words (formula))))
    where pmatcher(x:y:ys) "^" = (Branch{op = "^", eval = True, left = x, right = y}):ys
          pmatcher(x:y:ys) "v" = (Branch{op = "v", eval = True, left = x, right = y}):ys
          pmatcher(x:y:ys) "->" = (Branch{op = "->", eval = True, left = x, right = y}):ys
          pmatcher(x:xs) "~" = (SBranch{op = "~", eval = True, left = x}):xs
          pmatcher xs operand = (Leaf{op = operand, eval = True}):xs

validateTree :: Tree -> Tree
validateTree tree
    | opr == "^" = tree{eval = True, left = validateTree $ left tree, right = validateTree $ right tree} -- REGRA 3
    | opr == "v" = tree{eval = True} -- TODO -- REGRA 5
    | opr == "->" = tree{eval = True} -- TODO -- REGRA 1
    | opr == "~" = tree{eval = True, left = negateTree $ left tree} -- REGRA 7
    | otherwise = tree{eval = True}
    where opr = op(tree)

negateTree :: Tree -> Tree
negateTree tree
    | opr == "^" = tree{eval = False} -- TODO -- REGRA 4
    | opr == "v" = tree{eval = False, left = negateTree $ left tree, right = negateTree $ right tree} -- REGRA 6
    | opr == "->" = tree{eval = False, left = validateTree $ left tree , right = negateTree $ right tree } -- REGRA 2
    | opr == "~" = tree{eval = False, left = validateTree $ left tree} -- REGRA 8
    | otherwise = tree{eval = False}
    where opr = op(tree)

--evaluateTree :: Tree -> IO ()

--printTree :: Tree -> Int -> IO ()
--printTree tree dashes = 

main = do
    let formula = "-> a -> a -> b a"
    print(negateTree $ buildTree formula)