
data Tree = Leaf {op :: String, eval :: Bool} | DBranch {op :: String, eval :: Bool, left :: Tree, right :: Tree} | SBranch {op :: String, eval :: Bool, left :: Tree} deriving Show

buildTree :: String -> Tree
buildTree formula = head $ Prelude.foldl pmatcher [] $ reverse $ words $ formula
    where pmatcher(x:y:ys) "^" = (DBranch{op = "^", eval = True, left = x, right = y}):ys
          pmatcher(x:y:ys) "v" = (DBranch{op = "v", eval = True, left = x, right = y}):ys
          pmatcher(x:y:ys) "->" = (DBranch{op = "->", eval = True, left = x, right = y}):ys
          pmatcher(x:xs) "~" = (SBranch{op = "~", eval = True, left = x}):xs
          pmatcher xs operand = (Leaf{op = operand, eval = True}):xs

vTree :: Tree -> Tree
vTree tree
    | opr == "^" = tree{eval = True, left = vTree $ left tree, right = vTree $ right tree}-- REGRA 3
    | opr == "v" = tree{eval = True,left = vTree $ left tree,right = vTree $ right tree} -- TODO -- REGRA 5
    | opr == "->" = tree{eval = True,left = fTree $ left tree, right = vTree $ right tree} -- TODO -- REGRA 1
    | opr == "~" = tree{eval = True, left = fTree $ left tree} -- REGRA 7
    | otherwise = tree{eval = True}
    where opr = op tree

fTree :: Tree -> Tree
fTree tree
    | opr == "^" = tree{eval = False,left = fTree $ left tree,right = fTree $ right tree } -- TODO -- REGRA 4
    | opr == "v" = tree{eval = False, left = fTree $ left tree, right = fTree $ right tree} -- REGRA 6
    | opr == "->" = tree{eval = False, left = vTree $ left tree , right = fTree $ right tree } -- REGRA 2
    | opr == "~" = tree{eval = False, left = vTree $ left tree} -- REGRA 8
    | otherwise = tree{eval = False}
    where opr = op tree

--branchCalc :: Tree -> Int -- calculates value of n in 2^n amount of branches
--branchCalc tree

--branchList :: Tree -> [Tree]
--branchList tree

--evalBranches :: Tree -> IO ()
avaliar :: Tree->String
avaliar t  
    | list !! ((opr `elem` list)+1) == not (eval t) = "Contradição" 
    | (opr == "^" ) && ((eval t) == False) = (avaliar (left t))++(avaliar (right t))
    | (opr == "^" ) && ((eval t) == True) = (avaliar t)
    | (opr == "v" ) && ((eval t) == False) = (avaliar t)
    | (opr == "v" ) && ((eval t) == True) = (avaliar t)++(avaliar t)
    | (opr == "->" ) && ((eval t) == False) = (avaliar t)
    where opr = op t 
          list = []

treeToStr :: Tree -> Int -> String
treeToStr tree count
    | opr == "->" = replicate count '-' ++ " " ++ opr ++ ": " ++ show ev ++ "\n" ++  left tree `treeToStr` (count + 3) ++ right tree `treeToStr` (count + 3)
    | opr == "^" || opr == "v" = replicate count '-' ++ " " ++ opr ++ " : " ++ show ev ++ "\n" ++  left tree `treeToStr` (count + 3) ++ right tree `treeToStr` (count + 3)
    | opr == "~" = replicate count '-' ++ " " ++ opr ++ " : " ++ show ev ++ "\n" ++  left tree `treeToStr` (count + 3)
    | otherwise = replicate count '-' ++ " " ++ opr ++ " : " ++ show ev ++ "\n"
    where opr = op tree
          ev = eval tree

evalFormula :: String -> IO()
evalFormula formula = putStr $ treeToStr (fTree $ buildTree formula) 1

main = do
    formula <- getLine
    print(fTree $ buildTree formula)
    evalFormula formula
