import Data.List
import Data.Map
import qualified Distribution.Simple.Program.HcPkg as Data
import qualified Data.Sequence as Data.Map
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
boolToString :: Bool -> String
boolToString True = "TRUE"
boolToString False = "FALSE"
--evalBranches :: Tree -> IO ()
avaliar :: Tree->[String]->String
avaliar t lista  
    |(( ( case opr `elemIndex` lista of 
        Just n ->  lista !! (n+1)
        maybe -> ""))) == boolToString (not (eval t)) = "Contradição" 
    | (opr == "^" ) && ((eval t) == False) = (avaliar (left t) []) ++ " "++(avaliar (right t) [])
    | (opr == "^" ) && ((eval t) == True) = (avaliar (left t) lista) ++ " "++ (avaliar (right t) lista)
    | (opr == "v" ) && ((eval t) == False) = (avaliar (left t) lista) ++ " "++ (avaliar (right t) lista)
    | (opr == "v" ) && ((eval t) == True) = (avaliar (left t) [])++ " "++(avaliar (right t) [])
    | (opr == "->" ) && ((eval t) == False) = (avaliar (left t) [])++ " "++(avaliar (right t) [])
    | (opr == "->" ) && ((eval t) == True) = (avaliar (right t) lista)++ " "++(avaliar (right t) lista)
    | (opr == "~") && ((eval t )== False) = " " ++(avaliar (left t) [])
    | (opr == "~") && ((eval t )== True) = " " ++ (avaliar (left t) lista)
    | otherwise = " " ++ (treeToStr t 1)
    where opr = op t 

idk :: String -> [String]
idk str = removeall ":" $ words str
    where removeall val list = [ x | x <- list, x /= val ]

pUtil :: String -> String -> Map String String-> Map String String
pUtil x y z = Data.Map.insert x y z 

 

aParser :: [String] -> Map String String-> String
 
aParser x y 
    | (Data.List.length x == 0) = "" 
    |(Data.List.length x == 1) && ((member (head x) y)  && not ( y Data.Map.! (head x) == (x!!2))) = " A formula e valida \n " 
    | (Data.List.length x == 1)=""
    | ((member (head x) y)  && not ( y Data.Map.! (head x) == (x!!2))) = " A formula e valida \n " 
    | otherwise =  aParser (Data.List.drop 2 x)  (Data.Map.insert (head x) (x !! 2)  y)


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
    let arvore= buildTree formula
    let fArvore = fTree arvore
   
    let result = avaliar fArvore []
    let aux = idk result
    let b = aParser aux (empty)
    if b == "" then putStrLn result else  (putStrLn $ " A formula e valida \n" ++ (treeToStr  fArvore 3) ) 
