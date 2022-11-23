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

funcao :: Tree -> [[Tree]] -> [[Tree]]
funcao tree arr
    | (opr == "^" ) && ((eval tree) == False) = arr ++ ([[tree]]++ (left tree) `funcao` ([]) ++ ( [[tree]]++ funcao (right tree) []))
    | (opr == "^" ) && ((eval tree) == True) = arr ++ [[tree]]++(  funcao (left tree) arr) ++ (  funcao (right tree) arr)
    | (opr == "v" ) && ((eval tree) == False) =arr ++  ([[tree]]++funcao (left tree) arr) ++ ([[tree]]++funcao (right tree) arr)
    | (opr == "v" ) && ((eval tree) == True) =arr ++ [[tree]]++ (funcao (left tree) []) ++(funcao (right tree) [])
    | (opr == "->" ) && ((eval tree) == False) =arr ++[[tree]]++  (funcao (left tree) arr) ++ (funcao (right tree) arr)
    | (opr == "->" ) && ((eval tree) == True) =arr ++  ([[tree]]++funcao (right tree) []) ++([[tree]]++funcao (right tree) [])
    | (opr == "~") && ((eval tree )== False) =arr ++ [[tree]]++ (funcao (left tree) arr)
    | (opr == "~") && ((eval tree )== True) =arr ++ [[tree]]++ (funcao (left tree) arr)
    | otherwise =arr ++   [[tree]]
    where opr = op tree

boolToString :: Bool -> String
boolToString True = "TRUE"
boolToString False = "FALSE"

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
idk str = removeall "-" $ removeall "\n" $ removeall ":" $ words str
    where removeall val list = [ x | x <- list, x /= val ]

pUtil :: String -> String -> Map String String-> Map String String
pUtil x y z = Data.Map.insert x y z 

aParser :: [String] -> Map String String-> String
aParser x y 
    | (Data.List.length x == 0) = "" 
    | ((member (head x) y)  && not ( y Data.Map.! (head x) == (x!!2))) = " A formula e valida \n " 
    | otherwise =  aParser (Data.List.drop 2 x)  (Data.Map.insert (head x) (x !! 2)  y)

lSubtreeIsOp :: Tree -> Bool
lSubtreeIsOp tree
    | opr == "^" || opr == "v" || opr == "->" = True
    | otherwise = False
    where opr = op (left tree)

rSubtreeIsOp :: Tree -> Bool
rSubtreeIsOp tree
    | opr == "^" || opr == "v" || opr == "->" = True
    | otherwise = False
    where opr = op (right tree)

treeToInfix :: Tree -> String
treeToInfix tree
    | opr == "^" || opr == "v" || opr == "->" = (if lSubtreeIsOp tree then "(" ++ treeToInfix (left tree) ++  ")" else treeToInfix (left tree)) ++ " " ++ opr ++ " " ++ (if rSubtreeIsOp tree then "(" ++ treeToInfix (right tree) ++  ")" else treeToInfix (right tree))
    | opr == "~" = opr ++ " " ++ (if lSubtreeIsOp tree then "(" ++ treeToInfix (left tree) ++  ")" else treeToInfix (left tree))
    | otherwise = opr
    where opr = op tree

--multiLineInfixTree :: Tree -> Int -> IO()
--multiLineInfixTree tree count = do
--    if isop then putStrLn $ not ++ "(" ++ treeToInfix tree ++ ") (" ++ show count ++ ")"  else putStrLn $ not ++ treeToInfix tree ++ " (" ++ show count ++ ")"
--    if opr == "^" || opr == "v" || opr == "->" then do
--        multiLineInfixTree (left tree) (count + 1)
--        multiLineInfixTree (right tree) (count + 1)
--    else if opr == "~" then multiLineInfixTree (left tree) (count + 1)
--    else return()
--    where opr = op tree
--          val = eval tree
--          isop = opr == "^" || opr == "v" || opr == "->"
--          not = if val then "" else "~ "

treeToStr :: Tree -> Int -> String
treeToStr tree count
    | opr == "->" = replicate count '-' ++ " " ++ treeToInfix tree ++ ": " ++ show ev ++ "\n" ++  left tree `treeToStr` (count + 3) ++ right tree `treeToStr` (count + 3)
    | opr == "^" || opr == "v" = replicate count '-' ++ " " ++ treeToInfix tree ++ " : " ++ show ev ++ "\n" ++  left tree `treeToStr` (count + 3) ++ right tree `treeToStr` (count + 3)
    | opr == "~" = replicate count '-' ++ " " ++ treeToInfix tree ++ " : " ++ show ev ++ "\n" ++  left tree `treeToStr` (count + 3)
    | otherwise = replicate count '-' ++ " " ++ opr ++ " : " ++ show ev ++ "\n"
    where opr = op tree
          ev = eval tree

evalFormula :: String -> IO()
evalFormula formula = putStr $ treeToStr (fTree $ buildTree formula) 1

main = do
    formula <- getLine
    let arvore= buildTree formula
    let fArvore = fTree arvore
   
    let result = funcao fArvore []
    putStrLn $ treeToStr  (head(head(result)))  1
    --let aux = idk result
    --let b = aParser aux (empty)
    
    --multiLineInfixTree fArvore 0
    --if b == "" then putStrLn $ "A fórmula é inválida. Contra-exemplo: \n" ++ result else  (putStrLn $ "A fórmula é válida. \n" ++ (treeToStr  fArvore 1) ) 
