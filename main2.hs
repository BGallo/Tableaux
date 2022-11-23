import Data.List
import Data.Map
import Data.Maybe

data Tree = Leaf {opr :: String, eval :: Bool} | DBranch {opr :: String, eval :: Bool, leftt :: Tree, rightt :: Tree} | SBranch {opr :: String, eval :: Bool, leftt :: Tree} deriving Show
data Formula = Var{op::String, deriv::Int, isop::Bool, ref::Int} | Not{op::String, child::Formula, deriv::Int, isop::Bool, ref::Int} | And{op::String, left::Formula, right::Formula, deriv::Int, isop::Bool, ref::Int} | Or{op::String, left::Formula, right::Formula, deriv::Int, isop::Bool, ref::Int} | Imp{op::String, left::Formula, right::Formula, deriv::Int, isop::Bool, ref::Int} deriving (Show, Eq)

instance Ord Formula where
    compare x y = compare (deriv x) (deriv y)

buildFormula :: String -> Formula
buildFormula formula = head $ Data.List.foldl pmatcher [] $ reverse $ words formula
    where pmatcher(x:y:ys) "^" = (And{op = "^", left = x, right = y, deriv = 0, isop = True, ref = 0}):ys
          pmatcher(x:y:ys) "v" = (Or{op = "v", left = x, right = y, deriv = 0, isop = True, ref = 0}):ys
          pmatcher(x:y:ys) "->" = (Imp{op = "->", left = x, right = y, deriv = 0, isop = True, ref = 0}):ys
          pmatcher(x:xs) "~" = (Not{op = "~", child = x, deriv = 0, isop = True, ref = 0}):xs
          pmatcher xs operand = (Var{op = operand, deriv = 0, isop = False, ref = 0}):xs

buildTree :: String -> Tree
buildTree formula = head $ Prelude.foldl pmatcher [] $ reverse $ words $ formula
    where pmatcher(x:y:ys) "^" = (DBranch{opr = "^", eval = True, leftt = x, rightt = y}):ys
          pmatcher(x:y:ys) "v" = (DBranch{opr = "v", eval = True, leftt = x, rightt = y}):ys
          pmatcher(x:y:ys) "->" = (DBranch{opr = "->", eval = True, leftt = x, rightt = y}):ys
          pmatcher(x:xs) "~" = (SBranch{opr = "~", eval = True, leftt = x}):xs
          pmatcher xs operand = (Leaf{opr = operand, eval = True}):xs

vTree :: Tree -> Tree
vTree tree
    | oprr == "^" = tree{eval = True, leftt = vTree $ leftt tree, rightt = vTree $ rightt tree}-- REGRA 3
    | oprr == "v" = tree{eval = True,leftt = vTree $ leftt tree,rightt = vTree $ rightt tree} -- TODO -- REGRA 5
    | oprr == "->" = tree{eval = True,leftt = fTree $ leftt tree, rightt = vTree $ rightt tree} -- TODO -- REGRA 1
    | oprr == "~" = tree{eval = True, leftt = fTree $ leftt tree} -- REGRA 7
    | otherwise = tree{eval = True}
    where oprr = opr tree

fTree :: Tree -> Tree
fTree tree
    | oprr == "^" = tree{eval = False,leftt = fTree $ leftt tree,rightt = fTree $ rightt tree } -- TODO -- REGRA 4
    | oprr == "v" = tree{eval = False, leftt = fTree $ leftt tree, rightt = fTree $ rightt tree} -- REGRA 6
    | oprr == "->" = tree{eval = False, leftt = vTree $ leftt tree , rightt = fTree $ rightt tree } -- REGRA 2
    | oprr == "~" = tree{eval = False, leftt = vTree $ leftt tree} -- REGRA 8
    | otherwise = tree{eval = False}
    where oprr = opr tree

childIsOp :: Formula -> Bool
childIsOp formula
    | opr == "^" || opr == "v" || opr == "->" = True
    | otherwise = False
    where opr = op (child formula)

lSubformulaIsOp :: Formula -> Bool
lSubformulaIsOp formula
    | opr == "^" || opr == "v" || opr == "->" = True
    | otherwise = False
    where opr = op (left formula)

rSubformulaIsOp :: Formula -> Bool
rSubformulaIsOp formula
    | opr == "^" || opr == "v" || opr == "->" = True
    | otherwise = False
    where opr = op (right formula)

printFormula :: Formula -> String
printFormula formula
    | opr == "^" || opr == "v" || opr == "->" = (if lSubformulaIsOp formula then "(" ++ printFormula (left formula) ++ ")" else printFormula (left formula)) ++ " " ++ opr ++ " " ++ (if rSubformulaIsOp formula then "(" ++ printFormula (right formula) ++ ")" else printFormula (right formula))
    | opr == "~" = if childIsOp formula then opr ++ "(" ++ printFormula (child formula) ++ ")" else opr ++ printFormula (child formula)
    | otherwise = opr
    where opr = op formula

betterPrintFormulas :: [[Formula]] -> String
betterPrintFormulas formulas = intercalate "\n\n" (Data.List.map printFormulas formulas)

printFormulas :: [Formula] -> String
printFormulas formula = intercalate "\n" (Data.List.map printFormula formula)

negateFormula :: Formula -> Formula
negateFormula formula = Not{op="~", child=formula, deriv=0, isop=True, ref=0}

recNegateFormula :: Formula -> Int -> Int -> [Formula]
recNegateFormula formula inc count
    | opr == "~" && op (child formula) == "->" = [updateDeriv (left $ child formula) 0 count] ++ [Not{op="~", child=right $ child formula, deriv=deriv formula, isop=True, ref=count}] ++ recNegateFormula (updateDeriv (left $ child formula) 0 count) inc (count+1) ++ recNegateFormula (Not{op="~", child=right $ child formula, deriv=deriv formula, isop=True, ref=count}) inc (count+1)
    | opr == "~" && op (child formula) == "^" = [updateDeriv (left $ child formula) (inc*5) count] ++ [updateDeriv (right $ child formula) (inc) count] ++ recNegateFormula(updateDeriv (left $ child formula) (inc*5) count) (inc `div` 2) (count+1) ++ recNegateFormula(updateDeriv (right $ child formula) (inc) count) (inc `div` 2) (count+1) -- AQUI
    | opr == "~" && op (child formula) == "v" = [Not{op="~", child=left $ child formula, deriv=deriv formula, isop=True, ref= count}] ++ [Not{op="~", child=right $ child formula, deriv=deriv formula, isop=True, ref=count}] ++ recNegateFormula (Not{op="~", child=left $ child formula, deriv=deriv formula, isop=True, ref=count}) inc (count+1) ++ recNegateFormula (Not{op="~", child=right $ child formula, deriv=deriv formula, isop=True, ref=count}) inc (count+1)
    | opr == "~" && op (child formula) == "~" = [updateDeriv (child $ child formula) 0 count] ++ recNegateFormula (updateDeriv (child $ child formula) 0 count) inc (count+1)
    | opr == "->" = [Not{op="~", child=updateDeriv (left formula) (inc*5) count, deriv=(inc*5) + deriv formula, isop=True, ref=count}] ++ [updateDeriv (right formula) (inc) count] ++ recNegateFormula (Not{op="~", child=updateDeriv (left formula) (inc*5) count, deriv=(inc*5) + deriv formula, isop=True, ref=count}) (inc `div` 2) (count+1) ++ recNegateFormula (updateDeriv (right formula) (inc) count) (inc `div` 2) (count+1) -- AQUI
    | opr == "^" = [updateDeriv (left formula) 0 count] ++ [updateDeriv (right formula) 0 count] ++ recNegateFormula (updateDeriv (left formula) 0 count) inc (count+1) ++ recNegateFormula (updateDeriv (right formula) 0 count) inc (count+1)
    | opr == "v" = [updateDeriv (left formula) (inc*5) count] ++ [updateDeriv (right formula) (inc) count] ++ recNegateFormula (updateDeriv (left formula) (inc*5) count) (inc `div` 2) (count+1) ++ recNegateFormula (updateDeriv (right formula) (inc) count) (inc `div` 2) (count+1) -- AQUI
    | otherwise = []
    where opr = op formula

updateDeriv :: Formula -> Int -> Int -> Formula
updateDeriv formula inc count = formula{deriv=inc + deriv formula, ref=count}

igual :: Formula -> Formula -> Bool
igual x y = if (deriv x) == (deriv y) then True else False

xpto :: [Formula] -> [[Formula]]
xpto x = groupBy (igual) (x)

pUtil :: String -> String -> Map String String-> Map String String
pUtil x y z = Data.Map.insert x y z

aUtil :: Formula -> Map String String ->String
aUtil x y
    |  ((member (op x) y)  && not ( y Data.Map.! (op  x) == (if (op   x == "~") then "FALSE" else "TRUE") )) =  " A tem contradicao \n "
    | (op (x)) == "~" =  "" ++ aUtil (child x)  (Data.Map.insert (op (child  x)) ((if (op  x == "~") then "FALSE" else "TRUE"))  y)
    |(op x == "v") ||(op x == "^") ||(op x == "->") = ""++ aUtil (left x) (Data.Map.insert (op( left x)) (((if (op  x == "~") then "FALSE" else "TRUE")))y) ++ aUtil (right x)(Data.Map.insert (op( right x)) (((if (op  x == "~") then "FALSE" else "TRUE")))y)
    | otherwise = ""

aParser :: [Formula] -> Map String String-> String
aParser x y 
    | (Data.List.length x == 0) = "" 
    | ((member (op (head x)) y)  && not ( y Data.Map.! (op (head x)) == (if (op (head  x) == "~") then "FALSE" else "TRUE") )) =  " A tem contradicao \n " 
    | (op (head x)) == "~" =  "" ++ aParser (Data.List.drop 1 x)  (Data.Map.insert (op (child (head x))) ((if (op (head  x) == "~") then "FALSE" else "TRUE"))  y)
    | otherwise = ""++ aParser (Data.List.drop 1 x)  (Data.Map.insert (op (head x)) ((if (op (head  x) == "~") then "FALSE" else "TRUE"))  y)

avaliar :: Tree->String
avaliar t
    | (oprr == "^" ) && ((eval t) == False) = (avaliar (leftt t) ) ++ ""++(avaliar (rightt t) )
    | (oprr == "^" ) && ((eval t) == True) = (avaliar (leftt t) ) ++ ""++ (avaliar (rightt t) )
    | (oprr == "v" ) && ((eval t) == False) = (avaliar (leftt t)) ++ ""++ (avaliar (rightt t) )
    | (oprr == "v" ) && ((eval t) == True) = (avaliar (leftt t) )++ ""++(avaliar (rightt t) )
    | (oprr == "->" ) && ((eval t) == False) = (avaliar (leftt t) )++ ""++(avaliar (rightt t) )
    | (oprr == "->" ) && ((eval t) == True) = (avaliar (rightt t) )++ ""++(avaliar (rightt t) )
    | (oprr == "~") && ((eval t )== False) = "" ++(avaliar (leftt t) )
    | (oprr == "~") && ((eval t )== True) = "" ++ (avaliar (leftt t) )
    | otherwise = " " ++ (treeToStr t 1)
    where oprr = opr t

treeToStr :: Tree -> Int -> String
treeToStr tree count
    | oprr == "->" = replicate count '-' ++ " " ++ treeToInfix tree ++ ": " ++ show ev ++ "\n" ++  leftt tree `treeToStr` (count + 3) ++ rightt tree `treeToStr` (count + 3)
    | oprr == "^" || oprr == "v" = replicate count '-' ++ " " ++ treeToInfix tree ++ " : " ++ show ev ++ "\n" ++  leftt tree `treeToStr` (count + 3) ++ rightt tree `treeToStr` (count + 3)
    | oprr == "~" = replicate count '-' ++ " " ++ treeToInfix tree ++ " : " ++ show ev ++ "\n" ++  leftt tree `treeToStr` (count + 3)
    | otherwise = replicate count '-' ++ " " ++ oprr ++ " : " ++ show ev ++ "\n"
    where oprr = opr tree
          ev = eval tree

treeToInfix :: Tree -> String
treeToInfix tree
    | oprr == "^" || oprr == "v" || oprr == "->" = (if lSubtreeIsOp tree then "(" ++ treeToInfix (leftt tree) ++  ")" else treeToInfix (leftt tree)) ++ " " ++ oprr ++ " " ++ (if rSubtreeIsOp tree then "(" ++ treeToInfix (rightt tree) ++  ")" else treeToInfix (rightt tree))
    | oprr == "~" = oprr ++ " " ++ (if lSubtreeIsOp tree then "(" ++ treeToInfix (leftt tree) ++  ")" else treeToInfix (leftt tree))
    | otherwise = oprr
    where oprr = opr tree

lSubtreeIsOp :: Tree -> Bool
lSubtreeIsOp tree
    | oprr == "^" || oprr == "v" || oprr == "->" = True
    | otherwise = False
    where oprr = opr (leftt tree)

rSubtreeIsOp :: Tree -> Bool
rSubtreeIsOp tree
    | oprr == "^" || oprr == "v" || oprr == "->" = True
    | otherwise = False
    where oprr = opr (rightt tree)

multiParser :: [[Formula]] -> [String]
multiParser (x:xs) = aParser x empty : multiParser xs
multiParser list = []

parseCheck :: [String] -> Bool
parseCheck strs = "" `elem` strs

teste :: Formula -> String
teste f 
    | not((op f == "v") ||(op f == "^") ||(op f == "->")||(op f == "~"))= op f
    | (op f == "~") && not ((op (child f) == "v") ||(op (child f) == "^") || (op (child f) == "->")) = op $ child f
    | otherwise = "" ++(teste $ left f ) ++ (teste $ right f)


main = do
    formula <- getLine
    let builtFormula = buildFormula formula
    let groupedSorted = xpto $ sort (negateFormula builtFormula : recNegateFormula (negateFormula builtFormula) 100 1)
    let concattedBranches = if length groupedSorted > 1 then Data.List.map (head groupedSorted ++) (tail groupedSorted) else groupedSorted
    let findings = multiParser concattedBranches
    putStrLn $ "Fórmula entrada: " ++ printFormula builtFormula
    if parseCheck findings then putStrLn $ "A fórmula é inválida. Contra-modelo:\n" ++ avaliar (fTree $ buildTree $ formula) else putStrLn $ "A fórmula é válida! Ramos calculados:\n" ++ betterPrintFormulas concattedBranches