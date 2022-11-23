import Data.List
import Data.Map

data Formula = Var{op::String, deriv::Int, isop::Bool} | Not{op::String, child::Formula, deriv::Int, isop::Bool} | And{op::String, left::Formula, right::Formula, deriv::Int, isop::Bool} | Or{op::String, left::Formula, right::Formula, deriv::Int, isop::Bool} | Imp{op::String, left::Formula, right::Formula, deriv::Int, isop::Bool} deriving (Show, Eq)
instance Ord Formula where
    compare x y = compare (deriv x) (deriv y)

buildFormula :: String -> Formula
buildFormula formula = head $ Data.List.foldl pmatcher [] $ reverse $ words formula
    where pmatcher(x:y:ys) "^" = (And{op = "^", left = x, right = y, deriv = 1, isop = True}):ys
          pmatcher(x:y:ys) "v" = (Or{op = "v", left = x, right = y, deriv = 1, isop = True}):ys
          pmatcher(x:y:ys) "->" = (Imp{op = "->", left = x, right = y, deriv = 1, isop = True}):ys
          pmatcher(x:xs) "~" = (Not{op = "~", child = x, deriv = 1, isop = True}):xs
          pmatcher xs operand = (Var{op = operand, deriv = 1, isop = False}):xs

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

printFormulas :: [Formula] -> String
printFormulas formula = intercalate "\n" (Data.List.map printFormula formula)

negateFormula :: Formula -> Formula
negateFormula formula = Not{op="~", child=formula, deriv=1, isop=True}

recNegateFormula :: Formula -> [Formula]
recNegateFormula formula
    | opr == "~" && op (child formula) == "->" = [updateDeriv (left $ child formula) 1] ++ [Not{op="~", child=right $ child formula, deriv=deriv formula, isop=True}] ++ recNegateFormula (updateDeriv (left $ child formula) 1) ++ recNegateFormula (Not{op="~", child=right $ child formula, deriv=deriv formula, isop=True})
    | opr == "~" && op (child formula) == "^" = [updateDeriv (left $ child formula) (-inc-1)] ++ [updateDeriv (right $ child formula) (inc+1)] ++ recNegateFormula(updateDeriv (left $ child formula) (-inc-1)) ++ recNegateFormula(updateDeriv (right $ child formula) (inc+1)) -- AQUI
    | opr == "~" && op (child formula) == "v" = [Not{op="~", child=left $ child formula, deriv=deriv formula, isop=True}] ++ [Not{op="~", child=right $ child formula, deriv=deriv formula, isop=True}] ++ recNegateFormula (Not{op="~", child=left $ child formula, deriv=deriv formula, isop=True}) ++ recNegateFormula (Not{op="~", child=right $ child formula, deriv=deriv formula, isop=True})
    | opr == "~" && op (child formula) == "~" = [updateDeriv (child $ child formula) 1] ++ recNegateFormula (updateDeriv (child $ child formula) 1)
    | opr == "->" = [Not{op="~", child=updateDeriv (left formula) 1, deriv=(-inc-1) * deriv formula, isop=True}] ++ [updateDeriv (right formula) (inc+1)] ++ recNegateFormula (Not{op="~", child=updateDeriv (left formula) 1, deriv=(-inc-1) * deriv formula, isop=True}) ++ recNegateFormula (updateDeriv (right formula) (inc+1)) -- AQUI
    | opr == "^" = [updateDeriv (left formula) 1] ++ [updateDeriv (right formula) 1] ++ recNegateFormula (updateDeriv (left formula) 1) ++ recNegateFormula (updateDeriv (right formula) 1)
    | opr == "v" = [updateDeriv (left formula) (-inc-1)] ++ [updateDeriv (right formula) (inc+1)] ++ recNegateFormula (updateDeriv (left formula) (-inc-1)) ++ recNegateFormula (updateDeriv (right formula) (inc+1)) -- AQUI
    | otherwise = []
    where opr = op formula
          inc = if deriv formula == 1 then 2 else 1

updateDeriv :: Formula -> Int -> Formula
updateDeriv formula inc = formula{deriv=inc * deriv formula}

igual :: Formula -> Formula -> Bool
igual x y = if (deriv x) == (deriv y) then True else False

xpto :: [Formula] -> [[Formula]]
xpto x = groupBy (igual) (x)

pUtil :: String -> String -> Map String String-> Map String String
pUtil x y z = Data.Map.insert x y z

aParser :: [Formula] -> Map String String-> String
aParser x y 
    | (Data.List.length x == 0) = "" 
    | ((member (op (head x)) y)  && not ( y Data.Map.! (op (head x)) == (if (op (head  x) == "~") then "FALSE" else "TRUE") )) = " A tem contradicao \n " 
    | otherwise =  aParser (Data.List.drop 1 x)  (Data.Map.insert (op (head x)) ((if (op (head  x) == "~") then "FALSE" else "TRUE"))  y)

multiParser :: [[Formula]] -> [String]
multiParser (x:xs) = aParser x empty : multiParser xs
multiParser list = []

parseCheck :: [String] -> Bool
parseCheck strs = "" `elem` strs

main = do
    formula <- getLine
    let builtFormula = buildFormula formula
    let firstLayerOfHell = xpto $ sort  (negateFormula builtFormula : recNegateFormula (negateFormula builtFormula))
    let secondLayerOfHell = if length firstLayerOfHell > 1 then Data.List.map (head firstLayerOfHell ++) (tail firstLayerOfHell) else firstLayerOfHell
    let thirdLayerOfHell = multiParser secondLayerOfHell
    putStrLn(printFormulas (head secondLayerOfHell))
    print(aParser (head secondLayerOfHell) empty)