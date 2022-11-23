import Data.List (intercalate)
data Formula = Var{op::String, deriv::Int, isop::Bool} | Not{op::String, child::Formula, deriv::Int, isop::Bool} | And{op::String, left::Formula, right::Formula, deriv::Int, isop::Bool} | Or{op::String, left::Formula, right::Formula, deriv::Int, isop::Bool} | Imp{op::String, left::Formula, right::Formula, deriv::Int, isop::Bool}

buildFormula :: String -> Formula
buildFormula formula = head $ foldl pmatcher [] $ reverse $ words formula
    where pmatcher(x:y:ys) "^" = (And{op = "^", left = x, right = y, deriv = 0, isop = True}):ys
          pmatcher(x:y:ys) "v" = (Or{op = "v", left = x, right = y, deriv = 0, isop = True}):ys
          pmatcher(x:y:ys) "->" = (Imp{op = "->", left = x, right = y, deriv = 0, isop = True}):ys
          pmatcher(x:xs) "~" = (Not{op = "~", child = x, deriv = 0, isop = True}):xs
          pmatcher xs operand = (Var{op = operand, deriv = 0, isop = False}):xs

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
printFormulas formula = intercalate "\n" (map printFormula formula)

negateFormula :: Formula -> Formula
negateFormula formula = Not{op="~", child=formula, deriv=0, isop=True}

recNegateFormula :: Formula -> [Formula]
recNegateFormula formula
    | opr == "~" && op (child formula) == "->" = left (child formula) : Not{op="~", child=right $ child formula, deriv=0, isop=True} : recNegateFormula Not{op="~", child=right $ child formula, deriv=0, isop=True}
    | opr == "~" && op (child formula) == "^" = [formula]
    | opr == "~" && op (child formula) == "v" = Not{op="~", child=left $ child formula, deriv=0, isop=True} : Not{op="~", child=right $ child formula, deriv=0, isop=True} : recNegateFormula Not{op="~", child=left $ child formula, deriv=0, isop=True} ++ recNegateFormula Not{op="~", child=right $ child formula, deriv=0, isop=True}
    | opr == "~" && op (child formula) == "~" = child (child formula) : recNegateFormula (child $ child formula)
    | opr == "->" = [formula]
    | opr == "^" = [left formula] ++ [right formula] ++ recNegateFormula (left formula) ++ recNegateFormula (right formula)
    | opr == "v" = [formula]
    | otherwise = [] -- why??
    where opr = op formula

proveFormula :: String -> IO()
proveFormula formula = do
    putStrLn $ "FÃ³rmula entrada: " ++ printFormula builtFormula
    putStrLn $ printFormulas (negateFormula builtFormula : (recNegateFormula $ negateFormula builtFormula))
    where builtFormula = buildFormula formula

main = do
    formula <- getLine
    proveFormula formula