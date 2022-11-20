{-# LANGUAGE RecordWildCards #-}
data Tree a = Leaf Render a 
            | DBranch Render (Tree a) (Tree  a) a
            | SBranch Render (Tree a) a 
            deriving Show


data Render = Data {
    depth::Int, 
    layer::Int}
    deriving Show

newRender = Data{
    depth = 0,
    layer = 0
}

data Node = Operation {
                operator::Operator,
                state::Bool,
                left::Node,
                right::Node
            }
            |Var {
                var::String,
                state::Bool
            }
            deriving Show

data Operator = And
                |Or
                |Impl
                |Not
                deriving Show



makeVar::String -> Node
makeVar var = Var{var = var, state = True}

makeOp::Node -> Node -> Operator -> Node
makeOp left right op = Operation{operator = op, state = True, left = left, right = right}

buildFormula::String -> Node
buildFormula input = head(foldl match [] (reverse(words input)))
            where match (x:y:xs) "^" = makeOp x y And:xs
                  match (x:y:xs) "v" = makeOp x y Or:xs
                  match (x:y:xs) "->" = makeOp x y Impl:xs
                  match ((Operation {..}):xs) "~" =  (Operation {state = not state, ..}):xs
                  match ((Var {..}):xs) "~" =  (Var {state = not state, ..}):xs
                  match xs operand = (makeVar operand):xs
                  otherwise = error "Entrada incorreta, pouco operandos"

main = do
    let formula = "-> v p ^ q r ^ v p q v p r"
    let a = buildFormula formula
    print a