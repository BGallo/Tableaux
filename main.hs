import Data.List

xpto expr =if length (words expr) == 3 
    then return [[head (words expr)],[words expr!!2],[words expr!!3]] 
else return [[head (words expr)],[words expr!!2]] 

main:: IO()

source = "-> a b"

main = print  (source)