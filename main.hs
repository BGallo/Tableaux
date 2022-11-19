data Dado = Dado { value :: String
                 , isop  :: Bool
                 , eval  :: Bool
                 } deriving Show
data 

create :: String -> Node
create value
    | value == "^" || value == "v" || value == "->" = Node {value = value, isop = True, eval = True}
    | otherwise = Node {value = value, isop = False, eval = True}

build :: [Node] -> [Node]
build op
    | is == True  = [val, [build (tail op), build (tail (tail op))]] -- o mundo se isso funcionasse https://media.tenor.com/YHZLVDh63dwAAAAd/futuristic-city-star-trek-discovery.gif
    | otherwise = [val]
    where val = op !! 0
          is  = isop  (op !! 0)

main = do
    let source = words "-> a b"
    let a = map create source
    print(build a)