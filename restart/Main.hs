module Main where
    import Grammar
    import Tokens
    import Text.ParserCombinators.Parsec
    import Data.CSV
    import Data.Map
    
    --evaluates predicates
    --evalPred :: [(String, Exp)] -> Pred -> Bool
    --evalPred vars (And p1 p2) = evalPred p1 && evalPred p2
    --evalPred vars (Or p1 p2) = evalPred p1 || evalPred p2
    --evalPred vars (Xor p1 p2) = (evalPred p1 || evalPred p2) && (!(evalPred p1 && evalPred p2))
    --evalPred vars (EqVarInt v i) = (getVal vars v) == i
    --evalPred vars (EqVarVar v1 v2) = (getVal vars v1) == (getVal vars v1)
    --evalPred vars (EqIntInt i1 i2) = i1 == i2
    --evalPred vars (PredTrue) = True
    --evalPred vars (PredFalse) = False
    
    --main method for no real reason
    main :: IO()
    main = do  
        str <- readFile "input.meme"
        --tokens <- alexScanTokens str
        putStrLn str
        --return
        --return $ parseCalc tokens

--eval :: ExpNorm -> ()
    express (ExpNorm outputs preds) = (datasources preds, equal preds)
    
    datasources :: Pred -> Map String Numbers
    datasources (PredAnd p1 p2) = union (datasources p1)  (datasources p2)
    datasources (PredSource file out) = singleton file out
    datasources (PredEq _ _) = empty

    equal :: Pred -> Map Int Int
    equal (PredAnd p1 p2) = union (equal p1) (equal p2)
    equal (PredSource file out) = empty
    equal (PredEq a b) = singleton a b

    --getPotentialMappings :: String -> [Int] -> [[String]]
    --getPotentialMappings fileName (x:xs) = do file <- parseFromFile csvFile $ fileName ++ ".csv"
    --                                          possible <- fmap getIndex file x
    --                                          return possible

    getAllMappings :: [Int] -> [[String]] -> [[String]]
    getAllMappings [] _ = []
    getAllMappings (x:xs) input = myzip (getIndex input x) (getAllMappings xs input)

    myzip :: [a] -> [[a]] -> [[a]]
    myzip xs [] = [[x] | x <- xs]
    myzip [] _ = []
    myzip (x:xs) ys = [x : y | y <- ys] ++ myzip xs ys

    listLength :: Numbers -> Int
    listLength (Number _ next) = 1 + listLength next
    listLength (NumberEnd _) = 1

    listOfColumns :: Numbers -> [Int]
    listOfColumns x = [0..(listLength x - 1)]

    varToColumnMapping :: Numbers -> Map Int Int
    varToColumnMapping x = varToColumnMapping' x 0

    varToColumnMapping' :: Numbers -> Int -> Map Int Int
    varToColumnMapping' (Number i next) counter = insert i counter $ varToColumnMapping' next (counter + 1)
    varToColumnMapping' (NumberEnd i) counter = singleton i counter

    genMapping :: Map Int Int -> [[String]] -> [Map Int String]
    genMapping _ [] = []
    genMapping columnToVar (x:xs) = Data.Map.map (\index -> x !! index) columnToVar : (genMapping columnToVar xs)

    format :: Numbers -> Map Int String -> [String]
    format (Number i next) mapping = mapping ! i : format next mapping
    format (NumberEnd i) mapping = [mapping ! i]

    formatGroup :: Numbers -> [Map Int String] -> [[String]]
    formatGroup output mappings = Prelude.map (format output) mappings

    getIndex :: [[String]] -> Int -> [String]
    getIndex [] index = []
    getIndex (line:rest) index = line !! index : getIndex rest index
    
    -- mappingsFromCSV :: String -> Numbers -> IO (Either ParseError [[String]])
    mappingsFromCSV file indexes output = do contents <- parseFromFile csvFile (file ++ ".csv")
                                             let inter = fmap (genMapping indexes) contents
                                             let result = fmap (formatGroup output) inter
                                             return result

    test = fst $ express $ killme $ alexScanTokens "1,3,2,4 where a(5,6) and b(3,4)"

    --mapping :: [[String]] -> [(Int, Int)] -> [[(Int, String)]]
    --mapping (x:xs)

    --allMappings input = mappingsFromCSV (fst input) (listOfColumns $ snd input)