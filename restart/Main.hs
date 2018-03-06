module Main where
    import Grammar
    import Tokens
    import Text.ParserCombinators.Parsec
    import Data.CSV
    import Data.Map
    
    --main method for no real reason
    main :: IO()
    main = do  
        str <- readFile "input.meme"
        --tokens <- alexScanTokens str
        putStrLn str
        --return
        --return $ parseCalc tokens
    
    --although we use FilePath the '.csv' is implied and not kept within the string
    --not redefining this but just for reference
    --type FilePath = String

    type VarToColumnMap = Map Int Int
    type VarToValueMap = Map Int String
    type DataSources = Map FilePath VarToColumnMap
    type Equalities = Map Int Int
    type Output = [Int]

    --dataflow:
    --we go from Exp -> ExpressionData -> ExpressionData -> ExpressionData -> Mappings -> Mappings -> [[String]] -> [[String]]
    --functions are express, errorCheck, impliedEquals, getMappings, filterMappings, mappingToCSV, lexicographicalOrdering

    data ExpressionData = EData Output DataSources Equalities deriving Show
    data Mappings = Mapping Output [VarToValueMap] Equalities deriving Show

    --this just makes the Expression more paletable
    express :: Exp -> ExpressionData
    express (ExpNorm outputs preds) = EData (numbersToList outputs) (sources preds) (equal preds)
    
    --here we check that variables needed for equality and output are actually sourced from a file
    --errorCheck :: ExpressionData -> ExpressionData

    --here we are checking for implied equals through use of one variable coming from multiple files
    --also change instance to another variable
    --impliedEquals :: ExpressionData -> ExpressionData

    --here we read the files and produce a list of possible maps of variables to string values
    --this also assumes no variables coming from two files
    getMappings :: ExpressionData -> Mappings
    getAllMappings (EData outs datasources equalities) = Mapping outs (datasourcesToMappings datasources) equalities

    --here we remove any mappings for which the equalities do not hold
    --filterMappings :: Mappings -> Mappings

    --here we produce a list of all possible outputs
    --mappingToCSV :: Mapping -> [[String]]

    --here we order the outputs lexicographically ;)
    --lexicographicalOrdering :: [[String]]

    numbersToList :: Numbers -> [Int]
    numbersToList (Number i next) = i : numbersToList next
    numbersToList (NumberEnd i) = [i]

    sources :: Pred -> DataSources
    sources mapIn = Data.Map.map varToColumnMapping $ sources' mapIn

    sources' :: Pred -> Map String Numbers
    sources' (PredAnd p1 p2) = union (sources' p1)  (sources' p2)
    sources' (PredSource file out) = singleton file out
    sources' (PredEq _ _) = empty

    --sourcesToMapping

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

    test = express $ killme $ alexScanTokens "1,3,2,4 where a(1,2) and b(3,4)"

    --mapping :: [[String]] -> [(Int, Int)] -> [[(Int, String)]]
    --mapping (x:xs)

    --allMappings input = mappingsFromCSV (fst input) (listOfColumns $ snd input)