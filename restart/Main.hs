module Main where
    import Grammar
    import Tokens
    import Text.ParserCombinators.Parsec
    import Data.CSV
    import Data.Map
    import Control.Monad
    
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

    type CSV = [[String]]
    type VarToColumnMap = Map Int Int
    type VarToAllValuesMap = Map Int [String]
    type VarToValueMap = Map Int String
    type DataSources = Map FilePath VarToColumnMap
    type Equalities = Map Int Int
    type Output = [Int]

    --dataflow:
    --we go from Exp -> ExpressionData -> ExpressionData -> ExpressionData -> Mappings -> Mappings -> [[String]] -> [[String]]
    --functions are express, errorCheck, impliedEquals, getMappings, filterMappings, mappingToCSV, lexicographicalOrdering

    data ExpressionData = EData Output DataSources Equalities deriving Show
    data Mappings = Mapping Output (IO (Either ParseError [VarToValueMap])) Equalities 

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
    getMappings (EData outs datasources equalities) = Mapping outs (dataSourcesToMappings datasources) equalities

    --here we remove any mappings for which the equalities do not hold
    --filterMappings :: Mappings -> Mappings

    --here we produce a list of all possible outputs
    --mappingToCSV :: Mapping -> [[String]]

    --here we order the outputs lexicographically ;)
    --lexicographicalOrdering :: [[String]]

    --AUXILIARY FUNCTIONS
    --express
    numbersToList :: Numbers -> [Int]
    numbersToList (Number i next) = i : numbersToList next
    numbersToList (NumberEnd i) = [i]

    sources :: Pred -> DataSources
    sources mapIn = Data.Map.map varToColumnMapping $ sources' mapIn

    sources' :: Pred -> Map String Numbers
    sources' (PredAnd p1 p2) = union (sources' p1)  (sources' p2)
    sources' (PredSource file out) = singleton file out
    sources' (PredEq _ _) = empty

    equal :: Pred -> Map Int Int
    equal (PredAnd p1 p2) = union (equal p1) (equal p2)
    equal (PredSource file out) = empty
    equal (PredEq a b) = singleton a b

    --errorCheck

    --impliedEquals

    --getMappings

    getSources (EData outs datasources equalities) = datasources

    dataSourcesToMappings datasources = do readColumns <- traverseWithKey dataSourceToMapping datasources
                                           let columns = elems readColumns
                                           let combined = combineEitherMaps columns
                                           let result = fmap inter combined
                                           return result

    dataSourceToMapping file vartocolumn = do contents <- parseFromFile csvFile $ file ++ ".csv"
                                              let result = fmap (getColumns vartocolumn) contents
                                              return result
    
    getColumns :: VarToColumnMap -> CSV -> VarToAllValuesMap
    getColumns vartocolumn csv = Data.Map.map (\col -> [line !! col | line <-csv]) vartocolumn

    combineEitherMaps :: Ord k => [Either ParseError (Map k a)] -> Either ParseError (Map k a)
    combineEitherMaps [] = (Right empty)
    combineEitherMaps (x:xs) = liftM2 union x (combineEitherMaps xs)

    combineMaps :: Ord k => [Map k a] -> Map k a
    combineMaps [] = empty
    combineMaps (x:xs) = union x $ combineMaps xs

    vartoall :: Map Int [String]
    vartoall = insert 1 ["test", "me"] (singleton 2 ["work", "now"])

    inter :: VarToAllValuesMap -> [VarToValueMap]
    inter x = inter4' $ inter3' $ inter2' $ inter' x 

    inter' :: VarToAllValuesMap -> [(Int, [String])]
    inter' vartoall = assocs vartoall

    inter2' :: [(Int, [String])] -> [[(Int, String)]]
    inter2' [] = []
    inter2' (x:xs) = [(fst x, y) | y <- snd x] : inter2' xs

    inter3' :: [[(Int, String)]] -> [[(Int, String)]]
    inter3' [] = []
    inter3' (x:xs) = combinations x $ inter3' xs

    inter4' :: [[(Int, String)]] -> [VarToValueMap]
    inter4' [] = []
    inter4' (x:xs) = fromList x : inter4' xs

    combinations :: [a] -> [[a]] -> [[a]]
    combinations xs [] = [[x] | x <- xs]
    combinations [] _ = []
    combinations (x:xs) ys = [x : y | y <- ys] ++ combinations xs ys

    --listLength :: Numbers -> Int
    --listLength (Number _ next) = 1 + listLength next
    --listLength (NumberEnd _) = 1

    --listOfColumns :: Numbers -> [Int]
    --listOfColumns x = [0..(listLength x - 1)]

    varToColumnMapping :: Numbers -> Map Int Int
    varToColumnMapping x = varToColumnMapping' x 0

    varToColumnMapping' :: Numbers -> Int -> Map Int Int
    varToColumnMapping' (Number i next) counter = insert i counter $ varToColumnMapping' next (counter + 1)
    varToColumnMapping' (NumberEnd i) counter = singleton i counter

    --genMapping :: Map Int Int -> [[String]] -> [Map Int String]
    --genMapping _ [] = []
    --genMapping columnToVar (x:xs) = Data.Map.map (\index -> x !! index) columnToVar : (genMapping columnToVar xs)

    --format :: Numbers -> Map Int String -> [String]
    --format (Number i next) mapping = mapping ! i : format next mapping
    --format (NumberEnd i) mapping = [mapping ! i]

    --formatGroup :: Numbers -> [Map Int String] -> [[String]]
    --formatGroup output mappings = Prelude.map (format output) mappings

    --getIndex :: [[String]] -> Int -> [String]
    --getIndex [] index = []
    --getIndex (line:rest) index = line !! index : getIndex rest index
    
    -- mappingsFromCSV :: String -> Numbers -> IO (Either ParseError [[String]])
    --mappingsFromCSV file indexes output = do contents <- parseFromFile csvFile (file ++ ".csv")
    --                                         let inter = fmap (genMapping indexes) contents
    --                                         let result = fmap (formatGroup output) inter
    --                                         return result

    test = express $ killme $ alexScanTokens "1,3,2,4 where a(1,2) and b(3,4)"

    --mapping :: [[String]] -> [(Int, Int)] -> [[(Int, String)]]
    --mapping (x:xs)

    --allMappings input = mappingsFromCSV (fst input) (listOfColumns $ snd input)