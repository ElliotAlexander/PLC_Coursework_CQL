module Main where
    import Grammar
    import Tokens
    import Text.ParserCombinators.Parsec
    import Data.CSV
    -- Apparently we should use .strict if we're not thunking computation on larger data structures
    import Data.Map.Strict
    import Data.List
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
    type Equalities = [(Int, Int)]
    type Output = [Int]

    --dataflow:
    --we go from Exp -> ExpressionData -> ExpressionData -> ExpressionData -> Mappings -> Mappings -> [[String]] -> [String]
    --functions are express, errorCheck, impliedEquals, getMappings, filterMappings, mappingToCSV, lexicographicalOrdering

    data ExpressionData = EData Output DataSources Equalities deriving Show
    data Mappings = Mapping Output (IO (Either ParseError [VarToValueMap])) Equalities


    --this just makes the Expression more paletable
    express :: Exp -> ExpressionData
    express (ExpNorm outputs preds) = EData (numbersToList outputs) (sources preds) (equal preds)

    --here we check that variables needed for equality and output are actually sourced from a file
    --errorCheck :: ExpressionData -> ExpressionData
    errorCheck :: ExpressionData -> ExpressionData
    errorCheck e = e

    --here we are checking for implied equals through use of one variable coming from multiple files
    --also change instance to another variable
    --impliedEquals :: ExpressionData -> ExpressionData

    --here we read the files and produce a list of possible maps of variables to string values
    --this also assumes no variables coming from two files
    getMappings :: ExpressionData -> Mappings
    getMappings (EData outs datasources equalities) = Mapping outs (dataSourcesToMappings datasources) equalities

    --here we remove any mappings for which the equalities do not hold

    -- Outs = [Int]
    -- dataSourceMappings = [Map Int String]
    -- equalities = [(Int, Int)]
    filterMappings :: Mappings -> Mappings
    filterMappings (Mapping outs dataSourceMappings equalities) = Mapping outs (fmap (fmap (filterMappings' equalities)) dataSourceMappings) equalities

    -- Produces a list of valid mappings. idk if we can presume this to be 1?
    filterMappings' :: [(Int, Int)] -> [VarToValueMap] -> [VarToValueMap]
    filterMappings' equalities = Data.List.filter (ifEq' equalities)
    --filterMappings' equalities vals = do out <- [ x | x <- vals, ifEq' x equalities == True]
    --                                   return out

    ifEq' :: [(Int, Int)] -> VarToValueMap -> Bool
    ifEq' [] mapping = True
    ifEq' (x:xs) mapping
      | Data.Map.Strict.lookup (fst x) mapping == Data.Map.Strict.lookup (snd x) mapping = ifEq' xs mapping
      | otherwise = False



    --here we produce a list of all possible outputs
    --mappingToCSV :: Mappings -> [[String]]
    --mappingToCSV (Mapping outs dataSourceMappings equalities) =




    mappingToCSV' :: [Int] -> [VarToValueMap]-> [[String]]
    mappingToCSV' outputs xs = Data.List.map (\ x -> [x ! o | o <- outputs]) xs


    testMaps :: [VarToValueMap]
    testMaps = [fromList [(1,"first"), (3, "third"), (2, "second")]]


    lexicographicalOrdering :: [[String]] -> [String]
    lexicographicalOrdering xss = sort $ Data.List.map (intercalate ",") xss

    --AUXILIARY FUNCTIONS
    --express
    numbersToList :: Numbers -> [Int]
    numbersToList (Number i next) = i : numbersToList next
    numbersToList (NumberEnd i) = [i]

    sources :: Pred -> DataSources
    sources mapIn = Data.Map.Strict.map varToColumnMapping $ sources' mapIn

    sources' :: Pred -> Map String Numbers
    sources' (PredAnd p1 p2) = sources' p1 `Data.Map.Strict.union` sources' p2
    sources' (PredSource file out) = singleton file out
    sources' (PredEq _ _) = empty

    varToColumnMapping :: Numbers -> VarToColumnMap
    varToColumnMapping = varToColumnMapping' 0

    varToColumnMapping' :: Int -> Numbers -> VarToColumnMap
    varToColumnMapping' counter (Number i next) = Data.Map.Strict.insert i counter $ varToColumnMapping' (counter + 1) next
    varToColumnMapping' counter (NumberEnd i) = singleton i counter

    equal :: Pred -> [(Int, Int)]
    equal (PredSource file out) = []
    equal (PredEq a b) = [(a,b)]
    equal (PredAnd p1 p2) = equal p1 ++ equal p2

    --errorCheck

    --impliedEquals

    --getMappings
    dataSourcesToMappings :: DataSources -> IO (Either ParseError [VarToValueMap])
    dataSourcesToMappings datasources = do readColumns <- traverseWithKey dataSourceToMapping datasources
                                           let columns = elems readColumns
                                           let combined = combineEitherMaps columns
                                           let result = fmap generateMappings combined
                                           return result

    dataSourceToMapping :: FilePath -> VarToColumnMap -> IO (Either ParseError VarToAllValuesMap)
    dataSourceToMapping file vartocolumn = do contents <- parseFromFile csvFile $ file ++ ".csv"
                                              let result = fmap (getColumns vartocolumn) contents
                                              return result

    getColumns :: VarToColumnMap -> CSV -> VarToAllValuesMap
    getColumns vartocolumn csv = Data.Map.Strict.map (\col -> [line !! col | line <-csv]) vartocolumn

    combineEitherMaps :: [Either a1 (Map Int a)] -> Either a1 (Map Int a)
    combineEitherMaps = Prelude.foldr (liftM2 Data.Map.Strict.union) (Right empty)

    combineMaps :: Ord k => [Map k a] -> Map k a
    combineMaps = Prelude.foldr Data.Map.Strict.union empty

    --just a test
    vartoall :: VarToAllValuesMap
    vartoall = Data.Map.Strict.insert 1 ["test", "me"] (singleton 2 ["work", "now"])

    --generates possible mappings
    generateMappings :: Ord k => Map k [v] -> [Map k v]
    generateMappings x = Prelude.map fromList $ Prelude.foldr (combinations . (\ pair -> [(fst pair, val) | val <- snd pair])) [] $ assocs x

    --recursive combiner
    combinations :: [a] -> [[a]] -> [[a]]
    combinations xs [] = [[x] | x <- xs]
    combinations [] _ = []
    combinations (x:xs) ys = [x : y | y <- ys] ++ combinations xs ys

    --just a parsing test
    test :: ExpressionData
    test = express $ killme $ alexScanTokens "1,3,2,4 where a(1,2) and b(3,4) and 1 = 2"
