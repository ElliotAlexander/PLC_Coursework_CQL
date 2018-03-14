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
    type DataSourceIntermediate = Map FilePath [Int]
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
    express (ExpNorm outputs preds) = EData (numbersToList outputs) (Data.Map.Strict.map listToMap (fst (impliedEquals preds))) (snd (impliedEquals preds))

    --here we check that variables needed for equality and output are actually sourced from a file
    --errorCheck :: ExpressionData -> ExpressionData
    errorCheck :: ExpressionData -> ExpressionData
    errorCheck e = e


    freeEqualitiesCheck :: ExpressionData -> [Int]
    freeEqualitiesCheck (EData out ds equalities) = do equali_keys <- Data.List.union ([ fst x | x <- equalities]) ([ snd y | y <- equalities])
                                                       varToColumns <- concat (fmap (Data.Map.Strict.keys) (Data.Map.Strict.elems ds))
                                                       return (equali_keys Data.List.\\ varToColumns)
                                                       --return equali_keys
                                                       --return varToColumns



    --here we are checking for implied equals through use of one variable coming from multiple files
    --also change instance to another variable
    --impliedEquals :: ExpressionData -> ExpressionData

    --here we read the files and produce a list of possible maps of variables to string values
    --this also assumes no variables coming from two files
    getMappings :: ExpressionData -> Mappings
    getMappings (EData outs datasources equalities) = Mapping outs (dataSourceMappings datasources) equalities

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

    impliedEquals :: Pred -> (DataSourceIntermediate, Equalities)
    impliedEquals (PredSource file vars) = (Data.Map.Strict.singleton file $ numbersToList vars, [])
    impliedEquals (PredAnd (PredSource file vars) p2)
      | any (`elem` taken) listvars = (Data.Map.Strict.singleton file newvars `Data.Map.Strict.union` fst (impliedEquals p2), neweqs ++ snd (impliedEquals p2))
                  | otherwise = (Data.Map.Strict.singleton file (numbersToList vars) `Data.Map.Strict.union` fst (impliedEquals p2), snd (impliedEquals p2))
          where listvars = numbersToList vars
                taken = bound p2
                renamedStuff = rename taken listvars
                newvars = fst renamedStuff
                neweqs = snd renamedStuff
    impliedEquals (PredAnd p1 p2) = (Data.Map.Strict.union ds1 ds2, eq1 ++ eq2)
      where ds1 = fst $ impliedEquals p1
            ds2 = fst $ impliedEquals p2
            eq1 = snd $ impliedEquals p1
            eq2 = snd $ impliedEquals p2
    impliedEquals (PredEq a b) = (Data.Map.Strict.empty, [(a,b)])

    rename :: [Int] -> [Int] -> ([Int], [(Int,Int)])
    --rename taken listvars
    rename taken (x:xs)
      | elem x taken = ([new] ++ fst (rename taken xs), [neweq] ++ snd (rename taken xs))
            | otherwise = ([x] ++ fst (rename taken xs), snd (rename taken xs))
        where new = head [y | y <- [0..], notElem y taken]
              neweq = (x,new)

    bound :: Pred -> [Int]
    bound (PredAnd p1 p2) = bound p1 ++ bound p2
    bound (PredSource _ vars) = numbersToList vars
    bound (PredEq _ _) = []

    listToMap :: [Int] -> Map Int Int
    listToMap xs = listToMap' xs 0

    listToMap' :: [Int] -> Int -> Map Int Int
    listToMap' [] _ = Data.Map.Strict.empty
    listToMap' (x:xs) counter = Data.Map.Strict.singleton x counter `Data.Map.Strict.union` (listToMap' xs (counter + 1))

    equal :: Pred -> [(Int, Int)]
    equal (PredSource file out) = []
    equal (PredEq a b) = [(a,b)]
    equal (PredAnd p1 p2) = equal p1 ++ equal p2

    --errorCheck

    --impliedEquals

    --getMappings
    --dataSourcesToMappings :: DataSources -> IO (Either ParseError [VarToValueMap])
    --dataSourcesToMappings datasources = do readColumns <- traverseWithKey dataSourceToMapping datasources
    --                                       let columns = elems readColumns
    --                                       let combined = combineEitherMaps columns
    --                                       let result = fmap generateMappings combined
    --                                       return result

    --dataSourceMappings :: DataSources -> IO(Either ParseError [VarToValueMap])
    dataSourceMappings datasources = do sourced <- traverseWithKey readDataSource datasources
                                        let filed = elems sourced
                                        let result = cartProductMonadic filed
                                        return result

    readDataSource :: FilePath -> VarToColumnMap -> IO (Either ParseError [VarToValueMap])
    readDataSource filePath vartocolumn = do csv <- parseFromFile csvFile $ filePath ++ ".csv"
                                             let assigned = fmap (assignVariables vartocolumn) csv
                                             return assigned

    --cartProductMonadic :: [Either a [VarToValueMap]] -> [Either a VarToValueMap]
    --cartProductMonadic [] =
    --cartProductMonadic (x:xs) = liftM2 cartProduct' x (cartProductMonadic xs)

    cartProductMonadic xs = Data.List.foldr (liftM2 cartProduct') (Right [Data.Map.Strict.empty]) xs

    cartProduct :: [[VarToValueMap]] -> [VarToValueMap]
    cartProduct xs = Data.List.foldr cartProduct' [Data.Map.Strict.empty] xs

    test1 :: [VarToValueMap]
    test1 = [Data.Map.Strict.insert 1 "hi" $ Data.Map.Strict.insert 2 "ho" $ singleton 3 "hum",Data.Map.Strict.insert 1 "first" $ Data.Map.Strict.insert 2 "second" $ singleton 3 "third"]

    test2 :: VarToValueMap
    test2 = Data.Map.Strict.insert 4 "fuck" $ Data.Map.Strict.insert 5 "this" $ singleton 6 "shit"

    cartProduct' :: [VarToValueMap] -> [VarToValueMap] -> [VarToValueMap]
    cartProduct' xs ys = [Data.Map.Strict.union x y | x <- xs, y <-ys]

    assignVariables :: VarToColumnMap -> CSV -> [VarToValueMap]
    assignVariables _ [] = []
    assignVariables vartocolumn (line:rest) = Data.Map.Strict.map (line !!) vartocolumn : assignVariables vartocolumn rest

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
    --test :: ExpressionData
    --test = express $ killme $ alexScanTokens "1,3,2,4 where a(1,2) and b(3,4) and 1 = 2"
