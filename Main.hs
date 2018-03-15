module Main where
    import Grammar
    import Tokens
    import Text.ParserCombinators.Parsec
    import Data.CSV
    import System.Environment
    import Data.Map.Strict
    import Data.List
    import Control.Monad
    import qualified Data.Foldable as Foldable


    -- Note that if youre trying to run this in GHCI, you'll need to enable
    -- :set -XRankNTypes
    -- inside each session

    main :: IO()
    main = do args <- getArgs
              let progfile = head args
              program <- readFile progfile
              do result <- evaluate program
                 case result of
                   Left pe -> putStrLn ("Parse Error " ++ show pe)
                   Right contents -> putStrLn contents

    -- ==================================
    --      Begin test cases
    -- ==================================

    finalTest :: String
    finalTest = "1,3,2,4 where a(1,2) and b(3,4)"

    onpTest :: String
    onpTest = "1,3,2,5 where a(1,2) and b(3,4)"

    fecTest :: String
    fecTest = "1,3,2,4 where a(1,2) and b(3,4) and 3=5"



    testMaps :: [VarToValueMap]
    testMaps = [fromList [(1,"first"), (3, "third"), (2, "second")]]
    --      End test cases
    -- ==================================

    evaluate program = fmap (fmap lexicographicalOrdering) $ mappingToCSV $ filterMappings $ getMappings $ errorCheck $ express $ parseCql $ alexScanTokens program

    --types we defined to make following stuff a bit easier
    type CSV = [[String]]
    type VarToColumnMap = Map Int Int
    type VarToAllValuesMap = Map Int [String]
    type VarToValueMap = Map Int String
    type DataSourceIntermediate = Map FilePath [Int]
    type DataSources = Map FilePath VarToColumnMap
    type Equalities = [(Int, Int)]
    type Output = [Int]

    --dataflow:
    --we go from Exp -> ExpressionData -> ExpressionData -> ExpressionData -> Mappings -> Mappings -> [[String]] -> String
    --functions are express, errorCheck, impliedEquals, getMappings, filterMappings, mappingToCSV, lexicographicalOrdering

    data ExpressionData = EData Output DataSources Equalities deriving Show
    data Mappings = Mapping Output (IO (Either ParseError [VarToValueMap])) Equalities


    --this just makes the Expression more paletable
    express :: Exp -> ExpressionData
    express (ExpNorm outputs preds) = EData (numbersToList outputs) (Data.Map.Strict.map listToMap (fst (impliedEquals preds))) (snd (impliedEquals preds))

    --here we check that variables needed for equality and output are actually sourced from a file
    --errorCheck :: ExpressionData -> ExpressionData
    errorCheck :: ExpressionData -> ExpressionData
    errorCheck e
      | outputNotPresentCheck e && freeEqualitiesCheck e = e
      | not $ outputNotPresentCheck e = error "You have a variable in your output with no source."
      | otherwise = error "You have an equality which refers to a variable with no source."

    outputNotPresentCheck :: ExpressionData -> Bool
    outputNotPresentCheck (EData out ds equalities)
      | length undeclared_list == 0 = True
      | otherwise = error ("Undeclared Variable inside output descriptors. \nList of undeclared variables =: " ++ show undeclared_list)
      where
        declared_vars = concat (fmap (Data.Map.Strict.keys) (Data.Map.Strict.elems ds))
        undeclared_list = [ x | x <- out, x `notElem` declared_vars]

    freeEqualitiesCheck :: ExpressionData -> Bool
    freeEqualitiesCheck (EData out ds equalities)
        | length undeclared_list == 0 = True
        | otherwise = error ("Undeclared Variable inside Equality. \nList of undeclared variables=: " ++ show undeclared_list)
        where
          equalites_vars = [ fst x | x <- equalities] `Data.List.union` [ snd y | y <- equalities]
          declared_vars = concat (fmap (Data.Map.Strict.keys) (Data.Map.Strict.elems ds))
          undeclared_list = [ x | x <- equalites_vars, not $ elem x declared_vars]

    --here we read the files and produce a list of possible maps of variables to string values
    --this also assumes no variables coming from two files
    getMappings :: ExpressionData -> Mappings
    getMappings (EData outs datasources equalities) = Mapping outs (dataSourceMappings datasources) equalities

    --here we remove any mappings for which the equalities do not hold
    filterMappings :: Mappings -> Mappings
    filterMappings (Mapping outs dataSourceMappings equalities) = Mapping outs (fmap (fmap (filterMappings' equalities)) dataSourceMappings) equalities

    -- filters mappings based on the equalities
    filterMappings' :: [(Int, Int)] -> [VarToValueMap] -> [VarToValueMap]
    filterMappings' equalities = Data.List.filter (ifEq' equalities)

    ifEq' :: [(Int, Int)] -> VarToValueMap -> Bool
    ifEq' [] mapping = True
    ifEq' (x:xs) mapping
      | Data.Map.Strict.lookup (fst x) mapping == Data.Map.Strict.lookup (snd x) mapping = ifEq' xs mapping
      | otherwise = False

    --here we produce a list of all possible outputs
    mappingToCSV :: Mappings -> IO (Either ParseError [[String]])
    mappingToCSV (Mapping outs dataSourceMappings _) = fmap (fmap (mappingToCSV' outs)) dataSourceMappings

    mappingToCSV' :: [Int] -> [VarToValueMap]-> [[String]]
    mappingToCSV' outputs xs = Data.List.map (\ x -> [x ! o | o <- outputs]) xs


    lexicographicalOrdering :: [[String]] -> String
    lexicographicalOrdering xss = intercalate "\n" $ sort $ Data.List.map (intercalate ",") xss

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
    rename _ [] = ([], [])
    rename taken (x:xs)
      | elem x taken = (new : fst (rename taken xs), neweq : snd (rename taken xs))
            | otherwise = (x : fst (rename taken xs), snd (rename taken xs))
        where new = head [y | y <- [0..], y `notElem` taken]
              neweq = (x,new)

    bound :: Pred -> [Int]
    bound (PredAnd p1 p2) = bound p1 ++ bound p2
    bound (PredSource _ vars) = numbersToList vars
    bound (PredEq _ _) = []

    listToMap :: [Int] -> Map Int Int
    listToMap xs = listToMap' xs 0

    listToMap' :: [Int] -> Int -> Map Int Int
    listToMap' [] _ = Data.Map.Strict.empty
    listToMap' (x:xs) counter = Data.Map.Strict.singleton x counter `Data.Map.Strict.union` listToMap' xs (counter + 1)

    equal :: Pred -> [(Int, Int)]
    equal (PredSource file out) = []
    equal (PredEq a b) = [(a,b)]
    equal (PredAnd p1 p2) = equal p1 ++ equal p2

    dataSourceMappings :: DataSources -> IO (Either ParseError [VarToValueMap])
    dataSourceMappings datasources = do sourced <- traverseWithKey readDataSource datasources
                                        let filed = elems sourced
                                        let result = cartProductMonadic filed
                                        return result

    readDataSource :: FilePath -> VarToColumnMap -> IO (Either ParseError [VarToValueMap])
    readDataSource filePath vartocolumn = do addLineBreak $ filePath ++ ".csv"
                                             csv <- parseFromFile csvFile $ filePath ++ ".csv"
                                             let assigned = fmap (assignVariables vartocolumn) csv
                                             return assigned

    addLineBreak :: FilePath -> IO ()
    addLineBreak filePath = do contents <- readFile filePath
                               let finalChar = last contents
                               case finalChar of
                                 '\n' -> print ()
                                 _ -> appendFile filePath "\n"

    cartProductMonadic :: Foldable t => t (Either a [VarToValueMap]) -> Either a [VarToValueMap]
    cartProductMonadic xs = Data.List.foldr (liftM2 cartProduct') (Right [Data.Map.Strict.empty]) xs

    cartProduct :: [[VarToValueMap]] -> [VarToValueMap]
    cartProduct xs = Data.List.foldr cartProduct' [Data.Map.Strict.empty] xs

    cartProduct' :: [VarToValueMap] -> [VarToValueMap] -> [VarToValueMap]
    cartProduct' xs ys = [Data.Map.Strict.union x y | x <- xs, y <-ys]

    assignVariables :: VarToColumnMap -> CSV -> [VarToValueMap]
    assignVariables _ [] = []
    assignVariables vartocolumn (line:rest)
      | any (length line <=) $ elems vartocolumn = assignVariables vartocolumn rest
      | otherwise = Data.Map.Strict.map (line !!) vartocolumn : assignVariables vartocolumn rest
