module Main where
import Grammar
import Tokens

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
    --return $ parseCalc tokens

--tree of functions to list of functions
getFunctions :: Func -> [(String, Func)]
getFunctions (Function name args body next) = [(name, (FuncEnd name args body))] ++ getFunctions next
getFunctions (FuncEnd name args body) = [(name, (FuncEnd name args body))]

--executes function
execute :: String -> [(String, Data)] -> [(String, Func)] -> [(String, Data)]
execute funcName vars fs
    | name == funcName = process relevant body
    | otherwise = execute funcName vars $ tail fs
        where   (FuncEnd name args body) = snd $ head fs
                relevant = getRelevant args vars
                getRelevant :: Args -> [(String, Data)] -> [(String, Data)]
                getRelevant (Arg argName next) ys = [y | y <- ys, argName == fst y] ++ getRelevant next ys
                getRelevant (ArgEnd argName) ys = [y | y <- ys, argName == fst y]

data Data = DataFunc Func | DataInt Int | DataString String | DataNull deriving Show

--method for updating variables
update :: [(String, Data)] -> (String, Data) -> [(String, Data)]
update [] newVar = [newVar]
update (x:xs) newVar
    | fst x == fst newVar = newVar : xs
    | otherwise = x : update xs newVar

--method for getting variable value
getData :: String -> [(String, Data)] -> Data
getData name [] = DataNull
getData name (x:xs)
    | fst x == name = snd x
    | otherwise = getData name xs

--actually processing language
process :: [(String, Data)] -> Exp -> [(String, Data)]
process vars (VarInt var int next) = process (update vars (var, DataInt int)) next
process vars (VarVar var1 var2 next) = process (update vars (var1, getData var2 vars)) next
process vars (VarFunc var func next) = process (update vars (var, DataFunc func)) next
--process vars (IfElse) = 
--process vars (If) = 
--process vars (FuncCall) = 
process vars (Return) = vars
--process vars (ReturnVar) = 
--process vars (ReturnInt) = 