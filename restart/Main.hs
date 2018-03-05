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
        --return
        --return $ parseCalc tokens

--eval :: ExpNorm -> ()
    ds (ExpNorm outputs preds) = (datasources preds, equal preds)
    
    datasources :: Pred -> [(String, Output)]
    datasources (PredAnd p1 p2) = datasources p1 ++ datasources p2
    datasources (PredSource file out) = [(file, out)]
    datasources (PredEq _ _) = []

    equal :: Pred -> [(Int, Int)]
    equal (PredAnd p1 p2) = equal p1 ++ equal p2
    equal (PredSource file out) = []
    equal (PredEq a b) = [(a,b)]