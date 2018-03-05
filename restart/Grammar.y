{
module Grammar where
import Tokens


}


%name killme
%tokentype { Token }
%error { parseError }
%token
    '='     { TokenEq _ } 
    ','     { TokenComma _ } 
    and     { TokenAnd _ } 
    where   { TokenWhere _ } 
    '('     { TokenLBracket _ } 
    ')'     { TokenRBracket _ } 
    int     { TokenInt _ $$ }
    var     { TokenVar _ $$ }
    
%right where
%left and ','
%nonassoc '(' ')'
%%

Exp : Output where Pred {ExpNorm $1 $3}

Output : int ',' Output {OutputInt $1 $3}
       | int            {OutputEnd $1}

Pred : Pred and Pred {PredAnd $1 $3}
     | var '(' Output ')' {PredSource $1 $3}
     | int '=' int {PredEq $1 $3}

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Exp = ExpNorm Output Pred deriving Show

data Output = OutputInt Int Output
    | OutputEnd Int deriving Show

data Pred = PredAnd Pred Pred
    | PredSource String Output
    | PredEq Int Int deriving Show
}