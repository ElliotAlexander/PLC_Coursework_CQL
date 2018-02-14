{
module Grammar where
import Tokens
}

-- Note that this still needs fixing for AlexPosn

%name parseCalc
%tokentype { Token }
%error { parseError }
%token
    and { TokenAnd AlexPosn }
    if  { TokenIf AlexPosn }
    is  { TokenIs AlexPosn }
    '=' { TokenEq AlexPosn }
    int { TokenInt AlexPosn $$ }
    var { TokenVar AlexPosn $$ }
    '{' { TokenLCurly AlexPosn }
    '}' { TokenRCurly AlexPosn }
    '&' { TokenAnd AlexPosn }
    '(' { TokenLBracket AlexPosn }
    ')' { TokenRBracket AlexPosn }


%right in
%left '+' '-'
%left '*' '/' '^'
%left NEG
%%
Exp : let var '=' Exp in Exp { Let $2 $4 $6 }
    | Exp '+' Exp            { Plus $1 $3 }
    | Exp '-' Exp            { Minus $1 $3 }
    | Exp '*' Exp            { Times $1 $3 }
    | Exp '/' Exp            { Div $1 $3 }
    | Exp '^' Exp              { Expo $1 $3}
    | '(' Exp ')'            { $2 }
    | '-' Exp %prec NEG      { Negate $2 }
    | int                    { Int $1 }
    | var                    { Var $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
data Exp = Let String Exp Exp
         | Plus Exp Exp
         | Minus Exp Exp
         | Times Exp Exp
         | Div Exp Exp
         | Negate Exp
         | Expo Exp Exp
         | Int Int
         | Var String
         deriving Show
}
