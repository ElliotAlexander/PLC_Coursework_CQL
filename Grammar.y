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
    else { TokenElse AlexPosn }
    is  { TokenIs AlexPosn }
    '=' { TokenEq AlexPosn }
    int { TokenInt AlexPosn $$ }
    var { TokenVar AlexPosn $$ }
    '{' { TokenLCurly AlexPosn }
    '}' { TokenRCurly AlexPosn }
    '&' { TokenAnd AlexPosn }
    '(' { TokenLBracket AlexPosn }
    ')' { TokenRBracket AlexPosn }

%nonassoc '='
%nonassoc and
%right if else
%left is
%nonassoc '{' '}' '(' ')'
%%

Exp : let var '=' Exp in Exp { Let $2 $4 $6 }
    | Exp and Exp            { Plus $1 $3 }
    | Exp
    | '(' Exp ')'            { $2 }
    | '{' Exp '}'            { $2 }
    | '-' Exp %prec NEG      { Negate $2 }
    | int                    { Int $1 }
    | var                    { Var $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"


data Exp = Let String Exp Exp
         | And Exp Exp
         | IfElse Exp Exp Else Exp
         | If Exp Exp
         | Is String Int
         | Int Int
         | Var String
         deriving Show
}
