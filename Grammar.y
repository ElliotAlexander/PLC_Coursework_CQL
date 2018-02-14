{
module Grammar where
import Tokens


}

-- Note that this still needs fixing for AlexPosn

%name killme
%tokentype { Token }
%error { parseError }
%token
    and    { TokenAnd _ }
    or     { TokenOr _ }
    xor    { TokenXor _ }
    if     { TokenIf _ }
    else   { TokenElse _ }
    is     { TokenIs _ }
    '='    { TokenEq _ }
    int    { TokenInt _ $$ }
    var    { TokenVar _ $$ }
    '{'    { TokenLCurly _ }
    '}'    { TokenRCurly _ }
    '('    { TokenLBracket _ }
    ')'    { TokenRBracket _ }
    return { TokenReturn _ }
    true   { TokenTrue _}
    false   { TokenFalse _}

%nonassoc '='
%nonassoc and
%right if else
%left is
%nonassoc '{' '}' '(' ')'
%%

Exp : var is int Exp                        { VarInt $1 $3 $4 }
    | var is var Exp                        { VarVar $1 $3 $4 }
    | if '(' Pred ')' '{' Exp '}' else '{' Exp '}' Exp  { IfElse $3 $6 $10 $12}
    | if '(' Pred ')' '{' Exp '}' Exp                      { If $3 $6 $8 }
    | return { Return }

Pred : Pred and Pred { And $1 $3 }
     | Pred or Pred  { Or $1 $3}
     | Pred xor Pred  { Xor $1 $3}
     | var '=' int { EqVarInt $1 $3}
     | var '=' var { EqVarVar $1 $3}
     | int '=' int { EqIntInt $1 $3}
     | int '=' var { EqIntVar $1 $3}
     | true { PredTrue }
     | false { PredFalse }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Pred = And Pred Pred
        | Or Pred Pred
        | Xor Pred Pred
        | EqVarInt String Int
        | EqVarVar String String
        | EqIntInt Int Int
        | EqIntVar Int String
        | PredTrue
        | PredFalse
        deriving Show

data Exp = VarInt String Int Exp
        | VarVar String String Exp
        | IfElse Pred Exp Exp Exp
        | If Pred Exp Exp
        | Return
        deriving Show
}
