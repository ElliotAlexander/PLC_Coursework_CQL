{
    module Tokens where
}

%wrapper "posn"

$digit = 1-9
$alpha = [a-zA-Z]

tokens :-
    $white+     ;
    "//".*      ;
    "#".*       ;
    \=          { tok (\p s -> TokenEq p) }
    where       { tok (\p s -> TokenWhere p) }
    and         { tok (\p s -> TokenAnd p) }
    \(          { tok (\p s -> TokenLBracket p) }
    \)          { tok (\p s -> TokenRBracket p) }
    \,          { tok (\p s -> TokenComma p) }
    $digit+     { tok (\p s -> TokenInt p (read s)) }
    $alpha+     { tok (\p s -> TokenVar p s) }
{

tok f p s = f p s

data Token = TokenEq AlexPosn |
            TokenComma AlexPosn |
            TokenAnd AlexPosn |
            TokenWhere AlexPosn |
            TokenLBracket AlexPosn |
            TokenRBracket AlexPosn |
            TokenInt AlexPosn Int |
            TokenVar AlexPosn String
            deriving Show
}
