{
    module Tokens where
}

%wrapper "posn"

$digit = 1-9
$alpha = [a-z]

tokens :-
    $white+     ;
    "//".*      ;
    "#".*       ;
    &           { tok (\p s -> TokenAnd p) }
    =           { tok (\p s -> TokenEq p) }
    if          { tok (\p s -> TokenIf p) }
    \(          { tok (\p s -> TokenLBracket p) }
    \)          { tok (\p s -> TokenRBracket p) }
    \{          { tok (\p s -> TokenLCurly p) }
    \}          { tok (\p s -> TokenRCurly p) }
    $digit+     { tok (\p s -> TokenInt p (read s)) }
    $alpha+     { tok (\p s -> TokenVar p s) }
{

tok f p s = f p s

data Token = 
    TokenAnd AlexPosn |
    TokenEq AlexPosn |
    TokenIf AlexPosn |
    TokenLBracket AlexPosn |
    TokenRBracket AlexPosn |
    TokenLCurly AlexPosn |
    TokenRCurly AlexPosn |
    TokenInt AlexPosn Int |
    TokenVar AlexPosn String
    deriving Show
}