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
    \|          { tok (\p s -> TokenOr p)}
    \|\*          { tok (\p s -> TokenXor p)}
    and          { tok (\p s -> TokenAnd p) }
    return         { tok (\p s -> TokenReturn p) }
    =           { tok (\p s -> TokenEq p) }
    if          { tok (\p s -> TokenIf p) }
    else        { tok (\p s -> TokenElse p) }
    is          { tok (\p s -> TokenIs p) }
    true        { tok (\p s -> TokenTrue p)}
    false       { tok (\p s -> TokenFalse p)}
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
    TokenElse AlexPosn |
    TokenIs AlexPosn |
    TokenLBracket AlexPosn |
    TokenRBracket AlexPosn |
    TokenLCurly AlexPosn |
    TokenRCurly AlexPosn |
    TokenInt AlexPosn Int |
    TokenVar AlexPosn String |
    TokenOr AlexPosn |
    TokenXor AlexPosn |
    TokenReturn AlexPosn |
    TokenTrue AlexPosn |
    TokenFalse AlexPosn
    deriving Show
}
