{
module Language.Lexer (scan) where
import Language.Tokens as LT (Token (..))
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-

  $white+                        ;
  "--".*                         ;
  "~~"                           { \_ -> LT.TTypeHintIndicator }
  "("                            { \_ -> LT.TLeftBrace }
  ")"                            { \_ -> LT.TRightBrace }
  [\=\+\-\*\/\(\)]               { \s -> LT.TOperator (head s) }
  $digit+                        { \s -> LT.TNumber (read s) }
  $alpha [$alpha $digit \_ \']*  { \s -> LT.TText s }


{
-- Each action has type :: String -> Token

-- | Tokenizes a string.
scan :: String -> [LT.Token]
scan = alexScanTokens
}