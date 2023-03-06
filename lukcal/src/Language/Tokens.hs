module Language.Tokens (Token (..)) where

data Token
  = TLeftBrace
  | TRightBrace
  | TOperator Char
  | TText String
  | TNumber Int
  -- Type hint indicator
  | TTypeHintIndicator
  deriving (Eq, Show)
