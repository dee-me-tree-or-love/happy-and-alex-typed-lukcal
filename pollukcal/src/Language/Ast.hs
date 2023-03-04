module Language.Ast
  ( TypedExpression (..),
    TypeHint (..),
    Expression (..),
    Operator (..),
    Term (..),
  )
where

data TypedExpression
  = STypedExpression TypeHint TypedExpression
  | STypedExpressionContainer TypedExpression
  | SExpression Expression
  deriving (Eq, Show)

newtype TypeHint
  = STypeHint String
  deriving (Eq, Show)

data Expression
  = SBinExpression Operator TypedExpression TypedExpression
  | SUnExpression Operator TypedExpression
  | STerm Term
  deriving (Eq, Show)

newtype Operator
  = SOperator Char
  deriving (Eq, Show)

data Term
  = SNumber Int
  | SText String
  deriving (Eq, Show)
