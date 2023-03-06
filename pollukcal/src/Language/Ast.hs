module Language.Ast
  ( GTypedExpression (..),
    GExpression (..),
    TypeHint (..),
    Operator (..),
    Term (..),
    TypedExpression,
    AnnotatedTypedExpression,
    Expression,
    AnnotatedExpression,
  )
where

type TypedExpression = GTypedExpression (Maybe TypeHint)
type AnnotatedTypedExpression = GTypedExpression (Maybe TypeHint, Maybe TypeHint)

type Expression = GExpression TypedExpression
type AnnotatedExpression = GExpression AnnotatedTypedExpression

data GTypedExpression a
  = STypedExpression a (GExpression (GTypedExpression a))
  | STypedExpressionContainer (GTypedExpression a)
  deriving (Eq, Show)

newtype TypeHint
  = STypeHint String
  deriving (Eq, Show)

data GExpression e
  = SBinExpression Operator e e
  | SUnExpression Operator e
  | STerm Term
  deriving (Eq, Show)

newtype Operator
  = SOperator Char
  deriving (Eq, Show)

data Term
  = SNumber Int
  | SText String
  deriving (Eq, Show)
