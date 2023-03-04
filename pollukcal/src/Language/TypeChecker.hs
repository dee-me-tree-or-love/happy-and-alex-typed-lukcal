module Language.TypeChecker (infer, check) where

import qualified Language.Ast as LAST

-- Default types
-- ~~~~~~~~~~~~~

textTypeHint :: LAST.TypeHint
textTypeHint = LAST.STypeHint "Text"

numberTypeHint :: LAST.TypeHint
numberTypeHint = LAST.STypeHint "Number"

-- TODO: Replace with Nothing
failTypeHint :: LAST.TypeHint
failTypeHint = LAST.STypeHint "FAIL"

type TypeJudgement = Either String String


-- Type inference
-- ~~~~~~~~~~~~~~

infer :: LAST.TypedExpression -> LAST.TypeHint
infer (LAST.SExpression x)               = infer' x
infer (LAST.STypedExpression _ x)        = infer x
infer (LAST.STypedExpressionContainer x) = infer x

infer' :: LAST.Expression -> LAST.TypeHint
infer' (LAST.STerm t)              = derive t
infer' (LAST.SUnExpression _ e)    = infer e
infer' (LAST.SBinExpression o e f) = solve o (infer e) (infer f)

derive :: LAST.Term -> LAST.TypeHint
derive (LAST.SText _)   = textTypeHint
derive (LAST.SNumber _) = numberTypeHint

-- TODO: wrap it in Maybe's
solve :: LAST.Operator -> LAST.TypeHint -> LAST.TypeHint -> LAST.TypeHint
solve _ t1 t2
    | t1 == t2 = t1
    | otherwise  = failTypeHint

-- Type checking
-- ~~~~~~~~~~~~~

-- FIXME: this works for only the top-level type hints.
check :: LAST.TypedExpression -> TypeJudgement
check _ = Left "fail"
