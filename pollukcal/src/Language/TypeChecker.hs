module Language.TypeChecker (infer, check, TypeJudgement) where

import qualified Language.Ast as LAST

-- Default types
-- ~~~~~~~~~~~~~

textTypeHint :: LAST.TypeHint
textTypeHint = LAST.STypeHint "Text"

numberTypeHint :: LAST.TypeHint
numberTypeHint = LAST.STypeHint "Number"


-- Type inference
-- ~~~~~~~~~~~~~~

infer :: LAST.TypedExpression -> Maybe LAST.TypeHint
infer (LAST.STypedExpression _ x)        = infer' x
infer (LAST.STypedExpressionContainer x) = infer x

infer' :: LAST.Expression -> Maybe LAST.TypeHint
infer' (LAST.STerm t)              = derive t
infer' (LAST.SUnExpression _ e)    = infer e
infer' (LAST.SBinExpression o e f) = solve o (infer e) (infer f)

derive :: LAST.Term -> Maybe LAST.TypeHint
derive (LAST.SText _)   = Just textTypeHint
derive (LAST.SNumber _) = Just numberTypeHint

solve :: LAST.Operator -> Maybe LAST.TypeHint -> Maybe LAST.TypeHint -> Maybe LAST.TypeHint
solve _ t1 t2
    | t1 == t2 = t1
    | otherwise  = Nothing


-- Type checking (naive)
-- ~~~~~~~~~~~~~~~~~~~~~
-- TODO: implement

type TypeJudgement = Either (Maybe LAST.TypeHint, String) (Maybe LAST.TypeHint, String)

check :: LAST.AnnotatedTypedExpression -> TypeJudgement
check = error "Not implemented"
