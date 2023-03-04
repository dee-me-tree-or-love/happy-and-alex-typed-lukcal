module Language.TypeChecker (annotate, check) where

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

-- TODO: make use of Functors & Monads

annotate :: LAST.TypedExpression -> LAST.TypedExpression
annotate (LAST.STypedExpressionContainer x) = annotate x
annotate (LAST.STypedExpression t x) = LAST.STypedExpression t x
annotate (LAST.SUntypedExpression x) = LAST.STypedExpression t $ LAST.SUntypedExpression x
    where t = infer x

-- TODO: replace with monadic unwrap?
untype :: LAST.TypedExpression -> LAST.Expression
untype (LAST.SUntypedExpression x)        = x
untype (LAST.STypedExpression _ x)        = untype x
untype (LAST.STypedExpressionContainer x) = untype x

infer :: LAST.Expression -> LAST.TypeHint
infer (LAST.STerm t)                = derive t
infer (LAST.SUnExpression _ e)      = infer $ untype e
infer (LAST.SBinExpression o e1 e2) = solve o (infer $ untype e1) (infer $ untype e2)

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
check (LAST.STypedExpression t e)
    | t == failTypeHint = Left $ "Specified failed type: " ++ show t
    | t == k && (k /= failTypeHint) = Right "Okay"
    | otherwise = Left ("Inferred type: " ++ show k ++ ", doesn't match specified: " ++ show t)
    where k = infer $ untype e
check (LAST.SUntypedExpression e)
    | failTypeHint == k = Left ("Inferred type: " ++ show k)
    | otherwise = Right "Okay"
    where k = infer e
check (LAST.STypedExpressionContainer e) = check e
