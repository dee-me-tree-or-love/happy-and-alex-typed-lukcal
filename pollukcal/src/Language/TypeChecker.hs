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


-- Type inference
-- ~~~~~~~~~~~~~~

-- TODO: make use of Functors

annotate :: LAST.TypedExpression -> LAST.TypedExpression
annotate (LAST.SUntypedExpression x) = LAST.STypedExpression t x
    where t = infer x
annotate x = x

infer :: LAST.Expression -> LAST.TypeHint
infer (LAST.STerm (LAST.SText _))   = textTypeHint
infer (LAST.STerm (LAST.SNumber _)) = numberTypeHint
infer (LAST.SExpressionContainer e) = infer e
infer (LAST.SUnExpression _ e)      = infer e
infer (LAST.SBinExpression o e1 e2) = solve o (infer e1) (infer e2)

-- TODO: wrap it in Maybe's
solve :: LAST.Operator -> LAST.TypeHint -> LAST.TypeHint -> LAST.TypeHint
solve _ t1 t2
    | t1 == t2 = t1
    | otherwise  = failTypeHint

-- Type checking
-- ~~~~~~~~~~~~~

check :: LAST.TypedExpression -> Either String String
check (LAST.STypedExpression t e)
    | t == it && (t /= failTypeHint) && (it /= failTypeHint) = Right "Okay"
    | otherwise = Left ("Inferred type: " ++ show it ++ ", doesn't match specified: " ++ show t)
    where it = infer e
check (LAST.SUntypedExpression e)
    | failTypeHint == it = Left ("Inferred type: " ++ show it)
    | otherwise = Right "Okay"
    where it = infer e
