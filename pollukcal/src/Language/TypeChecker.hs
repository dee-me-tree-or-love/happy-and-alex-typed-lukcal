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


-- Type checking (naive)
-- ~~~~~~~~~~~~~~~~~~~~~
-- TODO: this should be optimized

-- FIXME: this needs to be implemented monadically
check :: LAST.TypedExpression -> TypeJudgement
-- if type hint is provided -> infer and check equality
check (LAST.STypedExpression t e)
    | t == k = Right $ "Inferred type: " ++ show k ++ ", matches the specified type: " ++ show t
    | otherwise = Left $ "Inferred type: " ++ show k ++ ", does not match the specified type: " ++ show t
    where k = infer e
-- if it is a container -> check nested
check (LAST.STypedExpressionContainer e) = check e
-- if no hints are provided -> check nested
check (LAST.SExpression e) = check' e

check' :: LAST.Expression -> TypeJudgement
check' (LAST.STerm t)              = Right $ "Inferred type: " ++ show (derive t)
check' (LAST.SUnExpression _ e)    = check e
check' (LAST.SBinExpression _ e f) = resolve k l
    where k = check e
          l = check f

resolve :: TypeJudgement -> TypeJudgement -> TypeJudgement
resolve (Left x) _          = Left x
resolve _ (Left x)          = Left x
resolve (Right x) (Right y) = Right $ x ++ " and " ++ y
