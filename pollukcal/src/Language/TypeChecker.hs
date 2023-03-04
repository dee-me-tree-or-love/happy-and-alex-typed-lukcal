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
annotate (LAST.STypedExpression t x) = LAST.STypedExpression t x
annotate (LAST.SUntypedExpression x) = LAST.STypedExpression t x
    where t = infer x

-- TODO: replace with monadic unwrap?
untype :: LAST.TypedExpression -> LAST.Expression
untype (LAST.SUntypedExpression x) = x
untype (LAST.STypedExpression _ x) = x

infer :: LAST.Expression -> LAST.TypeHint
infer (LAST.STerm t)                = derive t
infer (LAST.SExpressionContainer e) = infer $ untype e
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
check (LAST.STypedExpression t e) = resolve (Just t) k
        where k = infer e
check (LAST.SUntypedExpression e) = resolve (expand e) k
        where k = infer e

resolve :: Maybe LAST.TypeHint -> LAST.TypeHint -> TypeJudgement
resolve Nothing x
    | x == failTypeHint = Left $ "Type inference failed: " ++ show x
    | otherwise = Right "OK"
resolve (Just x) y
    | x == failTypeHint = Left $ "Specified type failed: " ++ show x
    | x == y = Right "OK"
    | otherwise = Left $ "Inferred type: " ++ show y  ++ ", does not match specified type: " ++ show x

expand :: LAST.Expression -> Maybe LAST.TypeHint
expand (LAST.STerm _)                = Nothing
expand (LAST.SExpressionContainer e) = extract e
expand (LAST.SUnExpression _ e)      = extract e
expand (LAST.SBinExpression o e f) = do
    t <- extract e
    k <- extract f
    return $ solve o t k

extract :: LAST.TypedExpression -> Maybe LAST.TypeHint
extract (LAST.SUntypedExpression _) = Nothing
extract (LAST.STypedExpression t _) = Just t
