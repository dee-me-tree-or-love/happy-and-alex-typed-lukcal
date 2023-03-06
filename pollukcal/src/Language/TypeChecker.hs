module Language.TypeChecker (infer, annotate, check, TypeJudgement) where

import           Data.Maybe   (isNothing)
import qualified Language.Ast as LAST

-- Default types
-- ~~~~~~~~~~~~~

textTypeHint :: LAST.TypeHint
textTypeHint = LAST.STypeHint "Text"

numberTypeHint :: LAST.TypeHint
numberTypeHint = LAST.STypeHint "Number"


-- Type judgements
-- ~~~~~~~~~~~~~~~

derive :: LAST.Term -> Maybe LAST.TypeHint
derive (LAST.SText _)   = Just textTypeHint
derive (LAST.SNumber _) = Just numberTypeHint

solve :: LAST.Operator -> Maybe LAST.TypeHint -> Maybe LAST.TypeHint -> Maybe LAST.TypeHint
solve _ t1 t2
    | t1 == t2 = t1
    | otherwise  = Nothing


-- Type inference
-- ~~~~~~~~~~~~~~

infer :: LAST.TypedExpression -> Maybe LAST.TypeHint
infer (LAST.STypedExpression _ x)        = infer' x
infer (LAST.STypedExpressionContainer x) = infer x

infer' :: LAST.Expression -> Maybe LAST.TypeHint
infer' (LAST.STerm t)              = derive t
infer' (LAST.SUnExpression _ e)    = infer e
infer' (LAST.SBinExpression o e f) = solve o (infer e) (infer f)



-- Type annotations
-- ~~~~~~~~~~~~~~~~

annotate :: LAST.TypedExpression -> LAST.AnnotatedTypedExpression
annotate (LAST.STypedExpression t x)        = LAST.STypedExpression (t,i) e
    where (i, e) = annotate' x
annotate (LAST.STypedExpressionContainer x) = annotate x

annotate' :: LAST.Expression -> (Maybe LAST.TypeHint, LAST.AnnotatedExpression)
annotate' (LAST.STerm t)              = (derive t, LAST.STerm t)
annotate' (LAST.SUnExpression o e)    = (infer e, LAST.SUnExpression o $ annotate e)
annotate' (LAST.SBinExpression o e f) = (i, s)
    where i = solve o (infer e) (infer f)
          s = LAST.SBinExpression o (annotate e) (annotate f)

-- Type checking (naive)
-- ~~~~~~~~~~~~~~~~~~~~~
-- TODO: implement

type TypeJudgement = Either (Maybe LAST.TypeHint, String) (Maybe LAST.TypeHint, String)

reportInferred :: Maybe LAST.TypeHint -> String
reportInferred i = "Inferred type is: " ++ show i

reportInferredSpecified :: Maybe LAST.TypeHint -> Maybe LAST.TypeHint -> String
reportInferredSpecified i t = reportInferred i ++ ", specified: " ++ show t

check :: LAST.AnnotatedTypedExpression -> TypeJudgement
check (LAST.STypedExpression (t,i) e)
    | isNothing i = Left (Nothing, reportInferredSpecified i t)
    | isNothing t = do
        _ <- check' e
        return (i, reportInferred i)
    | t == i = do
        _ <- check' e
        return (t, reportInferredSpecified i t)
    | otherwise = Left (Nothing, reportInferredSpecified i t)

check (LAST.STypedExpressionContainer x) = check x

check' :: LAST.AnnotatedExpression -> TypeJudgement
check' (LAST.STerm t)              = Right (derive t, reportInferred $ derive t)
check' (LAST.SUnExpression _ e)    = check e
check' (LAST.SBinExpression o e f) = do
    (rt, _) <- check e
    (lt, _) <- check f
    let ct = solve o rt lt
    case ct of
        Nothing -> Left (ct, reportInferred ct)
        Just _  -> Right (ct, reportInferred ct)
