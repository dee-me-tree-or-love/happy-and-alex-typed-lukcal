module Language.Evaluator (eval, EvalOutput) where

import qualified Language.Ast as LAST

-- TODO: make this a functor
data EvalResult
    = NumberResult Int
    | TextResult String
    | ErrorResult String
    deriving (Eq, Show)

type EvalOutput = Either String EvalResult

eval :: LAST.TypedExpression -> EvalOutput
eval (LAST.STypedExpression _ x) = compute x
eval (LAST.SUntypedExpression x) = compute x

-- Computing the expressions
-- ~~~~~~~~~~~~~~~~~~~~~~~~~

compute :: LAST.Expression -> EvalOutput
-- leaf
compute (LAST.STerm t)                = get t
-- wrapped expression
compute (LAST.SExpressionContainer e) = compute e
-- unary expression
compute (LAST.SUnExpression o e)      = sApply o $ compute e
-- binary expression
compute (LAST.SBinExpression o e1 e2) = bApply o (compute e1) (compute e2)

get :: LAST.Term -> EvalOutput
get (LAST.SNumber n) = Right $ NumberResult n
get (LAST.SText t)   = Right $ TextResult t

sApply :: LAST.Operator -> EvalOutput -> EvalOutput
sApply (LAST.SOperator '+') v = v
sApply (LAST.SOperator o) v = Left $ "Unsupported operator: " ++ [o] ++ ", for input: " ++ show v

bApply :: LAST.Operator -> EvalOutput -> EvalOutput -> EvalOutput
-- Numbers
bApply (LAST.SOperator '+') (Right (NumberResult x)) (Right (NumberResult y)) = Right $ NumberResult $ x + y
bApply (LAST.SOperator '-') (Right (NumberResult x)) (Right (NumberResult y)) = Right $ NumberResult $ x - y
bApply (LAST.SOperator '*') (Right (NumberResult x)) (Right (NumberResult y)) = Right $ NumberResult $ x * y
-- Text
bApply (LAST.SOperator '+') (Right (TextResult x)) (Right (TextResult y)) = Right $ TextResult $ x ++ y
-- No runtime support
bApply (LAST.SOperator o) u v = Left $ "Unsupported operator: " ++ [o] ++ ", for inputs: " ++ show u ++ ", and " ++ show v
