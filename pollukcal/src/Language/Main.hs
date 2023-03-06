module Language.Main
  ( main,
    getTokens,
    getAst,
    inferType,
    evalExpression,
  )
where

import qualified Language.Ast         as LAST (TypeHint, TypedExpression)
import qualified Language.Evaluator   as LE (EvalOutput, eval)
import qualified Language.Lexer       as LL (scan)
import qualified Language.Parser      as LP (parse)
import qualified Language.Tokens      as LT (Token)
import qualified Language.TypeChecker as LTC (infer)

-- | Tokenizes a string.
--
-- Examples:
--
-- >>> getTokens "(+ 2 1)"
-- [TLeftBrace,TOperator '+',TNumber 2,TNumber 1,TRightBrace]
--
-- >>> getTokens "(+ cat 1)"
-- [TLeftBrace,TOperator '+',TText "cat",TNumber 1,TRightBrace]
--
-- >>> getTokens "-- this is a comment"
-- []
--
-- >>> getTokens "~~ TypeHint"
-- [TTypeHintIndicator,TText "TypeHint"]
--
-- >>> getTokens "%%%%% this causes an error"
-- lexical error
--
getTokens :: String -> [LT.Token]
getTokens = LL.scan

-- | Parses the tokenized input into an AST
--
-- >>> getAst $ getTokens "+ 1 2"
-- STypedExpression Nothing (SBinExpression (SOperator '+') (STypedExpression Nothing (STerm (SNumber 1))) (STypedExpression Nothing (STerm (SNumber 2))))
--
-- >>> getAst $ getTokens "~~ Number + 1 2"
-- STypedExpression (Just (STypeHint "Number")) (SBinExpression (SOperator '+') (STypedExpression Nothing (STerm (SNumber 1))) (STypedExpression Nothing (STerm (SNumber 2))))
--
-- >>> getAst $ getTokens "~~ Number + 1 (~~ Number - 1 2)"
-- STypedExpression (Just (STypeHint "Number")) (SBinExpression (SOperator '+') (STypedExpression Nothing (STerm (SNumber 1))) (STypedExpressionContainer (STypedExpression (Just (STypeHint "Number")) (SBinExpression (SOperator '-') (STypedExpression Nothing (STerm (SNumber 1))) (STypedExpression Nothing (STerm (SNumber 2)))))))
--
-- >>> getAst $ getTokens "(+ 1 2)"
-- STypedExpressionContainer (STypedExpression Nothing (SBinExpression (SOperator '+') (STypedExpression Nothing (STerm (SNumber 1))) (STypedExpression Nothing (STerm (SNumber 2)))))
--
-- >>> getAst $ getTokens "(+ 1)"
-- STypedExpressionContainer (STypedExpression Nothing (SUnExpression (SOperator '+') (STypedExpression Nothing (STerm (SNumber 1)))))
--
-- >>> getAst $ getTokens "(+ (+ 1 2))"
-- STypedExpressionContainer (STypedExpression Nothing (SUnExpression (SOperator '+') (STypedExpressionContainer (STypedExpression Nothing (SBinExpression (SOperator '+') (STypedExpression Nothing (STerm (SNumber 1))) (STypedExpression Nothing (STerm (SNumber 2))))))))
--
-- >>> getAst $ getTokens "~~ Number + (+ 1 2)"
-- STypedExpression (Just (STypeHint "Number")) (SUnExpression (SOperator '+') (STypedExpressionContainer (STypedExpression Nothing (SBinExpression (SOperator '+') (STypedExpression Nothing (STerm (SNumber 1))) (STypedExpression Nothing (STerm (SNumber 2)))))))
--
getAst :: [LT.Token] -> LAST.TypedExpression
getAst = LP.parse

-- | Assigns expression types.
--
-- >>> inferType $ getAst $ getTokens "~~ Number - (+ 1 2)"
-- Just (STypeHint "Number")
--
-- >>> inferType $ getAst $ getTokens "(- (+ 1 2))"
-- Just (STypeHint "Number")
--
-- >>> inferType $ getAst $ getTokens "(- (+ cat (+ flower grass)))"
-- Just (STypeHint "Text")
--
-- >>> inferType $ getAst $ getTokens "(- (~~ Number + cat mouse))"
-- Just (STypeHint "Text")
--
-- >>> inferType $ getAst $ getTokens "(+ (+ mouse bread))"
-- Just (STypeHint "Text")
--
-- >>> inferType $ getAst $ getTokens "(+ (+ cat 2))"
-- Nothing
--
inferType :: LAST.TypedExpression -> Maybe LAST.TypeHint
inferType = LTC.infer

-- | Evaluates the expression.
--
-- >>> evalExpression $ getAst $ getTokens "~~ Number (+ (+ 1 2))"
-- Right (NumberResult 3)
--
-- >>> evalExpression $ getAst $ getTokens "~~ Number (+ (- 2 5))"
-- Right (NumberResult (-3))

-- >>> evalExpression $ getAst $ getTokens "(+ (+ 1 2))"
-- Right (NumberResult 3)
--
-- >>> evalExpression $ getAst $ getTokens "(+ (+ cat mouse))"
-- Right (TextResult "catmouse")
--
-- >>> evalExpression $ getAst $ getTokens "~~ String (+ (+ 1 2))"
-- Right (NumberResult 3)
--
-- >>> evalExpression $ getAst $ getTokens "(+ (+ cat 2))"
-- Left "Unsupported operator: +, for inputs: Right (TextResult \"cat\"), and Right (NumberResult 2)"
--
evalExpression :: LAST.TypedExpression -> LE.EvalOutput
evalExpression = LE.eval

main :: IO ()
main = do
  s <- getContents
  let ast = getAst $ getTokens s
  print $ "\nInferred type:\t" ++ show (inferType ast)
  print $ "\nEvaluation:\t" ++ show (evalExpression ast)
