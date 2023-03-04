module Language.Main
  ( main,
    getTokens,
    getAst,
    inferType,
    checkTypes,
    evalExpression,
  )
where

import qualified Language.Ast         as LAST (TypeHint, TypedExpression)
import qualified Language.Evaluator   as LE (EvalOutput, eval)
import qualified Language.Lexer       as LL (scan)
import qualified Language.Parser      as LP (parse)
import qualified Language.Tokens      as LT (Token)
import qualified Language.TypeChecker as LTC (check, infer)

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
-- SExpression (SBinExpression (SOperator '+') (SExpression (STerm (SNumber 1))) (SExpression (STerm (SNumber 2))))
--
-- >>> getAst $ getTokens "~~ Number + 1 2"
-- STypedExpression (STypeHint "Number") (SBinExpression (SOperator '+') (SExpression (STerm (SNumber 1))) (SExpression (STerm (SNumber 2))))
--
-- >>> getAst $ getTokens "~~ Number + 1 (~~ Number - 1 2)"
-- STypedExpression (STypeHint "Number") (SBinExpression (SOperator '+') (SExpression (STerm (SNumber 1))) (SExpression (SExpressionContainer (STypedExpression (STypeHint "Number") (SBinExpression (SOperator '-') (SExpression (STerm (SNumber 1))) (SExpression (STerm (SNumber 2))))))))
--
-- >>> getAst $ getTokens "(+ 1 2)"
-- SExpression (SExpressionContainer (SExpression (SBinExpression (SOperator '+') (SExpression (STerm (SNumber 1))) (SExpression (STerm (SNumber 2))))))
--
-- >>> getAst $ getTokens "(+ 1)"
-- SExpression (SExpressionContainer (SExpression (SUnExpression (SOperator '+') (SExpression (STerm (SNumber 1))))))
--
-- >>> getAst $ getTokens "(+ (+ 1 2))"
-- SExpression (SExpressionContainer (SExpression (SUnExpression (SOperator '+') (SExpression (SExpressionContainer (SExpression (SBinExpression (SOperator '+') (SExpression (STerm (SNumber 1))) (SExpression (STerm (SNumber 2))))))))))
--
-- >>> getAst $ getTokens "~~ Number (+ (+ 1 2))"
-- STypedExpression (STypeHint "Number") (SExpressionContainer (SExpression (SUnExpression (SOperator '+') (SExpression (SExpressionContainer (SExpression (SBinExpression (SOperator '+') (SExpression (STerm (SNumber 1))) (SExpression (STerm (SNumber 2))))))))))
--
getAst :: [LT.Token] -> LAST.TypedExpression
getAst = LP.parse

-- | Assigns expression types.
--
-- >>> inferType $ getAst $ getTokens "~~ Number (- (+ 1 2))"
-- STypeHint "Number"
--
-- >>> inferType $ getAst $ getTokens "(- (+ 1 2))"
-- STypeHint "Number"
--
-- >>> inferType $ getAst $ getTokens "(- (+ cat (+ flower grass)))"
-- STypeHint "Text"
--
-- >>> inferType $ getAst $ getTokens "(- (~~ Number + cat mouse))"
-- STypeHint "Text"
--
-- >>> inferType $ getAst $ getTokens "(+ (+ mouse bread))"
-- STypeHint "Text"
--
-- >>> inferType $ getAst $ getTokens "(+ (+ cat 2))"
-- STypeHint "FAIL"
--
inferType :: LAST.TypedExpression -> LAST.TypeHint
inferType = LTC.infer

-- | Checks the correctness of the typed expression
--
-- >>> checkTypes $ getAst $ getTokens "~~ Number (+ (+ 1 2))"
-- Left "fail"
--
-- >>> checkTypes $ getAst $ getTokens "(+ (+ 1 2))"
-- Left "fail"
--
-- >>> checkTypes $ getAst $ getTokens "(+ (+ cat mouse))"
-- Left "fail"
--
-- >>> checkTypes $ getAst $ getTokens "~~ Number + cat mouse"
-- Left "fail"
--
-- >>> checkTypes $ getAst $ getTokens "(~~ Number + cat mouse)"
-- Left "fail"
--
-- >>> checkTypes $ getAst $ getTokens "~~ Text (~~ Number + cat mouse)"
-- Left "fail"
--
-- >>> checkTypes $ getAst $ getTokens "(+ (~~ Number + cat mouse))"
-- Left "fail"
--
-- >>> checkTypes $ getAst $ getTokens "(+ (+ cat 2))"
-- Left "fail"
--
-- >>> checkTypes $ getAst $ getTokens "~~ String (+ (+ 1 2))"
-- Left "fail"
--
checkTypes :: LAST.TypedExpression -> Either String String
checkTypes = LTC.check

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
  print $ "\nType check:\t" ++ show (checkTypes ast)
  print $ "\nEvaluation:\t" ++ show (evalExpression ast)
