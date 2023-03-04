module Language.Main
  ( main,
    getTokens,
    getAst,
    assignTypes,
    checkTypes,
    evalExpression,
  )
where

import qualified Language.Ast         as LAST (TypedExpression)
import qualified Language.Evaluator   as LE (EvalOutput, eval)
import qualified Language.Lexer       as LL (scan)
import qualified Language.Parser      as LP (parse)
import qualified Language.Tokens      as LT (Token)
import qualified Language.TypeChecker as LTC (annotate, check)

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
-- SUntypedExpression (SBinExpression (SOperator '+') (SUntypedExpression (STerm (SNumber 1))) (SUntypedExpression (STerm (SNumber 2))))
--
-- >>> getAst $ getTokens "~~ Number + 1 2"
-- STypedExpression (STypeHint "Number") (SBinExpression (SOperator '+') (SUntypedExpression (STerm (SNumber 1))) (SUntypedExpression (STerm (SNumber 2))))
--
-- >>> getAst $ getTokens "~~ Number + 1 (~~ Number - 1 2)"
-- STypedExpression (STypeHint "Number") (SBinExpression (SOperator '+') (SUntypedExpression (STerm (SNumber 1))) (SUntypedExpression (SExpressionContainer (STypedExpression (STypeHint "Number") (SBinExpression (SOperator '-') (SUntypedExpression (STerm (SNumber 1))) (SUntypedExpression (STerm (SNumber 2))))))))
--
-- >>> getAst $ getTokens "(+ 1 2)"
-- SUntypedExpression (SExpressionContainer (SUntypedExpression (SBinExpression (SOperator '+') (SUntypedExpression (STerm (SNumber 1))) (SUntypedExpression (STerm (SNumber 2))))))
--
-- >>> getAst $ getTokens "(+ 1)"
-- SUntypedExpression (SExpressionContainer (SUntypedExpression (SUnExpression (SOperator '+') (SUntypedExpression (STerm (SNumber 1))))))
--
-- >>> getAst $ getTokens "(+ (+ 1 2))"
-- SUntypedExpression (SExpressionContainer (SUntypedExpression (SUnExpression (SOperator '+') (SUntypedExpression (SExpressionContainer (SUntypedExpression (SBinExpression (SOperator '+') (SUntypedExpression (STerm (SNumber 1))) (SUntypedExpression (STerm (SNumber 2))))))))))
--
-- >>> getAst $ getTokens "~~ Number (+ (+ 1 2))"
-- STypedExpression (STypeHint "Number") (SExpressionContainer (SUntypedExpression (SUnExpression (SOperator '+') (SUntypedExpression (SExpressionContainer (SUntypedExpression (SBinExpression (SOperator '+') (SUntypedExpression (STerm (SNumber 1))) (SUntypedExpression (STerm (SNumber 2))))))))))
--
getAst :: [LT.Token] -> LAST.TypedExpression
getAst = LP.parse

-- | Assigns expression types.
--
-- >>> assignTypes $ getAst $ getTokens "~~ Number (+ (+ 1 2))"
-- STypedExpression (STypeHint "Number") (SExpressionContainer (SUntypedExpression (SUnExpression (SOperator '+') (SUntypedExpression (SExpressionContainer (SUntypedExpression (SBinExpression (SOperator '+') (SUntypedExpression (STerm (SNumber 1))) (SUntypedExpression (STerm (SNumber 2))))))))))
--
-- >>> assignTypes $ getAst $ getTokens "(+ (+ 1 2))"
-- STypedExpression (STypeHint "Number") (SExpressionContainer (SUntypedExpression (SUnExpression (SOperator '+') (SUntypedExpression (SExpressionContainer (SUntypedExpression (SBinExpression (SOperator '+') (SUntypedExpression (STerm (SNumber 1))) (SUntypedExpression (STerm (SNumber 2))))))))))
--
-- >>> assignTypes $ getAst $ getTokens "(+ (+ cat (+ flower grass)))"
-- STypedExpression (STypeHint "Text") (SExpressionContainer (SUntypedExpression (SUnExpression (SOperator '+') (SUntypedExpression (SExpressionContainer (SUntypedExpression (SBinExpression (SOperator '+') (SUntypedExpression (STerm (SText "cat"))) (SUntypedExpression (SExpressionContainer (SUntypedExpression (SBinExpression (SOperator '+') (SUntypedExpression (STerm (SText "flower"))) (SUntypedExpression (STerm (SText "grass"))))))))))))))
--
-- >>> assignTypes $ getAst $ getTokens "(+ (~~ Number + cat mouse))"
-- STypedExpression (STypeHint "Text") (SExpressionContainer (SUntypedExpression (SUnExpression (SOperator '+') (SUntypedExpression (SExpressionContainer (STypedExpression (STypeHint "Number") (SBinExpression (SOperator '+') (SUntypedExpression (STerm (SText "cat"))) (SUntypedExpression (STerm (SText "mouse"))))))))))
--
-- >>> assignTypes $ getAst $ getTokens "(+ (+ mouse bread))"
-- STypedExpression (STypeHint "Text") (SExpressionContainer (SUntypedExpression (SUnExpression (SOperator '+') (SUntypedExpression (SExpressionContainer (SUntypedExpression (SBinExpression (SOperator '+') (SUntypedExpression (STerm (SText "mouse"))) (SUntypedExpression (STerm (SText "bread"))))))))))
--
-- >>> assignTypes $ getAst $ getTokens "(+ (+ cat 2))"
-- STypedExpression (STypeHint "FAIL") (SExpressionContainer (SUntypedExpression (SUnExpression (SOperator '+') (SUntypedExpression (SExpressionContainer (SUntypedExpression (SBinExpression (SOperator '+') (SUntypedExpression (STerm (SText "cat"))) (SUntypedExpression (STerm (SNumber 2))))))))))
--
assignTypes :: LAST.TypedExpression -> LAST.TypedExpression
assignTypes = LTC.annotate

-- | Checks the correctness of the typed expression
--
-- >>> checkTypes $ getAst $ getTokens "~~ Number (+ (+ 1 2))"
-- Right "Okay"
--
-- >>> checkTypes $ getAst $ getTokens "(+ (+ 1 2))"
-- Right "Okay"
--
-- >>> checkTypes $ getAst $ getTokens "(+ (+ cat mouse))"
-- Right "Okay"
--
-- >>> checkTypes $ getAst $ getTokens "~~ Number + cat mouse"
-- Left "Inferred type: STypeHint \"Text\", doesn't match specified: STypeHint \"Number\""
--
-- >>> checkTypes $ getAst $ getTokens "(~~ Number + cat mouse)"
-- Right "Okay"
--
-- >>> checkTypes $ getAst $ getTokens "(+ (~~ Number + cat mouse))"
-- Right "Okay"
--
-- >>> checkTypes $ getAst $ getTokens "(+ (+ cat 2))"
-- Left "Specified failed type: STypeHint \"FAIL\""

-- >>> checkTypes $ getAst $ getTokens "~~ String (+ (+ 1 2))"
-- Left "Inferred type: STypeHint \"Number\", doesn't match specified: STypeHint \"String\""
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
  print $ "\nInferred type:\t" ++ show (assignTypes ast)
  print $ "\nType check:\t" ++ show (checkTypes ast)
  print $ "\nEvaluation:\t" ++ show (evalExpression ast)
