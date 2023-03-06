module Language.Main
  (
    getTokens,
    getAst,
    inferType,
    annotateAst,
    checkTypes,
    evalExpression,
    stringToAst,
  )
where

import qualified Language.Ast         as LAST (AnnotatedTypedExpression,
                                               TypeHint, TypedExpression)
import qualified Language.Evaluator   as LE (EvalOutput, eval)
import qualified Language.Lexer       as LL (scan)
import qualified Language.Parser      as LP (parse)
import qualified Language.Tokens      as LT (Token)
import qualified Language.TypeChecker as LTC (TypeJudgement, annotate, check,
                                              infer)


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


-- | Combines tokenizer and parser to produce AST.
--
stringToAst :: String -> LAST.TypedExpression
stringToAst = getAst . getTokens


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
-- Parse error: [TLeftBrace,TOperator '+',TLeftBrace,TOperator '+',TNumber 1,TNumber 2,TRightBrace,TRightBrace]
--
-- >>> evalExpression $ getAst $ getTokens "(+ (+ cat 2))"
-- Left "Unsupported operator: +, for inputs: Right (TextResult \"cat\"), and Right (NumberResult 2)"
--
evalExpression :: LAST.TypedExpression -> LE.EvalOutput
evalExpression = LE.eval


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


-- | Annotates the expressions with inferred types.
--
-- >>> annotateAst $ getAst $ getTokens "~~ Number - (+ 1 2)"
-- STypedExpression (Just (STypeHint "Number"),Just (STypeHint "Number")) (SUnExpression (SOperator '-') (STypedExpression (Nothing,Just (STypeHint "Number")) (SBinExpression (SOperator '+') (STypedExpression (Nothing,Just (STypeHint "Number")) (STerm (SNumber 1))) (STypedExpression (Nothing,Just (STypeHint "Number")) (STerm (SNumber 2))))))
--
-- >>> annotateAst $ getAst $ getTokens "(- (+ 1 2))"
-- STypedExpression (Nothing,Just (STypeHint "Number")) (SUnExpression (SOperator '-') (STypedExpression (Nothing,Just (STypeHint "Number")) (SBinExpression (SOperator '+') (STypedExpression (Nothing,Just (STypeHint "Number")) (STerm (SNumber 1))) (STypedExpression (Nothing,Just (STypeHint "Number")) (STerm (SNumber 2))))))
--
-- >>> annotateAst $ getAst $ getTokens "(- (+ cat (+ flower grass)))"
-- STypedExpression (Nothing,Just (STypeHint "Text")) (SUnExpression (SOperator '-') (STypedExpression (Nothing,Just (STypeHint "Text")) (SBinExpression (SOperator '+') (STypedExpression (Nothing,Just (STypeHint "Text")) (STerm (SText "cat"))) (STypedExpression (Nothing,Just (STypeHint "Text")) (SBinExpression (SOperator '+') (STypedExpression (Nothing,Just (STypeHint "Text")) (STerm (SText "flower"))) (STypedExpression (Nothing,Just (STypeHint "Text")) (STerm (SText "grass"))))))))
--
-- >>> annotateAst $ getAst $ getTokens "(- (~~ Number + cat mouse))"
-- STypedExpression (Nothing,Just (STypeHint "Text")) (SUnExpression (SOperator '-') (STypedExpression (Just (STypeHint "Number"),Just (STypeHint "Text")) (SBinExpression (SOperator '+') (STypedExpression (Nothing,Just (STypeHint "Text")) (STerm (SText "cat"))) (STypedExpression (Nothing,Just (STypeHint "Text")) (STerm (SText "mouse"))))))
--
-- >>> annotateAst $ getAst $ getTokens "(+ (+ mouse bread))"
-- STypedExpression (Nothing,Just (STypeHint "Text")) (SUnExpression (SOperator '+') (STypedExpression (Nothing,Just (STypeHint "Text")) (SBinExpression (SOperator '+') (STypedExpression (Nothing,Just (STypeHint "Text")) (STerm (SText "mouse"))) (STypedExpression (Nothing,Just (STypeHint "Text")) (STerm (SText "bread"))))))
--
-- >>> annotateAst $ getAst $ getTokens "(+ (+ cat 2))"
-- STypedExpression (Nothing,Nothing) (SUnExpression (SOperator '+') (STypedExpression (Nothing,Nothing) (SBinExpression (SOperator '+') (STypedExpression (Nothing,Just (STypeHint "Text")) (STerm (SText "cat"))) (STypedExpression (Nothing,Just (STypeHint "Number")) (STerm (SNumber 2))))))
--
annotateAst :: LAST.TypedExpression -> LAST.AnnotatedTypedExpression
annotateAst = LTC.annotate

-- | Checks the annotated AST for type safety.
--
-- >>> checkTypes $ annotateAst $ getAst $ getTokens "~~ Number - (+ 1 2)"
-- Right (Just (STypeHint "Number"),"Inferred type is: Just (STypeHint \"Number\"), specified: Just (STypeHint \"Number\")")
--
-- >>> checkTypes $ annotateAst $ getAst $ getTokens "(- (+ 1 2))"
-- Right (Just (STypeHint "Number"),"Inferred type is: Just (STypeHint \"Number\")")
--
-- >>> checkTypes $ annotateAst $ getAst $ getTokens "(- (+ cat (+ flower grass)))"
-- Right (Just (STypeHint "Text"),"Inferred type is: Just (STypeHint \"Text\")")
--
-- >>> checkTypes $ annotateAst $ getAst $ getTokens "(- (~~ Number + cat mouse))"
-- Left (Nothing,"Inferred type is: Just (STypeHint \"Text\"), specified: Just (STypeHint \"Number\")")
--
-- >>> checkTypes $ annotateAst $ getAst $ getTokens "(+ (+ mouse bread))"
-- Right (Just (STypeHint "Text"),"Inferred type is: Just (STypeHint \"Text\")")
--
-- >>> checkTypes $ annotateAst $ getAst $ getTokens "(+ (+ cat 2))"
-- Left (Nothing,"Inferred type is: Nothing, specified: Nothing")
--
checkTypes :: LAST.AnnotatedTypedExpression -> LTC.TypeJudgement
checkTypes = LTC.check
