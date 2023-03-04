{
module Language.Parser (parse) where
import qualified Language.Tokens as LT
import qualified Language.Ast as LAST
import qualified Language.Lexer as LL
}

%name parse
%tokentype{LT.Token}
%error {parseError}

-- Token bindings
%token

    number  { LT.TNumber $$ }
    text    { LT.TText $$ }
    op      { LT.TOperator $$ }
    '('     { LT.TLeftBrace }
    ')'     { LT.TRightBrace }
    thi     { LT.TTypeHintIndicator }

-- Grammar rules
%%

TExp    : thi TH Exp    { LAST.STypedExpression $2 $3 }
        | Exp           { LAST.SUntypedExpression $1 }

TH      : text          { LAST.STypeHint $1 }

Exp     : Op Exp Exp    { LAST.SBinExpression $1 $2 $3 }
        | Op Exp        { LAST.SUnExpression $1 $2 }
        | '(' Exp ')'   { LAST.SExpressionContainer $2 }
        | Term          { LAST.STerm $1 }

Op      : op            { LAST.SOperator $1 }

Term    : number        { LAST.SNumber $1 }
        | text          { LAST.SText $1 }

{
parseError :: [LT.Token] -> a
parseError ts = error $ "Parse error: " ++ (show ts)
}