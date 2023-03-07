module Language.Readme () where

{- 
    This module contains all the utilities required for
    interpreting the Typed LukCal syntax.

    LukCal is a basic (non-practical) calculator language inspired on Polish notation.

    Contents:
        - [x] Main.hs           - the core of the Simple LukCal module
        - [x] Lexer.x           - to tokenize the raw LukCal input
        - [x] Tokens.hs         - defining all the lexemes for LukCal
        - [x] Ast.hs            - defining the syntax tree constructs for LukCal
        - [x] Parser.y          - to parse the tokenized LukCal input into an AST
        - [x] TypeChecker.hs    - to faciliate the type checking of simple LukCal expressions
        - [x] Evaluator.hs      - to evaluate the LukCal expressions

    Features:
        - [x] Basic lexing
        - [x] Basic parsing
        - [x] Type hint lexing
        - [x] Type hint parsing
        - [x] Type inference
        - [x] Type checking
        - [x] Evaluation
 -}
