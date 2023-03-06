module Language.Readme () where

{- 
    This module contains all the utilities required for
    interpreting the Typed PolLukCal syntax.

    PolLukCal is a basic (non-practical) calculator language inspired on Polish notation.

    Contents:
        - [x] Main.hs           - the core of the Simple Polcal module
        - [x] Lexer.x           - to tokenize the raw Polcal input
        - [x] Tokens.hs         - defining all the lexemes for Polcal
        - [x] Ast.hs            - defining the syntax tree constructs for Polcal
        - [x] Parser.y          - to parse the tokenized Polcal input into an AST
        - [x] TypeChecker.hs    - to faciliate the type checking of simple Polcal expressions
        - [x] Evaluator.hs      - to evaluate the Polcal expressions

    Features:
        - [x] Basic lexing
        - [x] Basic parsing
        - [x] Type hint lexing
        - [x] Type hint parsing
        - [x] Type inference
        - [x] Type checking
        - [x] Evaluation
 -}
