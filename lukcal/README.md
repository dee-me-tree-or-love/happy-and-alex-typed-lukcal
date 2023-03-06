# lukcal

The Haskell-based toolkit for *LukCal* language.

## Setup

- [`./app`](./app/) - Defines the executable `lukcal`
- [`./src`](./src/) - Defines the library/backend for `lukcal`
  - [`./src/CLI`](./src/CLI/) - Defines the CLI interface and handling
  - [`./src/Language`](./src/Language/) - Defines the *LukCal* language tooling

## Development

### 🚚 `stack build` - builds the project

### 💨 `stack run -- [Arguments]` - executes a freshly built project

#### Getting help

```bash
$ stack run -- --help
The lukcal program

lukcal [COMMAND] ... [OPTIONS]

Common flags:
  -c --checktypes=ITEM
  -i --infertypes=ITEM
  -? --help             Display help message
  -V --version          Print version information

lukcal exec [OPTIONS]

  -e --eval=ITEM        Expression to evaluate

lukcal file [OPTIONS]

  -e --eval=FILE        File to evaluate
```

#### Evaluating expressions

> Expression evaluation is based on simple evaluation logic,
> defined in [`./src/Language/Evaluator.hs`](./src/Language/Evaluator.hs)

```bash
$ stack run -- exec --eval "+ 1 (+ 1 2)"
"Right (NumberResult 4)"

$ stack run -- exec --eval "+ cup cake"
"Right (TextResult \"cupcake\")"

$ stack run -- exec --eval "+ 1 cake"
"Left \"Unsupported operator: +, for inputs: Right (NumberResult 1), and Right (TextResult \\\"cake\\\")\""
```

#### Type inferrence

> Type inference is based on simple inference logic,
> defined in [`./src/Language/TypeChecker.hs`](./src/Language/TypeChecker.hs)

```bash
$ stack run -- exec --infer "+ 1 (+ 1 2)"
"Just (STypeHint \"Number\")"

$ stack run -- exec --infer "+ cup cake"
"Just (STypeHint \"Text\")"

$ stack run -- exec --infer "+ 1 cake"
"Nothing"
```

#### Type checking

> Type inference is based on naive checking logic on top of type-annotated trees,
> defined in [`./src/Language/TypeChecker.hs`](./src/Language/TypeChecker.hs)

```bash
$ stack run -- exec --check "+ 1 (+ 1 2)"
"Right (Just (STypeHint \"Number\"),\"Inferred type is: Just (STypeHint \\\"Number\\\")\")"

$ stack run -- exec --check "+ cup cake"
"Right (Just (STypeHint \"Text\"),\"Inferred type is: Just (STypeHint \\\"Text\\\")\")"

$ stack run -- exec --check "+ 1 cake"
"Left (Nothing,\"Inferred type is: Nothing, specified: Nothing\")"

$ stack run -- exec --check "~~ Number + 1 (+ 1 2)"
"Right (Just (STypeHint \"Number\"),\"Inferred type is: Just (STypeHint \\\"Number\\\"), specified: Just (STypeHint \\\"Number\\\")\")"

$ stack run -- exec --check "~~ Text + 1 (+ 1 2)"
"Left (Nothing,\"Inferred type is: Just (STypeHint \\\"Number\\\"), specified: Just (STypeHint \\\"Text\\\")\")"
```

### 📦 `stack install` - to make *LukCal* available globally

> After this, all examples in
> [section above](#stack-run----arguments---executes-a-freshly-built-project)
> can replace `stack run --` with `lukcal-cli`

#### 🧰 `lukcal-cli` is available locally after installing

```bash
$ lukcal-cli exec --eval "+ cup cake"
"Right (TextResult \"cupcake\")"

$ lukcal-cli exec --infer "+ 1 (+ 1 2)"
"Just (STypeHint \"Number\")"

$ lukcal-cli exec --check "~~ Text + 1 cake"
"Left (Nothing,\"Inferred type is: Nothing, specified: Just (STypeHint \\\"Text\\\")\")"
```
