# Happy & Alex: Typed Polish Notation (`Łuk`asiewicz) `Cal`culator

This is a home of the `lukcal` language.

> 🪄 This is a project for experimenting with
> [Happy](https://haskell-happy.readthedocs.io/en/latest/) and
> [Alex](https://haskell-alex.readthedocs.io/en/latest/).  
> 🙃 This is a very simplistic language with no practical use intended,
> but is certainly encouraged.

## What is `LukCal`?

It is a Polish notation inspired language implementation
using Happy and Alex Haskell libraries.

```haskell
~~ Number + 1 2
-- Evaluating which will produce: Right (NumberResult 3)

~~ Text + cup cake
-- Evaluating which will produce: Right (TextResult "cupcake")
```

<!-- markdownlint-disable MD033 -->
<details>
<summary>Additional reading</summary>

- <https://en.wikipedia.org/wiki/Polish_notation>
- <https://en.wikipedia.org/wiki/Jan_%C5%81ukasiewicz>
- <https://haskell-happy.readthedocs.io/en/latest/>
- <https://haskell-alex.readthedocs.io/en/latest/>

</details>

### Language syntax

On a high-level, `lukcal` is a language supporting a prefix-form binary and unary expressions, with optional type hints:

```abnf title="High-level language syntax"
PROGRAM       = *(comment / expression)
comment       = "--" text
expression    = ["~~" hint] (operator term / operator term term / "(" expression ")")
hint          = text
term          = (text / number / expression)
operator      = ("+" / "-" / "*" / "/")
text          = 1*ALPHA ; any text
number        = 1*DIGIT ; any natural (with zero)
```

> ***Language feature backlog**:*
>
> - [x] unary operations
> - [x] binary operations
> - [x] comments
> - [x] type hints
> - [ ] flexible operator arity
>
> 💡 Detailed implementation of the supported syntax is
> in [`./lukcal/src/Language/Parser.y`](./lukcal/src/Language/Parser.y)

### Examples

```haskell
-- 👀 Expressions in LukCal do not require type hints:
+ 1 (+ 2 (+ 3 4))
-- =={eval}== Right (NumberResult 10)

-- 👌 But you can add them if you would like:
~~ Number + 1 (+ 2 (~~ Number + 3 4))
-- =={type check}== Right (Just (STypeHint "Number"), "Inferred type is: ...")
-- =={eval}== Right (NumberResult 10)

-- 🤲 The type system of LukCal allows operations only on identical types:
~~ Text + chocolate (~~ Text + cup cake)
-- =={type check}== Right (Just (STypeHint "Text"), "Inferred type is: ...")
-- =={eval}== Right (TextResult "chocolatecupcake")

-- 😨 And if the types are not respected, LukCal will not compute the result
+ 1 cake
-- =={type check}== Left (Nothing, "Inferred type is: Nothing, specified: Nothing")
-- =={eval}== Left "Unsupported operator: +, for inputs: Right (NumberResult 1), and Right (TextResult "cup")"
```

> 💡 Detailed implementation of the evaluation is
> in [`./lukcal/src/Language/Evaluator.hs`](./lukcal/src/Language/Evaluator.hs)
> and type checking in
> [`./lukcal/src/Language/TypeChecker.hs`](./lukcal/src/Language/TypeChecker.hs)

## Implementation

The `lukcal` toolkit is implemented using Haskell in [`./lukcal/`](./lukcal/).

### Installing

```bash
$ cd lukcal && stack install && cd ..
$ lukcal-cli --help
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

### Toolkit usage

> 🏗️ NB: the toolkit is still in development, so the output may not be the most
> readable, and all suggestions are very welcome!

#### Evaluation

```bash
$ lukcal-cli exec --eval "~~ Text + cup cake"
"Right (TextResult \"cupcake\")"
```

#### Type inference

```bash
$ lukcal-cli exec --infer "~~ Text + cup cake"
"Just (STypeHint \"Text\")"
```

#### Type checking

```bash
$ lukcal-cli exec --check "~~ Text + cup cake"
"Right (Just (STypeHint \"Text\"),\"Inferred type is: Just (STypeHint \\\"Text\\\"), specified: Just (STypeHint \\\"Text\\\")\")"
```
