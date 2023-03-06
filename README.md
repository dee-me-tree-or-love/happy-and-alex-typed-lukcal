# Happy & Alex: Typed `Pol`ish Notation (`Łuk`asiewicz) `Cal`culator

This is a home of the `pollukcal` language.

> 🪄 This is a project for experimenting with
> [Happy](https://haskell-happy.readthedocs.io/en/latest/) and
> [Alex](https://haskell-alex.readthedocs.io/en/latest/).  
> 🙃 This is a very simplistic language with no practical use intended,
> but is certainly encouraged.

## What is `PolLukCal`?

It is a Polish notation inspired language implementation
using Happy and Alex Haskell libraries.

```haskell
-- #file: pollukcal-demo.pol
-- This is a comment

-- This is a valid pollukcal expression:
~~ Number + 1 2
-- Evaluating which will produce: Right (NumberResult 3)

-- This is a valid pollukcal expression too:
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

## Language syntax

On a high-level, `pollukcal` is a language supporting a prefix-form binary and unary expressions, with optional type hints:

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
> in [`./pollukcal/src/Language/Parser.y`](./pollukcal/src/Language/Parser.y)

## Implementation

The `pollukcal` toolkit is implemented using Haskell in [`./pollukcal/`](./pollukcal/).

### Installing

```bash
$ cd pollukcal && stack install && cd ..
$ pollukcal-cli --help
The pollukcal program

pollukcal [COMMAND] ... [OPTIONS]

Common flags:
  -c --checktypes=ITEM
  -i --infertypes=ITEM
  -? --help             Display help message
  -V --version          Print version information

pollukcal exec [OPTIONS]

  -e --eval=ITEM        Expression to evaluate

pollukcal file [OPTIONS]

  -e --eval=FILE        File to evaluate
```

### Toolkit usage

> 🏗️ NB: the toolkit is still in development, so the output is not the best,
> all suggestions are very welcome!

#### Evaluation

```bash
$ pollukcal-cli exec --eval "~~ Text + cup cake"
"Right (TextResult \"cupcake\")"
```

#### Type inference

```bash
$ pollukcal-cli exec --infer "~~ Text + cup cake"
"Just (STypeHint \"Text\")"
```

#### Type checking

```bash
$ pollukcal-cli exec --check "~~ Text + cup cake"
"Right (Just (STypeHint \"Text\"),\"Inferred type is: Just (STypeHint \\\"Text\\\"), specified: Just (STypeHint \\\"Text\\\")\")"
```
