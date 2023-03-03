# Happy & Alex: Typed **Pol**ish Notation (**Łuk**asiewicz) **Cal**culator

> 🪄 This is a project for experimenting with
> [Happy](https://haskell-happy.readthedocs.io/en/latest/) and
> [Alex](https://haskell-alex.readthedocs.io/en/latest/).  
> 🙃 This is a very simplistic language with no practical use intended,
> but is certainly encouraged.

This is a home of the `pollukcal` language.

It is a Polish notation inspired language implementation
using Happy and Alex Haskell libraries.

```haskell
-- #file: pollukcal-demo.pol
-- This is a comment

-- This is a valid pollukcal expression:
~~ Number (+ 1 2)
-- Evaluating which will produce: Right (NumberResult 3)

-- This is a valid pollukcal expression too:
~~ Number (+ cat mouse)
-- Evaluating which will produce: Right (TextResult catmouse)
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

## Implementation

The `pollukcal` toolkit is implemented using Haskell in [`./pollukcal/`](./pollukcal/).

### Installing

```bash
$ cd pollukcal && stack install && cd ..
$ pollukcal --version
0.1.0 # or something similar?
```

### Toolkit usage

#### Evaluation

```bash
$ pollukcal act --eval pollukcal-demo.pol
Right (NumberResult 3)
Right (TextResult catmouse)
```

#### Type checking

```bash
$ pollukcal act --type-check pollukcal-demo.pol
Right "OK"
```

#### Exploration

```bash
$ pollukcal act --ast pollukcal-demo.pol
-- TODO: implement the AST output
```
