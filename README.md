# hs-lisp

Lisp interperter and compiler written in haskell.

## How to use
Run program to open REPL.

### REPL commands:
- `:q` - stop REPL.
- `:r` - reset REPL context.
- `:f <file>` - evaluate lisp expressions from file.
- `:js <file>` - compile lisp expressions from file to javascript.
- *lisp expressions* - evaluate lisp expressions.

### Sample lisp programs:
- lisp-programs/bin-search.lisp - binary search sample.
- lisp-programs/game.lisp - another binary search sample.

### Supported special forms:
- `if`
- `defun`
- `setq`
- `let`

### Supported lisp constants and functions:
- `nil`
- `read-int()`
- `print(x)`
- `princ(x)`
- `+(&rest xs)`
- `-(num &rest xs)`
- `/(num &rest xs)`
- `=(a b)`
- `<(a b)`
- `>(a b)`
- `>=(a b)`
- `atomp(x)`
- `numberp(x)`
- `listp(x)`
- `cons(a b)`
- `car(x)`
- `cdr(x)`
- `list(&rest xs)`
- `floor(x)`
- `append(&rest xs)`
- `null(x)`
- `first(x)`
- `rest(x)`
- `seq(&rest xs)`

## Prerequisites

This project relies on the [Haskell Stack tool](https://docs.haskellstack.org/en/stable/README/).

It is recommended to get Stack with batteries included by
installing [Haskell Platform](https://www.haskell.org/platform/).

## Build

To build this project simply run

```sh
stack build
```

This will install all dependencies, including a proper version of GHC
(which should be there already if you have Haskell Platform 8.4.3).

## Run

This project has one executable that you can run with

```
stack exec hs-lisp-exe
```

During development it is recommended a combination of `build` and `exec`:

```
stack build && stack exec hs-lisp-exe
```

Alternatively, you can run

```
stack build file-watch
```

For continuous builds in the background.

## Interpreter

You can run GHCi (GHC interpreter) for the whole project with

```
stack ghci
```

or

```
stack repl
```

During development it might be beneficial to work using an interpreter
for quick reloads, interacting with various parts of code and
trying out things.

Note that you can run executable from GHCi by invoking `main`.

## `ghcid`

For faster feedback from the compiler it is recommended to use `ghcid`
(GHCi deamon).

Install `ghcid` with `stack`:

```
stack install ghcid
```

Now you can run `ghcid` with Stack using

```
ghcid -c "stack repl"
```

This will run GHCi with the entire project loaded and will
quickly reload all modules when you modify them to tell you
if you have any errors or warnings.
