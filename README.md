# seqex

Sequence Expressions. Similar to regular expressions but able to describe
arbitrary sequences of values (not just characters).

## Status

Updated to 2.0 - generally simplified the API, added new syntax library.
Added syntax.clj which applies seqexes to the task of defining macros.
Added a couple of example files: trees.clj and math.clj.

## Quick Start

```clojure
(require '[n01se.seqex :as se])

(defn digit? [x] (contains? (set "123456789") x))
(defn alpha? [x] (contains? (set "abcdefghijklmnopqrstuvwxyz") x))
(def num (se/rep+ digit?))
(def word (se/cat alpha? (se/rep* alpha? digit?)))

(se/valid? num "42")       ;=> true
(se/valid? num "")         ;=> false
(se/valid? word "hello42") ;=> true
(se/valid? word "4hello2") ;=> false
```

## Usage

These functions are available to work with Seqexes by applying them to a
sequence of tokens.

* `se/exec`: returns both the end models and a final verdict of Matching or Invalid.
* `se/valid?`: returns true if the input stream matched the seqex constraints.
* `se/parse`: returns captured models, prints an error otherwise.

## Composing Seqexes

Most of the time, Seqexes can be composed of the following Seqexes plus
Clojure's standard values:

* `se/cat`: concatenated seqexes.
* `se/alt`: alternate seqexes (choosing one).
* `se/opt`: optional seqexes (choosing zero or one).
* `se/rep+`: one or more repeating seqexes (in any order).
* `se/rep*`: zero or more repeating seqexes (in any order).
* `se/rep`: a specific number of repeating seqexes (in any order).

Clojure's standard values: numbers, characters, strings, symbols, keywords,
lists, vectors, sets, and maps are all extended to implement the SeqEx protocol
such that they match exactly one occurance of themselves. Examples:

```clojure
(se/valid? \a "a")          ;=> true
(se/valid? \a "")           ;=> false
(se/valid? 1 [1])           ;=> true
(se/valid? 1 [1 1])         ;=> false
(se/valid? {:a 1} [{:a 1}]) ;=> true
```

Functions are treated as predicates that must be true for exactly one token in a
sequence.

Recursive expressions are defined by using delays which are assumed to wrap a
Seqex. See math.clj for an example.

Nested expressions (e.g., a tree structure) may be expressed with `se/subex`.

## Basic Concepts (protocols, verdicts and models)

### SeqEx Protocol

All sequence expressions implement the SeqEx protocol defined primarily by two
methods `se/begin-` and `se/continue-`. These functions are called before and
during parsing a sequence of tokens respectively. `se/begin-` returns an inital
verdict (i.e., a verdict after looking at a zero length sequence) and the
initial state of the Seqex. `se/continue-` takes the previous state of the Seqex
and the next token in the sequence and returns a new Seqex state and new
verdict. `se/continue-` is repeatedly called with tokens until no more tokens
remain or the returned verdict is `se/Passed` or `se/Failed`.

Once the sequence expression is done being applied to the tokens, one of the two
secondary methods `se/error-` or `se/model-` is applied to the state if the
final verdict is `se/Failed` or `se/Passed` respectively.

### Verdicts

Underlying all sequence expressions is the idea of returning a 'verdict' after
examining each token. Verdicts are actually a pair of boolean values: one
indictating matching (or not matching) and the other indicating continuing (or
not continuing). If the verdict indicates matching, it means that the specific
constraints of the Seqex have been fully met through the most recent token
examined. On the other hand, If the verdict indicates continuing, then the Seqex
is ready to examine the next token in the sequence (if any). There are four
possible verdicts: `se/Passing`, `se/Failing`, `se/Passed` and `se/Failed`.

As an example of how verdicts might look over time, assume a specific Seqex that
requires a sequence of characters `\c \a` followed by one or more `\t`. The
verdicts for the input sequence `"cattle"` would look like this:

```clojure
[ \c \a \t \t \l \e ] ;; Input sequence
 0  0  0  1  1  0  ,  ;; Matching bit (commas indicate not examined)
 1  1  1  1  1  0  ,  ;; Continue bit
```

Another example, this time matching `\c \a` followed by an optional `\t`. The
verdicts for the same input sequence are:

```clojure
[ \c \a \t \t \l \e ] ;; Input sequence
 0  0  1  1  ,  ,  ,  ;; Matching bit (commas indicate not examined)
 1  1  1  0  ,  ,  ,  ;; Continue bit
```

By specifing both flags, a Seqex is able to communicate both when the input
tokens are matching and when it is done examining the input tokens.

### Models

When a seqex has passed or is passing with no more tokens, a sequence of
'models' are extracted from the tokens examined.

The models are finalized and returned when `se/model-` is called and the models
are initialized and updated up when `se/begin-` and `se/continue-` are called.
Most standard Seqexes do not build any models at all and so return an empty
sequence of models. The exceptions to this are the standard capturing seqexes:
`se/cap`, `se/recap`, and `se/recap-map`.

`se/cap` is used to capture a vector of all tokens examined by the seqex
enclosed by `se/cap`. If an optional function is given, it is applied to the
vector of tokens. In either case, any the captured result is consed onto the
sequence of models returned by the enclosed seqex.

`se/recap` is used to combine all models captured by its enclosed seqex. This is
accomplished by applying a function to the sequence of models captured and
treating the return value of the function a single model.

`se/recap-map` applies the function to all captured models and must return a
sequence of models.

Most of the standard Seqexes (e.g., `se/cat`, `se/rep+`) will return a list of
models from seqexes below them. Also, `se/subex` will return a list of models
built from the Seqex it wraps.

There are a lot of interesting possiblities with model building but I need to
experiment with this before saying much more about it. I will say that combining
constraints with arbitrary model building is quite powerful. You have been
warned.

### Errors

When a seqex has failed or is failing with no more tokens, an error message is
returned based on the state of the seqex. At the moment, this error message
is represented as a sequence of strings to be printed.

### Examples

Examples can be found in the examples directory.
* `math.clj` parse a string of infix math expressions.
* `trees.clj` reshape tree data structures.

### Macro Syntax
There is a new syntax.clj file that defines a first attempt at applying seqexes
to macro parameters and providing syntax documentation in addition to the
existing error messages.

## License

Copyright (C) 2013 Jonathan Claggett

Distributed under the Eclipse Public License, the same as Clojure.
