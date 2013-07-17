# seqex

Sequence Expressions. Similar to regular expressions but able to describe
arbitrary sequences of values (not just characters).

## Status

The core expression engine works and can be used to describe very complex
patterns. It is not quick compared to regexes but I've used it to parse hundreds
of files in a few seconds. The big item missing at this point is an analyzer for
optimizing a sequence expression tree. I would not consider the API solid just
yet but that will improve through use.

## Quick Start

```clojure
(require '[n01se.seqex :as se])

(defn digit? [x] (contains? (set "123456789") x))
(defn alpha? [x] (contains? (set "abcdefghijklmnopqrstuvwxyz") x))
(def num (se/qty+ digit?))
(def word (se/ord alpha? (se/qty* alpha? digit?)))

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
* `se/model`: returns just the end models.

## Composing Seqexes

Most of the time, Seqexes can be composed of the following Seqexes plus
Clojure's standard values:

* `se/ord`: an ordered sequence of seqexes.
* `se/alt`: alternate seqexes (choosing one).
* `se/opt`: optional seqexes (choosing zero or one).
* `se/qty+`: one or more repeating seqexes (in any order).
* `se/qty*`: zero or more repeating seqexes (in any order).
* `se/qty`: a specific number of repeating seqexes (in any order).
* `se/all`: all seqexes in any permutation.

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
Seqex.

Nested expressions (e.g., a tree structure) may be expressed with `se/subex`.

### Old API (too confusing and cryptic)

To choose between multiple seqexes, use the c1, c? etc functions:

* (c1 & seqexes): matches any one of the given seqexes.
* (c? & seqexes): matches zero or any one of the given seqexes.
* (c+ & seqexes): matches one or more of the given seqexes in any order.
* (c\* & seqexes): matches zero or more of the given seqexes in any order.
* (cx x & seqexes): matches any of the seqexes exactly x times.
* (cr [n m] & seqexes): matches any of the seqexes between n and m times.

To order multiple seqexes sequentially, use the s1, s?, etc functions.

* (s1 & seqexes): matches all seqexes in order.
* (s? & seqexes): matches none or all seqexes in order.
* (s+ & seqexes): matches all seqexes in order one or more times.
* (s\* & seqexes): matches all seqexes in order zero or more times.
* (sx x & seqexes): matches all seqexes in order repeated x times.
* (sr [n m] & seqexes): matches all seqexes in order between n and m times.

## Basic Concepts (protocols, verdicts and models)

### SeqEx Protocol

All sequence expressions implement the SeqEx protocol defined as three
functions: `se/-begin`, `se/-continue`, and `se/-end`. These functions are
called before, during and after processing a sequence of tokens respectively.
`-begin` returns an inital verdict (i.e., a verdict after looking at a zero
length sequence) and the initial state of the Seqex. `-continue` takes the
previous state of the Seqex and the next token in the sequence and returns a new
Seqex state and new verdict. `-end` takes the last Seqex state and returns any
final model of the Seqex (Often used to capture tokens: see below for notes on
models).

### Verdicts

The first purpose of any sequence expression (Seqex) is to constrain a sequence
of tokens by returning a 'verdict' after examining each token. Verdicts are
actually a pair of boolean values: one indictating matching (or not matching)
and the other indicating continuing (or not continuing). If the verdict
indicates matching, it means that the specific constraints of the Seqex have
been fully met through the most recent token examined. On the other hand, If the
verdict indicates continuing, then the Seqex is ready to examine the next token
in the sequence (if any).

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

The second purpose of sequence expressions is to build up and return a 'model'
based on the tokens examined. Technically, a Seqex is expected to return a
(possibly empty) sequence of models.

The model(s) are finalized and returned when `se/-end` is called and the
model(s) can be initialized and updated up when `se/-begin` and `se/-continue`
are called. Most standard Seqexes do not build any model at all and ususally
return an empty sequence of models. The exceptions to this are the standard
capturing seqexes: `se/cap`, `se/cap-one` and `se/recap`.

`se/cap` and `se/cap-one` require a Seqex and both may have an optional finalize
function. `se/cap` captures all tokens examined by Seqex that were not judged
with an Invalid verdict. `se/cap-one` captures only the last non-Invalid token.
The captured token(s) are either then immediately returned or are passed to
finalize and its return value is used in their place. Any sub-models created by
the Seqex under `se/cap` or `se/cap-one` are then appended after the captured
tokens.

In constrast, `se/recap` does not capture tokens at all. Instead it wraps a
Seqex (like `se/cap`) and requires a finalize function which takes all models
captured by the wrapped Seqex. The return value of finalize must be a sequence
of zero or more models.

Most of the standard Seqexes (e.g., `se/ord`, `se/qty+`) will return a list of
models from seqexes below them. Also, `subex` will return a sequence of models
built from the Seqex it wraps.

There are a lot of interesting possiblities with model building but I need to
experiment with this before saying much more about it. I will say that combining
constraints with arbitrary model building is quite powerful. You have been
warned.

### Examples

Examples can be found in the examples directory.
* `math.clj` parse a string of infix math expressions.
* `trees.clj` reshape tree data structures.

## License

Copyright (C) 2013 Jonathan Claggett

Distributed under the Eclipse Public License, the same as Clojure.
