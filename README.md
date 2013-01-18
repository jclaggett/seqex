# seqex

Sequence Expressions. Similar to regular expressions but able to describe
arbitrary sequences of values (not just characters).

## Status

The core expression engine works and can be used to describe very complex
patterns. It is not quick compared to regexes. The big items missing are, in
order of importance:

1. Front end API: find, match, repeat, etc
2. Data capturing: Need a way to capture the interesting bits.
3. Basic optimizations: very little has been done to the existing engine
4. Advanced optimizations: At some point, add an analysis stage that can
   optimize expression trees.

## Defining
Literal values are considered to match a sequence of 1 item containing that
literal.

functions are assumed to be predicates that must be true for all elements in
a sequence.

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

delays are considered sequence expressions but are not dereferenced until
matched against the input sequence.

## Usage

TODO: I'd like to mimic re-find, re-matches, and re-seq.

### Examples

This is a more involved example showing how it is possible to build seqexes by
defining new functions and also showing circular references to create a simple
'grammar' using declare and delay.

```clojure
;; math expressions demo
(def ws
  "Arbitrary amount of whitespace."
  (c* \space \tab))

(defn s1-ws
  "Sequence of expressions interposed with whitespace."
  [& seqexes] (apply s1 (interpose ws seqexes)))

(defn s1*-ws
  "Sequence containing the first and zero or more occurences of the rest
  interposed with ws.  first and rest are also interposed by ws."
  [& seqexes]
   (let [[leader & following] (interpose ws seqexes)]
     (s1 leader (apply s* following))))

(def digits
  "At least one digit."
  (apply c+ "0123456789"))

(def number
  "Integer or real number."
  (s1 (c? \+ \-) digits (s? \. digits)))

(declare add-ex)
(def atom-ex (c1 number (s1-ws \( (delay add-ex) \) )))
(def pow-ex (s1*-ws atom-ex \^ atom-ex))
(def mul-ex (s1*-ws pow-ex (c1 \* \/) pow-ex))
(def add-ex (s1*-ws mul-ex (c1 \+ \-) mul-ex))
(def math-ex add-ex)

(deftest math-demo
  (check ws
         = ""
         = " "
         = "    "
         ! "    x ")
  (check (s1-ws \a \b \c)
         = "abc"
         = "a bc"
         = "ab c"
         = "a  b  c"
         ! " abc"
         ! "abc ")
  (check digits
         ! ""
         = "1"
         = "12"
         = "45223423")
  (check number
         ! ""
         = "1"
         = "-12"
         = "0.12"
         = "-123.567")
  (check math-ex
         = "1"
         = "(23.0)"
         = "( ( 42 ) )"
         = "1++2"
         = "43--12"
         = "2^(2+2) * (-1/(0--1) - 12.3)"))
```

## License

Copyright (C) 2013 Jonathan Claggett

Distributed under the Eclipse Public License, the same as Clojure.
