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

## Usage

I'm still working on a reasonable API. For a preview, see examples below.

### Examples

This is a more involved example showing how it is possible to build seqexes by
defining new functions and also showing circular references to create a simple
'grammar' using declare and delay.

```clojure
;; math expressions demo
(def ws
  "Arbitrary amount of whitespace."
  [n* \space \tab])

(defn seq-ws
  "Sequence of expressions interposed with whitespace."
  [& seqexes] (apply se/seq (interpose ws seqexes)))

(defn first-rest*
  "Matching the first and zero or more occurences of the rest of seq."
  [& seqexes]
   (let [[leader & following] (interpose ws seqexes)]
     (se/seq leader [n* (apply se/seq following)])))

(def digits
  "At least one digit."
  (cons n+ "0123456789"))

(def number
  "Integer or real number."
  (se/seq digits [n? (se/seq \. digits)]))

(declare add-ex)
(def atom-ex [n1 number (seq-ws \( (delay add-ex) \) )])
(def pow-ex (first-rest* atom-ex \^ atom-ex))
(def mul-ex (first-rest* pow-ex [n1 \* \/] pow-ex))
(def add-ex (first-rest* mul-ex [n1 \+ \-] mul-ex))
(def math-ex add-ex)

(deftest math-demo
  (check ws
         = ""
         = " "
         = "    "
         ! "    x ")
  (check (seq-ws \a \b \c)
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
         = "12"
         = "0.12"
         = "123.567")
  (check math-ex
         = "1"
         = "(23.0)"
         = "( ( 42 ) )"
         = "1+2"
         = "43-12"
         = "2^(2+2) * 12.3"))
```

## License

Copyright (C) 2013 Jonathan Claggett

Distributed under the Eclipse Public License, the same as Clojure.
