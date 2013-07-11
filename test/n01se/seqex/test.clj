(ns n01se.seqex.test
  (:require [criterium.core :as crit]
            [n01se.seqex :as se])
  (:use [clojure.test]))

; Unit testing.

(def ! not=) ;; yes, I am that lazy :-)

(defmacro check
  [se & test-pairs]
  (let [se-val (gensym "se")]
  `(let [~se-val ~se]
     ~@(for [[op input] (partition 2 test-pairs)
             :let [is-str (str se " " op " \"" input "\"")]]
         `(is (~op (se/valid? ~se-val ~input) true) ~is-str)))))

(deftest length
  (check se/n0
         = ""
         ! "a")
  (check se/n1
         ! ""
         = "a"
         ! "ab"
         ! "abc")
  (check se/n?
         = ""
         = "a"
         ! "ab"
         ! "abc")
  (check se/n*
         = ""
         = "a"
         = "abc"
         = (repeat 100 \b))
  (check se/n+
         ! ""
         = "a"
         = "ab"
         = "abc"
         = (repeat 100 \b))
  (check (se/nx 3)
         ! ""
         ! "a"
         ! "ab"
         = "abc"
         ! "abcd")
  (check (se/nx [2 4])
         ! ""
         ! "a"
         = "ab"
         = "abc"
         = "abcd"
         ! "abcde"))

(deftest fns
  (check (fn vowel? [c] ((set "aeiou") c))
         ! ""
         = "o"
         = "aa"
         = "eee"
         = "uoiea"
         ! "z"
         ! "abe")
  (check odd?
         ! []
         = [1 3 5]
         ! [1 3 4]
         ! [2]
         = [9 7 -1]))

(deftest misc
  (check se/vary
         = "abcdefghijklmnopqrstuvwxyz"
         = "abaca"
         ! "aabbcc")
  (check se/unique
         = "abcdefghijklmnopqrstuvwxyz"
         ! "abaca"
         ! "aabbcc")
  (check se/asc
         = [0 1 2 3 4 5 6]
         = [0 1 3 5 7 11]
         = [0 1 1 1 2 3]
         ! [0 1 1 0 2 3])
  (check (se/range 7)
         ! [0 1 2]
         ! [0 1 2 3 4]
         = [0 1 2 3 4 5 6]
         ! [0 1 3 5 7 11]
         ! [0 1 1 1 2 3]
         ! [0 1 1 0 2 3]))

(deftest logic
  (check (se/not (se/qty* odd?))
         = [2 4 6]
         ! [1]
         ! [])
  (check (se/or (se/qty* odd?) (se/qty* pos?))
         = [1 2 3]
         ! [-2]
         = [-1]
         ! [0])
  (check (se/and (se/qty* odd?) (se/qty* pos?))
         ! [0]
         ! [-1 -3 -5]
         = [1 3 5]))

;; stress expressions
(deftest stress
  (check (se/? \b)
         = ""
         = "b"
         ! "bb")
  (check (se/* (se/>> \a (se/? \b)))
         = ""
         = "a"
         ! "b"
         = "ab"
         = "abaab"
         = "aaaaba"
         = (repeat 100 \a)))

;; math expressions demo
(def ws
  "Arbitrary amount of whitespace."
  (se/* \space \tab))

(defn >ws>
  "Sequence of expressions interposed with whitespace."
  [& seqexes] (apply se/>> (interpose ws seqexes)))

(defn >ws>+
  "Ordered seqexes interposed with whitespace with just the first seqex
  required."
  [& seqexes]
   (let [[leader & following] (interpose ws seqexes)]
     (se/>> leader (se/* (apply se/>> following)))))

(def digits
  "At least one digit."
  (apply se/+ "0123456789"))

(def number
  "Integer or real number."
  (se/cap (se/>> (se/? \+ \-) digits (se/? (se/>> \. digits)))))

(declare add-expr)
(def atom-expr (se/| number (>ws> \( (delay add-expr) \) )))
(def pow-expr (>ws>+ atom-expr \^ atom-expr))
(def mul-expr (>ws>+ pow-expr (se/| \* \/) pow-expr))
(def add-expr (>ws>+ mul-expr (se/| \+ \-) mul-expr))
(def math-expr add-expr)

(def big-example "2^(2+2) * (-1/(0--1) - 12.3)")

(deftest math-demo
  (check ws
         = ""
         = " "
         = "    "
         ! "    x ")
  (check (>ws> \a \b \c)
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
  (check math-expr
         = "1"
         = "(23.0)"
         = "( ( 42 ) )"
         = "1++2"
         = "43--12"
         = big-example))

(deftest capturing
  (is (= (se/matches \1 "1") "1"))
  (is (= (se/matches \1 "2") nil))
  (is (= (se/matches (se/cap \1) "1") ["1" "1"]))
  (is (= (se/matches (se/cap \1) "2") nil))
  (is (= (se/matches math-expr big-example)
         [big-example "2" "2" "2" "-1" "0" "-1" "12.3"])))

(deftest ^:perf perf-math
  (crit/bench (se/valid? math-expr big-example)))

;; misc examples
(defn pr-test [se sym & inputs]
  (doseq [[indent input]
          (map list
               (cons
                 (name sym)
                 (repeat
                   (apply str (repeat (count (name sym)) " "))))
               inputs)]
    (println (str
               indent
               (if (se/valid? se input)
                 "  = \""
                 " != \"")
               input
               "\""))))


(def ws (apply se/c* " "))
(def word (apply se/c+ "abcdefghijklmnopqrstuvwxyz"))
(def end-punct (apply se/c1 ".?!"))
;; phrase    (seq word ws phrase)
;; sentence  (seq phrase end-punct)

(defn run []
  (pr-test ws 'ws "" " " "  " "asd")
  (pr-test word 'word "" "a" "is" "cat?" "@#!")
  (pr-test end-punct 'end-punct "" "." "?" "!" "!!" "?." "x"))

