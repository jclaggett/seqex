(ns n01se.seqex.test
  (:require [n01se.seqex :as se
             :refer [n0 n1 n? n* n+ nx
                        s1 s? s* s+ sx
                        c1 c? c* c+ cx]]
            [criterium.core :as crit])
  (:use [clojure.test]))

; Unit testing.

(def ! not=) ;; yes, I am that lazy :-)

(defmacro check
  [se & test-pairs]
  (let [se-val (gensym "se")]
  `(let [~se-val ~se]
     ~@(for [[op input] (partition 2 test-pairs)
             :let [is-str (str se " " op " \"" input "\"")]]
         `(is (~op (se/matches ~se-val ~input) true) ~is-str)))))

(deftest length
  (check n0
         = ""
         ! "a")
  (check n1
         ! ""
         = "a"
         ! "ab"
         ! "abc")
  (check n?
         = ""
         = "a"
         ! "ab"
         ! "abc")
  (check n*
         = ""
         = "a"
         = "abc"
         = (repeat 100 \b))
  (check n+
         ! ""
         = "a"
         = "ab"
         = "abc"
         = (repeat 100 \b))
  (check (nx 3)
         ! ""
         ! "a"
         ! "ab"
         = "abc"
         ! "abcd")
  (check (nx [2 4])
         ! ""
         ! "a"
         = "ab"
         = "abc"
         = "abcd"
         ! "abcde"))

(deftest fns
  (check (fn vowel? [c] ((set "aeiou") c))
         = ""
         = "o"
         = "aa"
         = "eee"
         = "uoiea"
         ! "z"
         ! "abe")
  (check odd?
         = []
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
  (check (se/not odd?)
         = [2 4 6]
         ! [1]
         = [])
  (check (se/or odd? pos?)
         = [1 2 3]
         ! [-2]
         = [-1]
         ! [0])
  (check (se/and odd? pos?)
         ! [0]
         ! [-1 -3 -5]
         = [1 3 5]))

;; stress expressions
(deftest stress
  (check (c? \b)
         = ""
         = "b"
         ! "bb")
  (check (s* \a (c? \b))
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

(deftest ^:perf perf-math
  (crit/bench (se/matches math-ex "2^(2+2) * (-1/(0--1) - 12.3)")))

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
               (if (se/matches se input)
                 "  = \""
                 " != \"")
               input
               "\""))))


(def ws (apply c* " "))
(def word (apply c+ "abcdefghijklmnopqrstuvwxyz"))
(def end-punct (apply c1 ".?!"))
;; phrase    (seq word ws phrase)
;; sentence  (seq phrase end-punct)

(defn run []
  (pr-test ws 'ws "" " " "  " "asd")
  (pr-test word 'word "" "a" "is" "cat?" "@#!")
  (pr-test end-punct 'end-punct "" "." "?" "!" "!!" "?." "x"))

