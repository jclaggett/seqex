(ns n01se.seqex.test
  (:require [n01se.seqex :as se
             :refer [n1 n? n* n+ nx nm]])
  (:use [clojure.test]))

; Unit testing.
(defn validate
  "Constrain a sequence of tokens."
  [se tokens]
  (loop [[state verdict] (se/-init se)
         [token & more :as tokens] tokens]
    (if (empty? tokens)
      (se/matching? verdict)
      (if (se/continue? verdict)
        (recur (se/-match se state token) more)
        ; The previous verdict indicated no continue so this
        ; token stream can never match.
        false))))

(def ! not=) ;; yes, I am that lazy :-)

(defmacro check
  [se & test-pairs]
  (let [se-val (gensym "se")]
  `(let [~se-val ~se]
     ~@(for [[op input] (partition 2 test-pairs)
             :let [is-str (str se " " op " \"" input "\"")]]
         `(is (~op (validate ~se-val ~input) true) ~is-str)))))

(deftest length
  (check nil
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
  (check (nm 2 4)
         ! ""
         ! "a"
         = "ab"
         = "abc"
         = "abcd"
         ! "abcde"))

(deftest value
  (check (set "aeiou")
         ! ""
         = "a"
         = "e"
         = "i"
         = "o"
         = "u"
         ! "eee"
         ! "uoiea"
         ! "z"
         ! "abe")
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

(deftest stateful-value
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
  (check [n? \b]
         = ""
         = "b"
         ! "bb")
  (check [n* (se/seq \a [n? \b])]
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
               (if (validate se input)
                 "  = \""
                 " != \"")
               input
               "\""))))


(def ws (cons n* " "))
(def word (cons n+ "abcdefghijklmnopqrstuvwxyz"))
(def end-punct (cons n1 ".?!"))
;; phrase    (seq word ws phrase)
;; sentence  (seq phrase end-punct)

(defn run []
  (pr-test ws 'ws "" " " "  " "asd")
  (pr-test word 'word "" "a" "is" "cat?" "@#!")
  (pr-test end-punct 'end-punct "" "." "?" "!" "!!" "?." "x"))

