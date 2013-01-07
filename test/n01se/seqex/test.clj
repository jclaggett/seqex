(ns n01se.seqex.test
  (:require [n01se.seqex :as se
             :refer [  n? n* n+ nx nm
                     o o? o* o+ ox onm
                     u u? u* u+ ux unm]])
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

(defn check [se & test-pairs]
  (doseq [[op input] (partition 2 test-pairs)]
    (is (op (validate se input) true) (str ({= '= ! '!} op) " " input))))

(deftest length
  (check nil
         = ""
         ! "a")
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
  (check (se/inc 0)
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
  (check (o? \b)
         = ""
         = "b"
         ! "bb")
  (check (o* \a (o? \b))
         = ""
         = "a"
         ! "b"
         = "ab"
         = "abaab"
         = "aaaaba"
         = (repeat 1000 \a)))

;; math expressions demo
(def ws (n* se/or \space \tab))
(defn seq-ws [& ses] (apply se/seq (interpose ws ses)))
(defn first-or-all [[se :as ses]] (se/or se (apply se/order ses)))
(def digits (apply n+ se/or (seq "0123456789")))
(def number (first-or-all digits \. digits))
(def expr (se/or nil
                   number
                   (o-ws \( (delay expr) \))))

(deftest math-demo
  (check digits
         ! ""
         = "1"
         = "12"
         = "45223423")
  (check expr
         = "1"
         = "(23)"
         = "( ( 42 ) )"))

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


(def ws (apply u* " "))
(def word (apply u+ "abcdefghijklmnopqrstuvwxyz"))
(def end-punct (apply u ".?!"))
;; phrase    (seq word ws phrase)
;; sentence  (seq phrase end-punct)

(defn run []
  (pr-test ws 'ws "" " " "  " "asd")
  (pr-test word 'word "" "a" "is" "cat?" "@#!")
  (pr-test end-punct 'end-punct "" "." "?" "!" "!!" "?." "x"))

