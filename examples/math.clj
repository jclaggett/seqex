(ns n01se.seqex.examples.math
  (:require [n01se.seqex :as se]))

;; Math Expressions Demo
;; Match basic standard infix notation and capture the results as a Clojure
;; expression that evaluates to the answer.

(def ws
  "Arbitrary amount of whitespace."
  (se/rep* \space \tab))

(def digits
  "One or more digits."
  (apply se/rep+ "0123456789"))

(def number
  "Real number. Captured as a double."
  (se/cap (se/cat (se/opt \+ \-)
                  (se/alt digits
                          (se/cat \. digits)
                          (se/cat digits \.)
                          (se/cat digits \. digits))
                  (se/opt (se/cat (se/alt \e \E)
                                  (se/opt \+ \-)
                                  digits)))
          #(Double/parseDouble (apply str %))))

(defn op
  "Operator. Captured as function."
  ([literal] (op literal (resolve (symbol (str literal)))))
  ([literal, func] (se/cap literal (constantly func))))

(defn bin-expr
  "Left to right associative, infix binary expressions. Captured as
  S-expressions."
  [expr & ops]
  (se/recap (se/cat expr ws
                (se/rep* (se/recap (se/cat (apply se/alt ops) ws expr)
                                   ;; wrap the op and expr in a list
                                   list)))
      ;; Take leading expr and following '(op expr) pairs and nest them.
      (fn [& models]
        (reduce #(cons (first %2)
                       (cons %1 (rest %2)))
                models))))

(declare add-expr) ;; allows atom-expr to refer to add-expr
(def atom-expr (se/alt number (se/cat \( ws (delay add-expr) ws \))))
(def pow-expr (bin-expr atom-expr (op \^ #(Math/pow %1 %2))))
(def mul-expr (bin-expr pow-expr (op \*) (op \/) (op \%)))
(def add-expr (bin-expr mul-expr (op \+) (op \-)))
(def math-expr add-expr)

(def big-example "2^(.2 + +2e+0) * (-1.E-1/(0--1) - 12.3)")
(def big-answer (* (Math/pow 2 (+ 0.2 2e+0)) (- (/ -1.0E-1 (- 0 -1)) 12.3)))

(assert (= true
           (se/valid? math-expr big-example)))

(assert (= big-answer
           (-> (se/parse math-expr big-example)
             first eval)))

