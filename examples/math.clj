(ns n01se.seqex.examples.math
  (:require [n01se.seqex :as se]))

;; Math Expressions Demo
;; Match basic standard infix notation and capture the results as a Clojure
;; expression that evaluates to the answer.

(def ws
  "Arbitrary amount of whitespace."
  (se/qty* \space \tab))

(def digits
  "One or more digits."
  (apply se/qty+ "0123456789"))

(def number
  "Real number. Captured as a double."
  (se/cap (se/ord (se/opt \+ \-)
                  (se/alt digits
                          (se/ord \. digits)
                          (se/ord digits \.)
                          (se/ord digits \. digits))
                  (se/opt (se/ord (se/alt \e \E)
                                  (se/opt \+ \-)
                                  digits)))
          #(Double/parseDouble (apply str %))))

(defn op
  "Operator. Captured as function."
  ([literal] (op literal (resolve (symbol (str literal)))))
  ([literal, func] (se/cap-one literal #(do %1 func))))

(defn bin-expr
  "Left to right associative, infix binary expressions. Captured as
  S-expressions."
  [expr & ops]
  (se/recap (se/ord expr ws
                (se/qty* (se/recap (se/ord (apply se/alt ops) ws expr)
                             ;; wrap the op and expr in a list
                             list)))
      ;; Take leading expr and following '(op expr) pairs and nest them.
      (fn [models]
        (list (reduce #(cons (first %2)
                             (cons %1 (rest %2)))
                      models)))))

(declare add-expr) ;; allows atom-expr to refer to add-expr
(def atom-expr (se/alt number (se/ord \( ws (delay add-expr) ws \))))
(def pow-expr (bin-expr atom-expr (op \^ #(Math/pow %1 %2))))
(def mul-expr (bin-expr pow-expr (op \*) (op \/) (op \%)))
(def add-expr (bin-expr mul-expr (op \+) (op \-)))
(def math-expr add-expr)

(def big-example "2^(.2 + +2e+0) * (-1.E-1/(0--1) - 12.3)")

(assert (= true
           (se/valid? math-expr big-example)))

(assert (= (* (Math/pow 2 (+ 0.2 2e+0)) (- (/ -1.0E-1 (- 0 -1)) 12.3))
           (-> (se/models math-expr big-example)
               first eval)))

