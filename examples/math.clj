(ns n01se.seqex.examples.math
  (:require [n01se.seqex :as se]))

;; math expressions demo
;; Match basic standard infix notation and capture the results as a list.
(def ws
  "Arbitrary amount of whitespace."
  (se/qty* \space \tab))

(defn ord-ws
  "Ordered seqexes interposed with whitespace."
  [& seqexes] (apply se/ord (interpose ws seqexes)))

(defn first-rest*
  "Ordered seqexes interposed with whitespace where the first seqex is required
  and all following seqexes are repeated zero or more times."
  [& seqexes]
   (let [[leader & following] (interpose ws seqexes)]
     (se/ord leader (se/qty* (apply se/ord following)))))

(def digits
  "One or more digits."
  (apply se/qty+ "0123456789"))

(def number
  "Real number. Captured and converted to a double."
  (se/cap (se/ord (se/opt \+ \-)
                  (se/alt digits
                          (se/ord \. digits)
                          (se/ord digits \.)
                          (se/ord digits \. digits))
                  (se/opt (se/ord (se/alt \e \E)
                                  (se/opt \+ \-)
                                  digits)))
          #(Double/parseDouble (apply str %))))

(declare add-expr) ;; allows paren-expr to refer to add-expr
(def paren-expr
  "Parens wrapping an expression. Recaptured in a list."
  (se/recap (ord-ws \( (delay add-expr) \))
            list))
(defn op
  "Operators. Captured and converted into symbols."
  [& op-chars]
  (se/cap-one (apply se/alt op-chars)
              (comp symbol str)))
(def atom-expr (se/alt number paren-expr))
(def pow-expr (first-rest* atom-expr (se/cap (se/ord \p \o \w)
                                         (comp symbol (partial apply str)))
                atom-expr))
(def mul-expr (first-rest* pow-expr (op \* \/) pow-expr))
(def add-expr (first-rest* mul-expr (op \+ \-) mul-expr))
(def math-expr add-expr)

(def big-example "2 pow (.2 + +2e+3) * (-1.E-1/(0--1) - 12.3)")

(assert (= true
           (se/valid? math-expr big-example)))
(assert (= '(2.0 pow (0.2 + 2000.0) * (-0.1 / (0.0 - -1.0) - 12.3))
           (se/model math-expr big-example)))

