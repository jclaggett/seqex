(ns n01se.seqex.examples.math
  (:require [n01se.seqex :as se]))

;; math expressions demo
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
  "one or more digits."
  (apply se/qty+ "0123456789"))

(def number
  "real number. (captured)"
  (se/recap #(Double/parseDouble (apply str %))
            (se/cap-tokens
              (se/ord (se/opt \+ \-)
                      (se/alt digits
                              (se/ord \. digits)
                              (se/ord digits \.)
                              (se/ord digits \. digits))
                      (se/opt (se/ord (se/alt \e \E)
                                      (se/opt \+ \-)
                                      digits))))))

(declare add-expr)
(def atom-expr (se/alt number (ord-ws \( (delay add-expr) \) )))
(def pow-expr (first-rest* atom-expr \^ atom-expr))
(def mul-expr (first-rest* pow-expr (se/alt \* \/) pow-expr))
(def add-expr (first-rest* mul-expr (se/alt \+ \-) mul-expr))
(def math-expr add-expr)

(def big-example "2^(2+2) * (-1/(0--1) - 12.3)")

(se/valid? math-expr big-example) ;=> true
(se/model math-expr big-example) ;=> (2.0 2.0 2.0 -1.0 0.0 -1.0 12.3)
