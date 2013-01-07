(ns n01se.seqex.util
    (:require (clojure walk)))

; Supporting functions

(defn transpose
  "transpose columns into rows and rows into columns.
  ((1 2)      ((1 3 5)
   (3 4)  <->  (2 4 6))
   (5 6))"
  [matrix]
  (apply map list matrix))

;; thread functions... similar to test->

(defmacro ->when [x pred & body]
  `(let [x# ~x]
     (if ~pred
       (-> x# ~@body)
       x#)))

(defmacro ->when-not [x pred & body]
  `(let [x# ~x]
     (if ~pred
       x#
       (-> x# ~@body))))

; Delay Function: functions that do not evaluate their arguments until they
; are dereferenced. Implemented as pairs of functions, one that has an inlined
; clause which it uses to defer implementation.

; example 1
; def:
; (defn-delay foo [x] @x)
; use:
; (foo (do (pr "test") 3)) ; 3
; imp:
; (defn foo-123 [x] @x)
; (defmacro foo [x] `(foo-123 (delay ~x)))

; example 2
; def:
; (defn-delay bar [ x & ys ] (str @x (some (comp str? deref) ys)))
; use:
; (bar "hello" 1 2 "world" 3); "hello world"
; imp:
; (defn bar-123 [x & ys] (str @x (some (comp str? deref) ys)))
; (defmacro bar [x & ys] `(bar-123 (delay ~x) ~@(map #(list `delay %) ys)))

; start typing:
(defn const-ref [x]
  "Return a trivial implimentation of deref."
  (reify clojure.lang.IDeref (deref [_] x)))

(defn parse-defn2
  "Parse defn-like form with only one arity/body."
  [[name & more]]
  (let [[docstring args & body]
          (if (string? (first more))
            more
            (cons "" more))
        internal-args
          (if (and (not (empty? args)) (= '& (peek (pop args))))
            (conj (vec (drop-last 2 args)) (peek args)) ; remove the &.
            args)]
    [name docstring args internal-args body]))

(defmacro defn-delay2
  "Defines a function that, when called directly, will not evaluate its arguments."
  [& more]
  (let [[name txt args i-args body] (parse-defn2 more)
        i-name (gensym (str name "-"))
        delayed-args (for [arg i-args] ``(delay ~~arg))
        const-args (for [arg i-args] `(const-ref ~arg))]
    `(do
       (defn ~i-name ~txt ~i-args ~@body)
       (defn
         ~(with-meta name
            {:inline
             `(fn ~args
                `(~~(symbol (str (.name *ns*)) (str i-name))
                    ~~@delayed-args))})
         ~txt ~args
         (~i-name ~@const-args)))))

(defn parse-defn
  "Parse defn-like form with only one arity/body."
  [[name & more]]
  (let [[docstring args & body]
          (if (string? (first more))
            more
            (cons "" more))
        [req-args var-arg]
          (if (and (not (empty? args)) (= '& (peek (pop args))))
            [(drop-last 2 args) (peek args)]
            [args nil])]
    [name docstring args req-args var-arg body]))

(defmacro defn-delay
  "Defines a function that, when called directly, will not evaluate its arguments."
  [& more]
  (let [[name txt args req-args var-arg body] (parse-defn more)
        fn-name (gensym (str name "-"))
        delayed-args (for [arg req-args] ``(delay ~~arg))]
    `(do
       (defn ~fn-name ~txt ~args ~@body)
       (defn
         ~(with-meta name
            {:inline
             `(fn ~args
                `(~~(symbol (str (.name *ns*)) (str fn-name))
                    ~~@delayed-args
                    ~@(map #(do `(delay ~%)) ~var-arg)))})
         ~txt ~args
         (apply ~fn-name (for [arg# (concat ~(vec req-args) ~var-arg)]
                           (const-ref arg#)))))))

