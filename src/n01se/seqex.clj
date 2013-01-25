;clojure symbols
(ns n01se.seqex
  "Sequence Expressions. Library for describing sequences."
  (:require [n01se.seqex.util :refer [transpose ->when ->when-not]]
            [clojure.pprint :refer [pprint]])
  (:refer-clojure :exclude [and not or range]))

(alias 'clj 'clojure.core)

;; Verdicts
(def Invalid   2r00) ;; Not matching and don't continue.
(def Continue  2r01) ;; Not matching but continue.
(def Matching  2r10) ;; Matching but don't continue.
(def Satisfied 2r11) ;; Matching and continue.

(defn vbool [v] (if v Satisfied Invalid))

(defn invalid?   [v] (= v Invalid))
(defn continue?  [v] (= (bit-and v Continue) Continue))
(defn matching?  [v] (= (bit-and v Matching) Matching))
(defn satisfied? [v] (= v Satisfied))

(defprotocol SeqEx
  "Sequence expression protocol. Used to define things that implement seqexes."
  (-begin [_] ; return [state verdict]
    "Initial function that returns the beginning state and verdict.")
  (-continue [_ state token] ; return [state verdict matches]
    "Continue sequence by matching the current token against current state.
    Returns new state and verdict."))

; Sequence Expression Library.

; Simple cardnality expressions
(def n0
  (reify SeqEx
    (-begin [_] [Invalid Matching])
    (-continue [_ s t] [s s])))

(def n1
  (reify SeqEx
    (-begin [_] [Matching Continue])
    (-continue [_ s t] [Invalid s])))

(def n?
  (reify SeqEx
    (-begin [_] [Matching Satisfied])
    (-continue [_ s t] [Invalid s])))

(def n*
  (reify SeqEx
    (-begin [_] [Satisfied Satisfied])
    (-continue [_ s t] [Satisfied s])))

(def n+
  (reify SeqEx
    (-begin [_] [Satisfied Continue])
    (-continue [_ s t] [Satisfied s])))

(defrecord Cardnality [low high]
  SeqEx
  (-begin [_] (-continue _ 0 nil))
  (-continue [_ s t]
    [(inc s)
     (cond
       (< s  low)  Continue
       (nil? high) Satisfied
       (< s  high) Satisfied
       (= s  high) Matching
       (> s  high) Invalid)]))

(defn nx [x] (if (sequential? x)
               (condp = (count x)
                 1 (->Cardnality 0 (first x))
                 2 (apply ->Cardnality x))
               (->Cardnality x x)))

; value expressions
(defn literal-begin [v] [true Continue])
(defn literal-continue [v s t] [false (if (clj/and s (= v t)) Matching Invalid)])

(extend-protocol SeqEx

  clojure.lang.Fn
  (-begin [pred] [nil Satisfied])
  (-continue [pred s t] [s (vbool (pred t))])

  clojure.lang.Delay
  (-begin [d] (-begin @d))
  (-continue [d s t] (-continue @d s t))

  nil
  (-begin [value] (literal-begin value))
  (-continue [value once token] (literal-continue value once token))

  clojure.lang.Symbol
  (-begin [value] (literal-begin value))
  (-continue [value once token] (literal-continue value once token))

  clojure.lang.Keyword
  (-begin [value] (literal-begin value))
  (-continue [value once token] (literal-continue value once token))

  java.lang.Character
  (-begin [value] (literal-begin value))
  (-continue [value once token] (literal-continue value once token))

  java.lang.String
  (-begin [value] (literal-begin value))
  (-continue [value once token] (literal-continue value once token))

  java.lang.Double
  (-begin [value] (literal-begin value))
  (-continue [value once token] (literal-continue value once token))

  java.lang.Long
  (-begin [value] (literal-begin value))
  (-continue [value once token] (literal-continue value once token))

  clojure.lang.PersistentHashSet
  (-begin [value] (literal-begin value))
  (-continue [value once token] (literal-continue value once token))

  clojure.lang.ArraySeq
  (-begin [value] (literal-begin value))
  (-continue [value once token] (literal-continue value once token))

  clojure.lang.LazySeq
  (-begin [value] (literal-begin value))
  (-continue [value once token] (literal-continue value once token))

  clojure.lang.Cons
  (-begin [value] (literal-begin value))
  (-continue [value once token] (literal-continue value once token))

  clojure.lang.PersistentList
  (-begin [value] (literal-begin value))
  (-continue [value once token] (literal-continue value once token))

  clojure.lang.PersistentVector
  (-begin [value] (literal-begin value))
  (-continue [value once token] (literal-continue value once token)))

;; Some generic value comparison operations
(defn gt [x] #(pos? (compare % x)))
(defn ge [x] #(clj/not (neg? (compare % x))))
(defn eq [x] #(zero? (compare % x)))
(defn le [x] #(clj/not (pos? (compare % x))))
(defn lt [x] #(neg? (compare % x)))
(defn rng [low high] #(clj/not (clj/or (pos? (compare low %))
                                       (pos? (compare % high)))))

; Stateful token value expressions

; Unique value used by vary and asc for initial state.
(def unique-value (Object.))

(def vary "Sequences containing non-consecutive equal values."
  (reify SeqEx
    (-begin [_] (-continue _ nil unique-value))
    (-continue [_ s t] [t (vbool (not= s t))])))

(def asc "Sequences of values greater than or equal to previous values."
  (reify SeqEx
    (-begin [_] (-continue _ unique-value unique-value))
    (-continue [_ s t] [t (vbool (clj/or (= s unique-value) (<= s t)))])))

(defn- se-range "Sequences of incrementing numbers from 0 to n-1."
  [n]
  (reify SeqEx
    (-begin [_] [0 Continue])
    (-continue [_ s t]
      [(inc s)
       (if (= s t)
         (if (< s (dec n))
           Continue
           (if (= s (dec n))
             Matching
             Invalid))
         Invalid)])))

(def unique "Sequences with no repeating values."
  (reify SeqEx
    (-begin [_] [#{} Satisfied])
    (-continue [_ s t] [(conj s t) (vbool (clj/not (contains? s t)))])))

; Higher order expressions (these take expressions as arguments).
; Arguably, these are the only expressions that need to be macros.

(defn- se-not "Sequences where expression is always Invalid."
  [se]
  (reify SeqEx
    (-begin [_] (-begin se))
    (-continue [_ s t]
      (let [[s v] (-continue se s t)]
        [s (vbool (= v Invalid))]))))

(defn- combine-results
  "Combine state and verdicts using the given bit operation."
  [bit-op results]
  (let [[states verdicts] (transpose results)]
    [states (apply bit-op verdicts)]))

(defn- parallel
  "Sequences constrained by multiple expressions combined with bit-op."
  [bit-op & ses]
  (reify SeqEx
    (-begin [_]
      (combine-results bit-op (map #(-begin %1) ses)))
    (-continue [_ s t]
      (combine-results bit-op (map #(-continue %1 %2 %3) ses s (repeat t))))))

(defn- se-and "Sequences in which all expressions are true."
  [& ses]
  (apply parallel bit-and ses))

(defn- se-or "Sequences in which any expressions is true."
  [& ses]
  (apply parallel bit-or ses))

(defn apply-fn "Sequences where expression is applied to (f value)."
  [f se]
  (reify SeqEx
    (-begin [_] (-begin se))
    (-continue [_ s t] (-continue se s (f t)))))

; Serial expression: compose muliple seqexes such that they are applied to the
; sequence one at a time and limited by a higher order seqex on the indicies of
; those seqexes. Pretty much the ultimate power in the universe :-).

; Example expression:
;   (serial unique vowels numbers symbols)

; Input strings:
;   ["23ei?!", "@a1"]

; Key:
;   path: [ssv ise isv] or [[ss sv] ise [is iv]]
;   ssv: superior state + verdict
;    ss: superior state
;    sv: superior verdict
;   ise: inferior seqex
;   isv: inferior state + verdict
;    is: inferior state
;    iv: inferior verdict

(defn pr-paths "Useful debugging tool."
  [paths msg]
  (println msg)
  (doseq [p paths] (println "  path:" p))
  paths)

(defn- root-path
  "Define the initial path."
  [superior-se]
  [[(-begin superior-se) n0 (-begin n0)]])

(defn- age-paths
  "Apply current token to paths"
  [paths token]
  (->> paths
    ;; keep only continuing paths
    (filter (fn [[ssv ise [is iv]]] (continue? iv)))
    ;; apply token to each continuing path
    (map (fn [[ssv ise [is iv]]] [ssv ise (-continue ise is token)]))
    ;; remove any now invalid paths
    (remove (fn [[ssv ise [is iv]]] (invalid? iv)))))

;; TODO I could use an ordered set data structure in branch-paths.
(defn- branch-paths
  "Check if each path has child paths and create those paths as needed."
  [old-paths superior-se inferior-ses]
  (letfn [(branch [new-paths ss]
            (->> inferior-ses
              ;; define first (superior) half of path
              (map-indexed (fn [idx ise] [(-continue superior-se ss idx) ise]))
              ;; filter Invalid paths (= sv Invalid)
              (remove (fn [[[ss sv] ise]] (invalid? sv)))
              ;; define second (inferior) half of path
              (map (fn [[ssv ise :as path]]
                     (conj path (-begin ise))))
              (reduce inspect new-paths)))

          (inspect [new-paths [[ss sv] ise [is iv] :as path]]
            (-> new-paths
              (->when-not (contains? new-paths path)
                          (conj path)
                          (->when (clj/and (continue? sv) (matching? iv))
                                  (branch ss)))))]
    (reduce inspect [] old-paths)))

(defn- judge-paths
  "Combine the verdicts of each path into a final verdict. Return a pair of
  paths plus final verdict."
  [paths]
  [paths
   (apply bit-or Invalid Invalid
          (for [[[ss sv] ise [is iv]] paths]
            (if (clj/or (continue? sv) (continue? iv))
              (if (clj/and (matching? sv) (matching? iv))
                Satisfied
                Continue)
              (if (clj/and (matching? sv) (matching? iv))
                Matching
                Invalid))))])

(defrecord Serial [superior-se inferior-ses]
  SeqEx
  (-begin [_]
    (-> (root-path superior-se)
      (branch-paths superior-se inferior-ses)
      judge-paths))

  (-continue [_ paths token]
    (-> (age-paths paths token)
      (branch-paths superior-se inferior-ses)
      judge-paths)))

(defn c1 [& seqexes] (->Serial n1 seqexes))
(defn c? [& seqexes] (->Serial n? seqexes))
(defn c* [& seqexes] (->Serial n* seqexes))
(defn c+ [& seqexes] (->Serial n+ seqexes))
(defn cx [x & seqexes] (->Serial (nx x) seqexes))

(defn s1 [& seqexes] (->Serial (se-range (count seqexes)) seqexes))
(defn s? [& seqexes] (c? (apply s1 seqexes)))
(defn s* [& seqexes] (c* (apply s1 seqexes)))
(defn s+ [& seqexes] (c+ (apply s1 seqexes)))
(defn sx [x & seqexes] (cx x (apply s1 seqexes)))

(defn matches
  "Returns the tokens if seqex matches or nil otherwise."
  [seqex tokens]
  (loop [[state verdict] (-begin seqex)
         [token & more :as tokens] tokens]
    (if (empty? tokens)
      (matching? verdict)
      (if (continue? verdict)
        (recur (-continue seqex state token) more)
        ; The previous verdict indicated no continue so this
        ; token stream can never match.
        false))))

;; API for using seqexes
(defn se-find
 "Find and return first occurance of seqex in tokens."
  [seqex tokens]
  )

(defn se-seq [seqex tokens]
  "Find and return a sequence of all non-overlapping occurances of seqex.")


;; Rename all the se-* expressions that overwrite built in names. Do this near
;; the bottom of the file so as to reduce the chance of accidentally using
;; those expressions during implementation.
(def and se-and)
(def not se-not)
(def or se-or)
(def range se-range)

