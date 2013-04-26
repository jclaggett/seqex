;clojure symbols
(ns n01se.seqex
  "Sequence Expressions. Library for describing sequences."
  (:use [n01se.seqex.util :only [transpose ->when ->when-not]]
        [clojure.pprint :only [pprint]])
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
    Returns new state and verdict.")
  (-end [_ state] ; return models
    "Finish by calculating zero or more models from state."))

(set! *warn-on-reflection* true)

; Sequence Expression Library.

; Simple cardnality expressions
(def n0
  (reify SeqEx
    (-begin [_] [Invalid Matching])
    (-continue [_ s t] [s s])
    (-end [_ s] nil)))

(def n1
  (reify SeqEx
    (-begin [_] [Matching Continue])
    (-continue [_ s t] [Invalid s])
    (-end [_ s] nil)))

(def n?
  (reify SeqEx
    (-begin [_] [Matching Satisfied])
    (-continue [_ s t] [Invalid s])
    (-end [_ s] nil)))

(def n*
  (reify SeqEx
    (-begin [_] [Satisfied Satisfied])
    (-continue [_ s t] [Satisfied s])
    (-end [_ s] nil)))

(def n+
  (reify SeqEx
    (-begin [_] [Satisfied Continue])
    (-continue [_ s t] [Satisfied s])
    (-end [_ s] nil)))

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
       (> s  high) Invalid)])
  (-end [_ s] nil))

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
  (-end [_ s] nil)

  clojure.lang.Delay
  (-begin [d] (-begin @d))
  (-continue [d s t] (-continue @d s t))
  (-end [d s] (-end @d s))

  nil
  (-begin [value] (literal-begin value))
  (-continue [value once token] (literal-continue value once token))
  (-end [_ s] nil)

  clojure.lang.Symbol
  (-begin [value] (literal-begin value))
  (-continue [value once token] (literal-continue value once token))
  (-end [_ s] nil)

  clojure.lang.Keyword
  (-begin [value] (literal-begin value))
  (-continue [value once token] (literal-continue value once token))
  (-end [_ s] nil)

  java.lang.Character
  (-begin [value] (literal-begin value))
  (-continue [value once token] (literal-continue value once token))
  (-end [_ s] nil)

  java.lang.String
  (-begin [value] (literal-begin value))
  (-continue [value once token] (literal-continue value once token))
  (-end [_ s] nil)

  java.lang.Double
  (-begin [value] (literal-begin value))
  (-continue [value once token] (literal-continue value once token))
  (-end [_ s] nil)

  java.lang.Long
  (-begin [value] (literal-begin value))
  (-continue [value once token] (literal-continue value once token))
  (-end [_ s] nil)

  clojure.lang.PersistentHashSet
  (-begin [value] (literal-begin value))
  (-continue [value once token] (literal-continue value once token))
  (-end [_ s] nil)

  clojure.lang.ArraySeq
  (-begin [value] (literal-begin value))
  (-continue [value once token] (literal-continue value once token))
  (-end [_ s] nil)

  clojure.lang.LazySeq
  (-begin [value] (literal-begin value))
  (-continue [value once token] (literal-continue value once token))
  (-end [_ s] nil)

  clojure.lang.Cons
  (-begin [value] (literal-begin value))
  (-continue [value once token] (literal-continue value once token))
  (-end [_ s] nil)

  clojure.lang.PersistentList
  (-begin [value] (literal-begin value))
  (-continue [value once token] (literal-continue value once token))
  (-end [_ s] nil)

  clojure.lang.PersistentVector
  (-begin [value] (literal-begin value))
  (-continue [value once token] (literal-continue value once token))
  (-end [_ s] nil))

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
    (-continue [_ s t] [t (vbool (not= s t))])
    (-end [_ s] nil)))

(def asc "Sequences of values greater than or equal to previous values."
  (reify SeqEx
    (-begin [_] (-continue _ unique-value unique-value))
    (-continue [_ s t] [t (vbool (clj/or (= s unique-value) (<= s t)))])
    (-end [_ s] nil)))

(defn- se-range "Sequences of incrementing numbers from 0 to n-1."
  [n]
  (let [n-1 (dec n)]
    (reify SeqEx
      (-begin [_] [0 Continue])
      (-continue [_ s t]
        [(inc s)
         (if (= s t)
           (if (< s n-1)
             Continue
             (if (= s n-1)
               Matching
               Invalid))
           Invalid)])
      (-end [_ s] nil))))

(def unique "Sequences with no repeating values."
  (reify SeqEx
    (-begin [_] [#{} Satisfied])
    (-continue [_ s t] [(conj s t) (vbool (clj/not (contains? s t)))])
    (-end [_ s] nil)))

; Higher order expressions (these take expressions as arguments).
; Arguably, these are the only expressions that need to be macros.

(defn- se-not "Sequences where expression is always Invalid."
  [se]
  (reify SeqEx
    (-begin [_] (-begin se))
    (-continue [_ s t]
      (let [[s v] (-continue se s t)]
        [s (vbool (= v Invalid))]))
    (-end [_ s] nil)))

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
      (combine-results bit-op (map #(-continue %1 %2 %3) ses s (repeat t))))
    (-end [_ s] nil)))

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
    (-continue [_ s t] (-continue se s (f t)))
    (-end [_ s] (-end se s))))

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
  [[(-begin superior-se) n0 (-begin n0) nil]])

(defn- age-paths
  "Apply current token to paths"
  [paths token]
  (->> paths
    ;; keep only continuing paths
    (filter (fn [[ssv ise [is iv]]] (continue? iv)))
    ;; apply token to each continuing path
    (map (fn [[ssv ise [is iv] models]] [ssv ise (-continue ise is token) models]))
    ;; remove any now invalid paths
    (remove (fn [[ssv ise [is iv]]] (invalid? iv)))))

;; TODO I could use an ordered set data structure in branch-paths.
(defn- branch-paths
  "Check if each path has child paths and create those paths as needed."
  [old-paths superior-se inferior-ses]
  (letfn [(branch [new-paths [[old-ss old-sv] old-ise [old-is old-iv] models]]
            (->> inferior-ses
              ;; define first (superior) half of path
              (map-indexed (fn [idx ise] [(-continue superior-se old-ss idx)
                                          ise]))
              ;; filter Invalid paths (= sv Invalid)
              (remove (fn [[[ss sv] ise]] (invalid? sv)))
              ;; define second (inferior) half of path
              (map (fn [[ssv ise :as path]]
                     (-> path
                         (conj (-begin ise))
                         (conj (concat models (-end old-ise old-is))))))
              (reduce inspect new-paths)))

          (inspect [new-paths [[ss sv] ise [is iv] :as path]]
            (-> new-paths
              (->when-not (contains? new-paths path)
                          (conj path)
                          (->when (clj/and (continue? sv) (matching? iv))
                                  (branch path)))))]
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
      judge-paths))
  (-end [_ [[ssv ise [is iv] models :as path] :as paths]]
    (when-not (empty? paths)
      (concat models (-end ise is)))))

;; choosing expressions
(defn c1 [& seqexes] (->Serial n1 seqexes))
(defn c? [& seqexes] (->Serial n? seqexes))
(defn c* [& seqexes] (->Serial n* seqexes))
(defn c+ [& seqexes] (->Serial n+ seqexes))
(defn cx [x & seqexes] (->Serial (nx x) seqexes))

;; sequence expressions
(defn s1 [& seqexes] (->Serial (se-range (count seqexes)) seqexes))
(defn s? [& seqexes] (c? (apply s1 seqexes)))
(defn s* [& seqexes] (c* (apply s1 seqexes)))
(defn s+ [& seqexes] (c+ (apply s1 seqexes)))
(defn sx [x & seqexes] (cx x (apply s1 seqexes)))

;; Capturing seqexes
(defn cap
  [seqex]
  (reify SeqEx
    (-begin [_]
      (let [[s v] (-begin seqex)]
        [[s []] v]))
    (-continue [_ [s capts] t]
      (let [[s v] (-continue seqex s t)]
        [[s (conj capts t)] v]))
    (-end [_ [s capts]]
      (cons capts (-end seqex s)))))

;; API for using seqexes
(defn matches
  "Returns tokens if tokens match seqex or nil otherwise. If additional
  capturing occurs, returns sequence of matches with behavior similar to
  re-groups."
  [seqex orig-tokens]
  (let [group-seqex (cap seqex)]
    (loop [[state verdict] (-begin group-seqex)
           [token & more :as tokens] orig-tokens]
      (if (empty? tokens)
        (if (matching? verdict)
          (let [groups (-end group-seqex state)
                groups (if (string? orig-tokens)
                         (map (partial apply str) groups)
                         groups)
                groups (if (= 1 (count groups))
                         (first groups)
                         groups)]
            groups)
          nil)
        (if (continue? verdict)
          (recur (-continue group-seqex state token) more)
          ; The previous verdict indicated no continue so this
          ; token stream can never match.
          nil)))))

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

