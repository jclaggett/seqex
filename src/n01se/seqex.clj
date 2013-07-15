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
  "Sequence expression protocol. Used by types that implement seqexes."
  (-begin [_] ; return [state verdict]
    "Initial function that returns the beginning state and verdict.")
  (-continue [_ state token] ; return [state verdict matches]
    "Continue sequence by examining the current token using current state.
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

; literal expressions
(doseq [T [nil clojure.lang.ArraySeq clojure.lang.Keyword
           clojure.lang.LazySeq clojure.lang.Cons
           clojure.lang.PersistentHashSet clojure.lang.PersistentList
           clojure.lang.PersistentVector clojure.lang.Symbol
           java.lang.Character java.lang.Double java.lang.Long
           java.lang.String]]
  (extend T SeqEx
          {:-begin (constantly [true Continue])
           :-continue (fn [literal first-time? token]
                        [false (if (clj/and first-time? (= literal token))
                                 Matching
                                 Invalid)])
           :-end (constantly nil)}))

; functions and delay refs
(extend-protocol SeqEx
  ;; Functions are treated as predicates on a single token.
  clojure.lang.Fn
  (-begin [_] [true Continue])
  (-continue [pred first-time? token]
    [false (if (clj/and first-time? (pred token))
             Matching
             Invalid)])
  (-end [_ _] nil)

  ;; Delays are assumed to hold a seqex to be used.
  clojure.lang.Delay
  (-begin [d] (-begin @d))
  (-continue [d s t] (-continue @d s t))
  (-end [d s] (-end @d s)))

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

(defn permute
  "Sequence containing any permuation of elems."
  [elems]
  (let [s (set elems)]
    (reify SeqEx
      (-begin [_] [s (if (empty? s) Matching Continue)])
      (-continue [_ s t] (if (contains? s t)
                           (let [s (disj s t)]
                             [s (if (empty? s) Matching Continue)])
                           [s Invalid]))
      (-end [_ s] nil))))

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
  [bit-op & seqexes]
  (reify SeqEx
    (-begin [_]
      (combine-results bit-op (map #(-begin %1) seqexes)))
    (-continue [_ states token]
      (combine-results bit-op (map #(-continue %1 %2 token) seqexes states)))
    (-end [_ states] (mapcat #(-end %1 %2) seqexes states))))

(defn- se-and "Sequences in which all expressions are true."
  [& seqexes]
  (apply parallel bit-and seqexes))

(defn- se-or "Sequences in which any expressions is true."
  [& seqexes]
  (apply parallel bit-or seqexes))

(defn apply-fn "Sequences where expression is applied to (f value)."
  [f seqex]
  (reify SeqEx
    (-begin [_] (-begin seqex))
    (-continue [_ s t] (-continue seqex s (f t)))
    (-end [_ s] (-end seqex s))))

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

;; New API

(defn ord "All seqexes in order." [& seqexes]
  (->Serial (se-range (count seqexes)) seqexes))
(defn alt "Alternate between seqexes (pick any one)." [& seqexes]
  (->Serial n1 seqexes))
(defn opt "Optionally alternate between seqexes." [& seqexes]
  (->Serial n? seqexes))
(defn qty+ "One or more seqexes (in any order)." [& seqexes]
  (->Serial n+ seqexes))
(defn qty* "Zero or more seqexes (in any order)." [& seqexes]
  (->Serial n* seqexes))
(defn qty
  "Repeat seqexes x times. If x is a single number, then repeat exactly that
  many times. If x is a sequence of two numbers then repeat between the first
  and second number of times. Finally, if x is a sequence of 1 number, repeat
  between 0 and that number of times."
  [x & seqexes]
  (->Serial (nx x) seqexes))

(defn all "All seqexes in any order."
  [& seqexes]
  (->Serial (permute (se-range (count seqexes))) seqexes))

;; Old API

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
  "Capture all non-invalid tokens examined by seqex. Return a vector of tokens
  unless finalize is specified and instead pass the token vector to finalize and
  use its return value instead. The resulting 'model' is prepended to any
  sub-models returned by seqex."
  [seqex & [finalize]]
  (let [finalize (clj/or finalize identity)]
    (reify SeqEx
      (-begin [_]
        (let [[state verdict] (-begin seqex)]
          [[state []] verdict]))
      (-continue [_ [state tokens] token]
        (let [[state verdict] (-continue seqex state token)]
          [[state (if (invalid? verdict)
                    tokens
                    (conj tokens token))]
           verdict]))
      (-end [_ [state tokens]]
        (cons (finalize tokens) (-end seqex state))))))

(defn recap
  "Apply finalize to all returned models by seqex and treat its result as a
  sequence of new models."
  [seqex finalize]
  (reify SeqEx
    (-begin [_] (-begin seqex))
    (-continue [_ state token] (-continue seqex state token))
    (-end [_ state] (finalize (-end seqex state)))))

;; API for using seqexes
(defn exec
  "Apply a seqex to a series of tokens. Returns results of (-end seqex) and a
  verdict of Matching or Invalid."
  [seqex tokens]
  (loop [[state verdict] (-begin seqex)
         [token & more :as ts] tokens]
    (if (empty? ts)
      (if (matching? verdict)
        [(-end seqex state) Matching]
        [nil Invalid])

      (if (continue? verdict)
        (recur (-continue seqex state token) more)
        [nil Invalid]))))

(defn valid?
  "Returns true when tokens are a valid input for seqex."
  [seqex tokens]
  (matching? (second (exec seqex tokens))))

(defn model
  "Executes seqex against tokens returning any captured model."
  [seqex tokens]
  (first (exec seqex tokens)))

(defn subex
  "Matches a single sequential token, matching seqex on its contents.
  This is a sort of 'descend' operation for matching nested data."
  [seqex]
  (reify SeqEx
    (-begin [_] [nil Continue])
    (-continue [_ _ token]
      (if (coll? token)
        (exec seqex (clj/seq token))
        [nil Invalid]))
    (-end [_ result]
      (when-not (nil? result)
        (list result)))))

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
