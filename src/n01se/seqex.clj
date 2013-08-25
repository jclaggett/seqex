;clojure symbols
(ns n01se.seqex
  "Sequence Expressions. Library for describing sequences."
  (:use [n01se.seqex.util :only [transpose ->when ->when-not]]
        [clojure.pprint :only [pprint]]
        [clojure.string :as str :only []])
  (:refer-clojure :exclude [and not or range defmacro]))

(alias 'clj 'clojure.core)

;; Verdicts
(def ^:const Failed  2r00) ;; Not matching and don't continue.
(def ^:const Failing 2r01) ;; Not matching but continue.
(def ^:const Passed  2r10) ;; Matching but don't continue.
(def ^:const Passing 2r11) ;; Matching and continue.

(def ^:const Continue Failing)
(def ^:const Matching Passed)

(defn failed?  [v] (= v Failed))
(defn failing? [v] (= v Failing))
(defn passed?  [v] (= v Passed))
(defn passing? [v] (= v Passing))
(defn continue?  [v] (= (bit-and v Continue) Continue))
(defn matching?  [v] (= (bit-and v Matching) Matching))

(defn vbool [v] (if v Passing Failed))

(defprotocol SeqEx
  "Sequence expression protocol. Used by types that implement seqexes."
  (-begin [_] ; return [state verdict]
    "Initial function that returns the beginning state and verdict.")
  (-continue [_ state token] ; return [state verdict]
    "Continue sequence by examining the current token using current state.
    Returns new state and verdict.")
  (-end [_ state] ; return models
    "Finish by calculating zero or more models from state."))

(set! *warn-on-reflection* true)

; Sequence Expression Library.

(defmethod print-method n01se.seqex.SeqEx [o ^java.io.Writer w]
  (.write w (str \< o \>)))
(prefer-method print-method n01se.seqex.SeqEx java.util.Map)
(prefer-method print-method n01se.seqex.SeqEx clojure.lang.IRecord)
(prefer-method print-method n01se.seqex.SeqEx clojure.lang.IPersistentMap)

; Simple cardnality expressions
(def n0
  (reify SeqEx
    (-begin [_] [Failed Passed])
    (-continue [_ s t] [s s])
    (-end [_ s] nil)

    Object (toString [_] "n0")))

(def n1
  (reify SeqEx
    (-begin [_] [Passed Failing])
    (-continue [_ s t] [Failed s])
    (-end [_ s] nil)
    Object (toString [_] "n1")))

(def n?
  (reify SeqEx
    (-begin [_] [Passed Passing])
    (-continue [_ s t] [Failed s])
    (-end [_ s] nil)
    Object (toString [_] "n?")))

(def n*
  (reify SeqEx
    (-begin [_] [Passing Passing])
    (-continue [_ s t] [Passing s])
    (-end [_ s] nil)
    Object (toString [_] "n*")))

(def n+
  (reify SeqEx
    (-begin [_] [Passing Failing])
    (-continue [_ s t] [Passing s])
    (-end [_ s] nil)
    Object (toString [_] "n+")))

(defrecord Cardnality [low high]
  SeqEx
  (-begin [_] (-continue _ 0 nil))
  (-continue [_ s t]
    [(inc s)
     (cond
       (< s  low)  Failing
       (nil? high) Passing
       (< s  high) Passing
       (= s  high) Passed
       (> s  high) Failed)])
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
          {:-begin (constantly [true Failing])
           :-continue (fn [literal first-time? token]
                        [false (if (clj/and first-time? (= literal token))
                                 Passed
                                 Failed)])
           :-end (constantly nil)}))

; functions and delay refs
(extend-protocol SeqEx
  ;; Functions are treated as predicates on a single token.
  clojure.lang.Fn
  (-begin [_] [true Failing])
  (-continue [pred first-time? token]
    [false (if (clj/and first-time? (pred token))
             Passed
             Failed)])
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
      (-begin [_] [0 Failing])
      (-continue [_ s t]
        [(inc s)
         (if (= s t)
           (if (< s n-1)
             Failing
             (if (= s n-1)
               Passed
               Failed))
           Failed)])
      (-end [_ s] nil))))

(def unique "Sequences with no repeating values."
  (reify SeqEx
    (-begin [_] [#{} Passing])
    (-continue [_ s t] [(conj s t) (vbool (clj/not (contains? s t)))])
    (-end [_ s] nil)))

(defn permute
  "Sequence containing any permuation of elems."
  [elems]
  (let [s (set elems)]
    (reify SeqEx
      (-begin [_] [s (if (empty? s) Passed Failing)])
      (-continue [_ s t] (if (contains? s t)
                           (let [s (disj s t)]
                             [s (if (empty? s) Passed Failing)])
                           [s Failed]))
      (-end [_ s] nil))))

; Higher order expressions (these take expressions as arguments).
; Arguably, these are the only expressions that need to be macros.

(defn- se-not "Sequences where expression is always Failed."
  [seqex]
  (reify SeqEx
    (-begin [_] (-begin seqex))
    (-continue [_ s t]
      (let [[s v] (-continue seqex s t)]
        [s (vbool (= v Failed))]))
    (-end [_ s] nil)))

(defn- combine-results [seqexes bit-op seqex-fn & [svs token]]
  (let [short-verdict (bit-op Passed Failed)
        default-verdict (bit-xor short-verdict Passed)
        svs (clj/or svs (repeat (count seqexes)
                                [nil (bit-or default-verdict Continue)]))]
    (loop [[seqex & more-seqexes :as seqexes] seqexes,
           [[state last-verdict :as last-sv] & more-svs] svs,
           final-state [],
           final-verdict default-verdict]
      (if (empty? seqexes)
        [final-state final-verdict]
        (let [[sub-state sub-verdict :as sub-sv] (if (= default-verdict last-verdict)
                                                   last-sv
                                                   (seqex-fn seqex state token))]
          (if (= short-verdict sub-verdict)
            [(conj (vec (repeat (count final-state)
                                [nil Failed]))
                   sub-sv) sub-verdict] ;; Short Circuit
            (recur more-seqexes more-svs
                   (conj final-state sub-sv)
                   ;; final verdict = logical <op> of matching bits
                   ;;               + logical or of continue bits.
                   (bit-or (bit-op (bit-and Matching final-verdict)
                                   (bit-and Matching sub-verdict))
                           (bit-and Continue final-verdict)
                           (bit-and Continue sub-verdict)))))))))

(defn- logic-combine
  "Sequences in which all seqexes are logically related. Short
  circuits if possible. Calls end-fn with a list of last state and
  verdict pairs for each seqex."
  [seqexes bit-op end-fn]
  (reify SeqEx
    (-begin [_]
      (combine-results seqexes bit-op (fn [seqex _ _] (-begin seqex))))
    (-continue [_ svs token]
      (combine-results seqexes bit-op -continue svs token))
    (-end [_ svs]
      (end-fn svs))
    Object
    (toString [_] (str/join " " (cons (if (= bit-op bit-and) "and" "or")
                                      (map pr-str seqexes))))))

(defn- se-and "Sequences in which all expressions are true."
  [& seqexes]
  (logic-combine seqexes
                 bit-and
                 #(if (some failing? (map second %))
                    nil ;; Failed... No models for you
                    (mapcat -end seqexes (map first %)))))

(defn- se-or "Sequences in which any expression is true."
  [& seqexes]
  (logic-combine seqexes
                 bit-or
                 #(first
                   (some (fn [[seqex [state verdict]]]
                           (when (matching? verdict)
                             [(-end seqex state)]))
                         (map list seqexes %)))))

(defn apply-fn "Sequences where expression is applied to (f value)."
  [f seqex]
  (reify SeqEx
    (-begin [_] (-begin seqex))
    (-continue [_ s t] (-continue seqex s (f t)))
    (-end [_ s] (-end seqex s))
    Object
    (toString [_] (pr-str 'apply-fn f seqex))))

; Serial expression: compose muliple seqexes such that they are applied to the
; sequence one at a time and limited by a higher order seqex on the indicies of
; those seqexes. Pretty much the ultimate power in the universe (:-).

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
    (remove (fn [[ssv ise [is iv]]] (failed? iv)))))

;; TODO I could use an ordered set data structure in branch-paths.
(defn- branch-paths
  "Check if each path has child paths and create those paths as needed."
  [old-paths superior-se inferior-ses]
  (letfn [(branch [new-paths [[old-ss old-sv] old-ise [old-is old-iv] models]]
            (->> inferior-ses
              ;; define first (superior) half of path
              (map-indexed (fn [idx ise] [(-continue superior-se old-ss idx)
                                          ise]))
              ;; filter Failed paths (= sv Failed)
              (remove (fn [[[ss sv] ise]] (failed? sv)))
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
   (apply bit-or Failed Failed
          (for [[[ss sv] ise [is iv]] paths]
            (if (clj/or (continue? sv) (continue? iv))
              (if (clj/and (matching? sv) (matching? iv))
                Passing
                Failing)
              (if (clj/and (matching? sv) (matching? iv))
                Passed
                Failed))))])

(defrecord Serial [superior-se inferior-ses name]
  SeqEx
  (-begin [_]
    (-> (root-path superior-se)
      (branch-paths superior-se inferior-ses)
      judge-paths))
  (-continue [_ paths token]
    (-> (age-paths paths token)
      (branch-paths superior-se inferior-ses)
      judge-paths))
  (-end [_ paths]
    (some (fn [[[ss sv] ise [is iv] models :as path]]
            (when (clj/and (matching? sv) (matching? iv))
                (concat models (-end ise is))))
          paths))
  Object
  (toString [_] (apply pr-str name inferior-ses)))

(defn mk-serial [superior-se inferior-ses & [name]]
  (->Serial superior-se inferior-ses (clj/or name "anonymous Serial")))

;; New API

(defn ord "All seqexes in order." [& seqexes]
  (mk-serial (se-range (count seqexes)) seqexes 'ord))
(defn alt "Alternate between seqexes (pick any one)." [& seqexes]
  (mk-serial n1 seqexes 'alt))
(defn opt "Optionally alternate between seqexes." [& seqexes]
  (mk-serial n? seqexes 'opt))
(defn qty+ "One or more seqexes (in any order)." [& seqexes]
  (mk-serial n+ seqexes 'qty+))
(defn qty* "Zero or more seqexes (in any order)." [& seqexes]
  (mk-serial n* seqexes 'qty*))
(defn qty
  "Repeat seqexes x times. If x is a single number, then repeat exactly that
  many times. If x is a sequence of two numbers then repeat between the first
  and second number of times. Finally, if x is a sequence of 1 number, repeat
  between 0 and that number of times."
  [x & seqexes]
  (mk-serial (nx x) seqexes 'qty))

(defn all "All seqexes in any order."
  [& seqexes]
  (mk-serial (permute (clj/range (count seqexes))) seqexes 'all))

;; Old API

;; choosing expressions
(defn c1 [& seqexes] (mk-serial n1 seqexes))
(defn c? [& seqexes] (mk-serial n? seqexes))
(defn c* [& seqexes] (mk-serial n* seqexes))
(defn c+ [& seqexes] (mk-serial n+ seqexes))
(defn cx [x & seqexes] (mk-serial (nx x) seqexes))

;; sequence expressions
(defn s1 [& seqexes] (mk-serial (se-range (count seqexes)) seqexes))
(defn s? [& seqexes] (c? (apply s1 seqexes)))
(defn s* [& seqexes] (c* (apply s1 seqexes)))
(defn s+ [& seqexes] (c+ (apply s1 seqexes)))
(defn sx [x & seqexes] (cx x (apply s1 seqexes)))

;; Capturing seqexes
(defn cap-model
  "Capture a model based on non-invalid tokens examined by seqex."
  [seqex & {:keys [begin continue end]
            :or   {begin vector continue conj end identity}}]
  (reify SeqEx
    (-begin [_]
      (let [[state verdict] (-begin seqex)]
        [[state (begin)] verdict]))
    (-continue [_ [state model] token]
      (let [[state verdict] (-continue seqex state token)]
        [[state (if (failed? verdict)
                  model
                  (continue model token))]
         verdict]))
    (-end [_ [state model]]
      (cons (end model) (-end seqex state)))))

(defn cap
  "Capture all non-invalid tokens examined by seqex. Return a vector of tokens
  unless finalize is specified and instead pass the token vector to finalize and
  use its return value instead. The resulting 'model' is prepended to any
  sub-models returned by seqex."
  [seqex & [finalize]]
  (cap-model seqex :end (clj/or finalize identity)))

(defn cap-one
  "Capture a single non-invalid token examined by seqex. Return just that token
  or, if specified, pass it to finalize and use its return value instead. If
  multiple tokens are matched by seqex, the last one is used."
  [seqex & [finalize]]
  (cap-model seqex
             :begin (constantly nil)
             :continue #(do %2)
             :end (clj/or finalize identity)))

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
  verdict of Passed or Failed."
  [seqex tokens]
  (loop [[state verdict] (-begin seqex)
         [token & more :as ts] tokens]
    (if (empty? ts)
      (if (matching? verdict)
        [(-end seqex state) Passed]
        [nil Failed])

      (if (continue? verdict)
        (recur (-continue seqex state token) more)
        [nil Failed]))))

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
    (-begin [_] [nil Failing])
    (-continue [_ _ token]
      (if (clj/or (nil? token)
                  (string? token)
                  (coll? token))
        (exec seqex (seq token))
        [nil Failed]))
    (-end [_ result]
      (when-not (nil? result)
        (list result)))
    Object
    (toString [_] (pr-str 'subex seqex))))

(defn se-find
 "Find and return first occurance of seqex in tokens."
  [seqex tokens]
  )

(defn se-seq [seqex tokens]
  "Find and return a sequence of all non-overlapping occurances of seqex.")

;; define our own defmacro but do it near the bottom for obvious reasons.
(clj/defmacro defmacro
  "Return a named macro defined by a seqex that is applied to the macro's
  arguments."
  [name seqex]
  `(clj/defmacro ~name [& tokens#]
     (let [[forms# verdict#] (exec ~seqex tokens#)]
       (if (matching? verdict#)
         (if (= 1 (count forms#))
           (first forms#)
           `(do ~@forms#))))
     `(do ~@(model ~seqex tokens#))))

;; Rename all the se-* expressions that overwrite built in names. Do this near
;; the bottom of the file so as to reduce the chance of accidentally using
;; those expressions during implementation.
(def and se-and)
(def not se-not)
(def or se-or)
(def range se-range)
