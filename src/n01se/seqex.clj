;clojure symbols
(ns n01se.seqex
  "Sequence Expressions. Library for describing sequences."
  (:use [n01se.seqex.util :only [transpose ->when ->when-not]]
        [clojure.pprint :only [pprint]]
        [clojure.string :as str :only []])
  (:refer-clojure :exclude [and not or range cat]))

(alias 'clj 'clojure.core)

;; Verdicts
(def ^:const Failed  2r00) ;; Not matching and don't continue.
(def ^:const Failing 2r01) ;; Not matching but continue.
(def ^:const Passed  2r10) ;; Matching but don't continue.
(def ^:const Passing 2r11) ;; Matching and continue.

(def ^:const Continue Failing)
(def ^:const Matching Passed)

(defn failed?   [v] (= v Failed))
(defn failing?  [v] (= v Failing))
(defn passed?   [v] (= v Passed))
(defn passing?  [v] (= v Passing))
(defn continue? [v] (= (bit-and v Continue) Continue))
(defn matching? [v] (= (bit-and v Matching) Matching))

(defn vbool [v] (if v Passing Failed))

(defprotocol SeqEx
  "Sequence expression protocol. Used by types that implement seqexes."
  (begin- [_] ; return [state verdict]
    "Initial function that returns the beginning state and verdict.")
  (continue- [_ state token] ; return [state verdict]
    "Continue sequence by examining the current token using current state.
    Returns new state and verdict.")
  (model- [_ state] ; return models
    "Finish by calculating zero or more models from state.")
  (error- [_ state] ; return error string
    "Return a description of why the seqex failed given state."))
;; Useful for navigation
(defprotocol Tree
  (children- [_] "Return immediate children of tree node."))

(defn children [x]
  (if (= clojure.lang.Delay (type x))
    (children- (deref x))
    (children- x)))

(defrecord SeqExError [msg seqex-state])

(def subex-error-msg "Nested rule failed:")

(defn error-msg
  ([msg] (error-msg msg SeqExError))
  ([msg state] (SeqExError. msg state)))

(defn error [seqex state]
  (if (= SeqExError (type state))
    (-> [(:msg state)]
      (->when (not= SeqExError (:seqex-state state))
              (concat (let [msgs (error- seqex (:seqex-state state))]
                        (if (= (first msgs) subex-error-msg)
                          msgs
                          (cons (str "Expected " (first msgs))
                                (rest msgs)))))))
    (error- seqex state)))

(defn format-errors [errors & {:keys [pad] :or {pad ""}}]
  (->> errors
    (map #(if (string? %)
            (str pad %)
            (format-errors % :pad (str pad "    "))))
    (interpose "\n")
    (apply str)))

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
    (begin- [_] [Failed Passed])
    (continue- [_ s t] [s s])
    (model- [_ s] nil)
    (error- [_ s] [])
    Object (toString [_] "n0")
    Tree (children- [_] nil)))

(def n1
  (reify SeqEx
    (begin- [_] [Passed Failing])
    (continue- [_ s t] [Failed s])
    (model- [_ s] nil)
    (error- [_ s] ["any one value"])
    Object (toString [_] "n1")
    Tree (children- [_] nil)))

(def n?
  (reify SeqEx
    (begin- [_] [Passed Passing])
    (continue- [_ s t] [Failed s])
    (model- [_ s] nil)
    (error- [_ s] [])
    Object (toString [_] "n?")
    Tree (children- [_] nil)))

(def n*
  (reify SeqEx
    (begin- [_] [Passing Passing])
    (continue- [_ s t] [Passing s])
    (model- [_ s] nil)
    (error- [_ s] [])
    Object (toString [_] "n*")
    Tree (children- [_] nil)))

(def n+
  (reify SeqEx
    (begin- [_] [Passing Failing])
    (continue- [_ s t] [Passing s])
    (model- [_ s] nil)
    (error- [_ s] ["at least one value"])
    Object (toString [_] "n+")
    Tree (children- [_] nil)))

(defrecord Cardnality [low high]
  SeqEx
  (begin- [_] (continue- _ 0 nil))
  (continue- [_ s t]
    [(inc s)
     (cond
       (< s  low)  Failing
       (nil? high) Passing
       (< s  high) Passing
       (= s  high) Passed
       (> s  high) Failed)])
  (model- [_ s] nil)
  (error- [_ s] [(str (- low s) " value(s)")])
  Tree (children- [_] nil))

(defn nx [x] (if (sequential? x)
               (condp = (count x)
                 1 (->Cardnality 0 (first x))
                 2 (apply ->Cardnality x))
               (->Cardnality x x)))

; literal expressions
(defprotocol LitEx)
(doseq [T [nil clojure.lang.ArraySeq clojure.lang.Keyword
           clojure.lang.LazySeq clojure.lang.Cons
           clojure.lang.PersistentArrayMap
           clojure.lang.PersistentHashSet clojure.lang.PersistentList
           clojure.lang.PersistentVector clojure.lang.Symbol
           java.lang.Character java.lang.Double java.lang.Long
           java.lang.String]]
  (extend T
    LitEx {}
    SeqEx {:begin- (constantly [true Failing])
           :continue- (fn [literal first-time? token]
                        [false (if (clj/and first-time? (= literal token))
                                 Passed
                                 Failed)])
           :model- (constantly nil)
           :error- (fn [literal _]
                     [(pr-str literal)])}
    Tree {:children- (constantly nil)}))

; functions and delay refs
(extend-protocol SeqEx
  ;; Functions are treated as predicates on a single token.
  clojure.lang.Fn
  (begin- [_] [true Failing])
  (continue- [pred first-time? token]
    [false (if (clj/and first-time? (pred token))
             Passed
             Failed)])
  (model- [_ _] nil)
  (error- [pred _]
    [(if-let [label (:terminal (meta pred))]
       (str label)
       (get {symbol? "symbol"
                keyword? "keyword"
                vector? "vector"
                map? "map"}
               pred
               (pr-str pred))) ])

  ;; Delays are assumed to hold a seqex to be used.
  clojure.lang.Delay
  (begin- [d] (begin- @d))
  (continue- [d s t] (continue- @d s t))
  (model- [d s] (model- @d s))
  (error- [d s] (error- @d s)))

(extend-protocol Tree
  clojure.lang.Fn
  (children- [_] nil)
  clojure.lang.Delay
  #_(children- [d] (pprint (type @d)) @d))

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
    (begin- [_] (continue- _ nil unique-value))
    (continue- [_ s t] [t (vbool (not= s t))])
    (model- [_ s] nil)
    (error- [_ s] [(str "any value not equal to previous value: " (pr-str s))])))

(def asc "Sequences of values greater than or equal to previous values."
  (reify SeqEx
    (begin- [_] [unique-value Passing])
    (continue- [_ s t] (if (number? t)
                         (if (clj/or (= s unique-value) (<= s t))
                           [t Passing]
                           [s Failed])
                         [nil Failed]))
    (model- [_ s] nil)
    (error- [_ s] [(if (nil? s)
                    (str "number")
                    (str "number >= " (pr-str s)))])))

(defn- se-range "Sequences of incrementing numbers from 0 to n-1."
  [n]
  (let [n-1 (dec n)]
    (reify SeqEx
      (begin- [_] [0 Failing])
      (continue- [_ s t]
        (if (= s t)
          [(inc s) (if (< s n-1)
                     Failing
                     (if (= s n-1)
                       Passed
                       Failed))]
          [s Failed]))
      (model- [_ s] nil)
      (error- [_ s] [(if (nil? s)
                       (str "number")
                       (pr-str s))]))))

(def unique "Sequences with no repeating values."
  (reify SeqEx
    (begin- [_] [#{} Passing])
    (continue- [_ s t] [(conj s t) (vbool (clj/not (contains? s t)))])
    (model- [_ s] nil)
    (error- [_ s] ["none of: " (map pr-str s)])))

(defn permute
  "Sequence containing any permuation of elems."
  [elems]
  (let [s (set elems)]
    (reify SeqEx
      (begin- [_] [s (if (empty? s) Passed Failing)])
      (continue- [_ s t] (if (contains? s t)
                           (let [s (disj s t)]
                             [s (if (empty? s) Passed Failing)])
                           [s Failed]))
      (model- [_ s] nil)
      (error- [_ s] ["any of: " (map pr-str s)]))))

; Higher order expressions (these take expressions as arguments).
; Arguably, these are the only expressions that need to be macros.

(defn- se-not "Sequences where expression is always Failed."
  [seqex]
  (reify SeqEx
    (begin- [_] (begin- seqex))
    (continue- [_ s t]
      (let [[s v] (continue- seqex s t)]
        [s (vbool (= v Failed))]))
    (model- [_ s] nil)
    (error- [_ s] ["failure by seqex (lame error message)"])
    Tree
    (children- [_] [seqex])))

;; sva: triple of [state, verdict, active]
;; svas: multiple sva triples
(defn- combine-results [seqexes bit-op f & [svas token]]
  (let [;; Define our logical verdicts depending on bit-op
        [passed-verdict
         failed-verdict
         failing-verdict] (condp = bit-op
                            bit-or  [Passed Failed Failing]
                            bit-and [Failed Passed Passing])
        ;; If svas was not specified, use a default.
        svas (clj/or svas (repeat (count seqexes)
                                  [nil failing-verdict true]))]

    (loop [[seqex & more-seqexes :as seqexes] seqexes,
           [[state verdict] & more-svas] svas,
           final-svas [],
           final-verdict failed-verdict]

      (if (empty? seqexes)
        ;; we are done
        [final-svas final-verdict]

        (let [[new-state new-verdict :as new-sva]
                (if (= failed-verdict verdict)
                  ;; do nothing and unset the active flag
                  [state verdict false]

                  ;; apply f and set the active flag
                  (conj (f seqex state token)
                        true))
              final-svas (conj final-svas new-sva)]

          (if (= passed-verdict new-verdict)
            ;; Stop looping and report success (short circuit)
            [final-svas passed-verdict]

            ;; continue to loop through seqexes and states
            (recur more-seqexes more-svas
                   final-svas

                   ;; Calculate final verdict:
                   (bit-or (bit-and Matching
                                    (bit-op final-verdict
                                            new-verdict))
                           (bit-and Continue
                                    (bit-or final-verdict
                                            new-verdict))))))))))

(defrecord LogicEx [seqexes bit-op end-fn error-fn]
  SeqEx
  (begin- [_]
    (combine-results seqexes bit-op (fn [seqex _ _] (begin- seqex))))
  (continue- [_ state token]
    (combine-results seqexes bit-op continue- state token))
  (model- [_ state]
    (end-fn state))
  (error- [_ state]
    (error-fn state))
  Tree
  (children- [_] seqexes)

  Object
  (toString [_] (str/join " " (cons (if (= bit-op bit-and) "and" "or")
                                    (map pr-str seqexes)))))
(defn- logic-combine
  "Sequences in which all seqexes are logically related. Short
  circuits if possible. Calls end-fn with a list of last state and
  verdict pairs for each seqex."
  [seqexes bit-op end-fn error-fn]
  (->LogicEx seqexes bit-op end-fn error-fn))

(defn verdict [pred]
  (fn [[_ _ verdict :as ssv]]
    (when (pred verdict) ssv)))

(defn get-errors [paths]
  (if (= 1 (count paths))
    (let [[seqex state] (first paths)]
      (error- seqex state))
    ["any of:" (mapcat #(error- (first %) (second %)) paths)]))

(defn- se-and "Sequences in which all expressions are true."
  [& seqexes]
  (logic-combine seqexes
                 bit-and
                 #(if (some failing? (map second %))
                    nil ;; Failed... No models for you
                    (mapcat model- seqexes (map first %)))

                 #(let [ssvas (map list* seqexes %)
                        failed-ssvas (filter (verdict failed?) ssvas)
                        error-ssvas (if (empty? failed-ssvas)
                                       (filter (verdict continue?) ssvas)
                                       failed-ssvas)]
                    (get-errors error-ssvas))))

(defn- se-or "Sequences in which any expression is true."
  [& seqexes]
  (logic-combine seqexes
                 bit-or
                 #(let [ssvas (map list* seqexes %)
                        [seqex state] (clj/or (some (verdict passed?) ssvas)
                                              (some (verdict passing?) ssvas))]
                    (model- seqex state))

                 (fn error [svas]
                   (let [ssvas (map list* seqexes svas)
                         continue-ssvas (filter (verdict continue?) ssvas)
                         error-ssvas (if (empty? continue-ssvas)
                                       (->> ssvas
                                            (filter last) ;; just active ssva
                                            (filter (verdict failed?)))
                                       continue-ssvas)]
                     (get-errors error-ssvas)))))

(defn apply-fn "Sequences where expression is applied to (f value)."
  [f seqex]
  (reify SeqEx
    (begin- [_] (begin- seqex))
    (continue- [_ s t] (continue- seqex s (f t)))
    (model- [_ s] (model- seqex s))
    (error- [_ s] (error- seqex s)) ;; hmm, this error may be misleading...

    Object
    (toString [_] (pr-str 'apply-fn f seqex))
    Tree
    (children- [_] [seqex])))

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

(defrecord SerialPath [ssv sv ss ise isv is iv parent])
(defn path [ssv ise isv parent]
  (->SerialPath
      ssv
      (first ssv) (second ssv)
      ise
      isv
      (first isv) (second isv)
      parent))

(defn pr-paths "Useful debugging tool."
  [paths msg]
  (println msg)
  (doseq [[[ss sv] ise [is iv] parent] paths]
    (prn "path:" [iv ise]))
  paths)

(defn- root-path
  "Define the initial path."
  [superior-se]
  [[(begin- superior-se) n0 (begin- n0) nil]])

(defn- age-paths
  "Apply current token to paths"
  [paths token]
  (->> paths
    ;; keep only continuing paths
    (filter (fn [[ssv ise [is iv]]] (continue? iv)))
    ;; apply token to each continuing path
    (map (fn [[ssv ise [is iv] parent]]
           [ssv ise (continue- ise is token) parent]))))

;; TODO I could use an ordered set data structure in branch-paths.
(defn- branch-paths
  "Check if each path has child paths and create those paths as needed."
  [old-paths superior-se inferior-ses]
  (letfn [(branch [new-paths [[old-ss old-sv] old-ise [old-is old-iv] :as parent-path]]
            (->> inferior-ses
              ;; define first (superior) half of path
              (map-indexed (fn [idx ise] [(continue- superior-se old-ss idx)
                                          ise]))
              ;; filter Failed paths (= sv Failed)
              (remove (fn [[[ss sv] ise]] (failed? sv)))
              ;; define second (inferior) half of path
              (map (fn [[ssv ise :as path]]
                     (-> path
                         (conj (begin- ise))
                         (conj parent-path))))
              (reduce inspect new-paths)))

          (dup-path? [new-paths [ssv ise isv]]
            (some (fn [[ssv2 ise2 isv2]]
                    (= [ssv ise isv] [ssv2 ise2 isv2]))
                  new-paths))

          (inspect [new-paths [[ss sv] ise [is iv] :as old-path]]
            (-> new-paths
              (->when-not (dup-path? new-paths old-path)
                  (conj old-path)
                  (->when (clj/and
                               (continue? sv)
                               (matching? iv))
                      (branch old-path)))))]
    (reduce inspect [] old-paths)))

(defn- judge-paths
  "Combine the verdicts of each path into a final verdict. Return a pair of
  paths plus final verdict."
  [paths]
  [paths
   (apply bit-or Failed Failed
          (for [[[ss sv] ise [is iv]] paths
                :when (clj/not (failed? iv))]
            (if (clj/or (continue? sv) (continue? iv))
              (if (clj/and (matching? sv) (matching? iv))
                Passing
                Failing)
              (if (clj/and (matching? sv) (matching? iv))
                Passed
                Failed))))])

(defrecord Serial [superior-se inferior-ses name]
  SeqEx
  (begin- [_]
    (-> (root-path superior-se)
        #_(pr-paths "root path")
        (branch-paths superior-se inferior-ses)
        #_(pr-paths "After branching")
        judge-paths
        ))
  (continue- [_ paths token]
    (-> paths
        (age-paths token)
        #_(pr-paths (str "After aging (" token ")"))
        (branch-paths superior-se inferior-ses)
        #_(pr-paths "After branching")
        judge-paths))
  (model- [_ paths]
    (some (fn [[[ss sv] ise [is iv] :as path]]
            (when (clj/and (matching? sv) (matching? iv))
              ;; loop through this and all parent models, calling model- on each.
              (loop [[_ ise [is iv] parent] path, models ()]
                (let [models (concat (model- ise is) models)]
                  (if (nil? parent)
                    models
                    (recur parent models))))))
          paths))
  (error- [_ paths]
    (assert (clj/not (empty? paths))) ;; there should always be an empty path.

    ;; First, something went wrong because error was called.
    ;; Second, judge-paths must have aggregated a non-matching final verdict.
    ;;   If the final verdict was Failing, we must have run out of tokens
    ;;   therefore the question is: which token(s) were we waiting for?
    ;;   answer: ask all Failing paths.
    ;;   If the final verdict was Failed, we must have been given a bad token
    ;;   therefore the question is: which token(s) were we waiting for?
    ;;   answer: ask all Failed paths.
    (let [open-paths (filter (fn [[ssv ise [is iv]]] (continue? iv))
                             paths)
          paths (if (empty? open-paths)
                  paths
                  open-paths)]
      (if (= 1 (count paths))
        (let [[ssv ise [is iv]] (first paths)]
          (error- ise is))
        ["any of:" (mapcat (fn [[ssv ise [is iv]]] (error- ise is))
                                    paths)])))
  Object
  (toString [_] (apply pr-str name inferior-ses))

  Tree (children- [_] inferior-ses))

(defn mk-serial [superior-se inferior-ses & [name]]
  (->Serial superior-se inferior-ses (clj/or name "anonymous Serial")))

;; New API

(defn cat "Concatenate seqexes." [& seqexes]
  (mk-serial (se-range (count seqexes)) seqexes 'cat))
(defn alt "Alternate between seqexes (pick any one)." [& seqexes]
  (mk-serial n1 seqexes 'alt))
(defn opt "Optionally alternate between seqexes." [& seqexes]
  (mk-serial n? seqexes 'opt))
(defn rep+ "One or more seqexes (in any order)." [& seqexes]
  (mk-serial n+ seqexes 'rep+))
(defn rep* "Zero or more seqexes (in any order)." [& seqexes]
  (mk-serial n* seqexes 'rep*))
(defn rep
  "Repeat seqexes x times. If x is a single number, then repeat exactly that
  many times. If x is a sequence of two numbers then repeat between the first
  and second number of times. Finally, if x is a sequence of 1 number, repeat
  between 0 and that number of times."
  [x & seqexes]
  (mk-serial (nx x) seqexes 'rep))

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
(defn cap-tokens
  "Capture tokens based on non-invalid tokens examined by seqex."
  [seqex & {:keys [begin continue end]
            :or   {begin vector continue conj end identity}}]
  (reify SeqEx
    (begin- [_]
      (let [[state verdict] (begin- seqex)]
        [[state (begin)] verdict]))
    (continue- [_ [state model] token]
      (let [[state verdict] (continue- seqex state token)]
        [[state (if (failed? verdict)
                  model
                  (continue model token))]
         verdict]))
    (model- [_ [state model]]
      (cons (end model) (model- seqex state)))
    (error- [_ [state model]]
      (error- seqex state))
    Tree
    (children- [_] [seqex])))


(defn cap
  "Capture all tokens examined by seqex (assuming seqex is valid) and return a
  sequence of captured tokens. If finalize is given, call it with the sequence
  of captured tokens as it's single argument and use its return value as the
  result."
  [seqex & [finalize]]
  (cap-tokens seqex
              :end (clj/or finalize identity)))

(defn recap-map
  "Call finalize with a sequence of models returned from seqex. Finalize is
  expected to return a sequence of new models."
  [seqex finalize]
  (reify SeqEx
    (begin- [_] (begin- seqex))
    (continue- [_ state token] (continue- seqex state token))
    (model- [_ state] (finalize (model- seqex state)))
    (error- [_ [state model]] (error- seqex state))
    Tree
    (children- [_] [seqex])))

(defn recap
  "Apply finalize to a sequence of models returned by seqex. The return value of
  finalize is treated as a single new model."
  [seqex finalize]
  (reify SeqEx
    (begin- [_] (begin- seqex))
    (continue- [_ state token] (continue- seqex state token))
    (model- [_ state] (vector (apply finalize (model- seqex state))))
    (error- [_ [state model]] (error- seqex state))
    Tree
    (children- [_] [seqex])))

;; API for using seqexes
(defn exec
  "Apply a seqex to a series of tokens. Returns the state plus verdict."
  [seqex tokens]
  (loop [[state verdict :as pair] (begin- seqex)
         [token & more :as ts] tokens]
    (if (empty? ts)
      (if (failing? verdict)
        [(error-msg (str "Missing value") state) Failed]
        [state Passed])
      (if (continue? verdict)
        (let [[state verdict :as pair] (continue- seqex state token)]
          (if (failed? verdict)
            [(error-msg (str "Bad value: " (pr-str token)) state) Failed]
            (recur pair more)))
        [(error-msg (str "Extra value: " (pr-str token))) Failed]))))

(defn valid?
  "Returns true when tokens are a valid input for seqex."
  [seqex tokens]
  (matching? (second (exec seqex tokens))))

(defn parse
  "Executes seqex against tokens returning any captured model."
  [seqex tokens]
  (let [[state verdict] (exec seqex tokens)]
    (if (matching? verdict)
      (model- seqex state)
      (println (format-errors (error seqex state))))))

(def not-seqable (Object.))

(defrecord SubEx [seqex]
  SeqEx
  (begin- [_] [not-seqable Failing])
  (continue- [_ _ token]
    (if (clj/or (nil? token)
                (string? token)
                (coll? token))
      (exec seqex (seq token))
      [not-seqable Failed]))
  (model- [_ state]
    (model- seqex state))
  (error- [_ state]
    (if (= not-seqable state)
      ["seqable (nil, string or collection)."]
      [subex-error-msg (error seqex state)]))

  Object (toString [_] (pr-str 'subex seqex))
  Tree (children- [_] [seqex]))

(defn subex
  "Matches a single sequential token, matching seqex on its contents.
  This is a sort of 'descend' operation for matching nested data."
  [seqex]
  (->SubEx seqex))

;; Rename all the se-* expressions that overwrite built in names. Do this near
;; the bottom of the file so as to reduce the chance of accidentally using
;; those expressions during implementation.
(def and se-and)
(def not se-not)
(def or se-or)
(def range se-range)


(comment "isomorphs: things that do the same thing."
         (rep+ n1) == n+
         (rep* n1) == n*
         (rep x n1) == (nx x)

         (alt x y z) == (or x y z)

         (rep* (rep* x y z)) == (rep* x y z)
         (rep* (rep+ x y z)) == (rep* x y z)
         (rep* (opt x y z)) == (rep* x y z)
         (rep* (alt x y z)) == (rep* x y z)
         (rep* (or x y z)) == (rep* x y z)

         (rep+ (rep+ x y z)) == (rep+ x y z)
         (rep+ (rep* x y z)) == (rep* x y z)
         (rep+ (opt x y z)) == (rep* x y z)
         )
