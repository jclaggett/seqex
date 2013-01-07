(ns tst
  (:use (clojure.pprint)))

(defn reload [] (use 'tst :reload))

; Experiment with 'topic' blocks as an alternative to -> and ->>.
(defmacro topic
  "Set the topic variable that is set to the result of each
  expression in the block."
  ([t] nil)
  ([t form] form)
  ([t form & more]
    `(let [~t ~form] (topic ~t ~@more))) )

(defmacro let2
  "Alternate let block that doesn't have a body, just bindings. The last binding is returned."
  ([] nil)
  ([& bindings]
    `(let [~@bindings] ~(first (take-last 2 bindings))) ) )

(def topic-ex1
  (topic % 1
    (+ 1 %)
    (/ % 2) ) )

(def topic-ex2
  [
    '(->>
      (map vector (range) coll)
      (filter (fn [[i & args]] (apply f args)))
      (map first) )

    '(topic coll
      (map vector (range) coll)
      (filter (fn [[i & args]] (apply f args)) coll)
      (map first coll) ) ] )

; solutions for the splice function

(def a [nil 2 3 nil nil 6 nil 8 9 nil])
(def b [1 4 5 7])


(defn splice1
  "Nil values in the primary collection are replaced with values from the
  secondary collection."
  [primary secondary]
  (loop
    [
      [p & prest :as pall] primary
      [s & srest :as sall] secondary
      result [] ]
    (if pall
      (if p
        (recur prest sall (conj result p))
        (recur prest srest (conj result s)) )
      result) ) )

; Various position functions
(defn position
  "Return a list of indicies matching f against coll"
  [f coll]
  (topic coll
    (map vector (range) coll)
    (filter (fn [[i & args]] (apply f args)) coll)
    (map first coll) ) )

(defn position2
  "Return a list of indicies matching f against coll"
  [f coll]
  (filter identity (map-indexed #(when (f %2) %1) coll)) )

(defn position3
  "Return a list of indicies matching f against coll"
  [f coll]
  (keep-indexed #(when (f %2) %1) coll) )

; First attempt at lazy sequences.

(defn lazy-count-to-ten
  ([] (lazy-count-to-ten 0))
  ([i]
    (println (str "before[" i "]"))
    (lazy-seq
    (println (str "inside[" i "]"))
      (when (< i 10)
        (cons i (lazy-count-to-ten (inc i))) ) ) ) )

(defn count-to-ten
  ([] (count-to-ten 0))
  ([i]
      (if (< i 10)
        (do
          (print (str ":" i ":"))
          (cons i (count-to-ten (inc i))))
        () ) ) )

; Notes on bookending

(defn bookend
  [ sym coll ]
  (if (= sym (last coll))
    (drop-last coll)
    (throw (Exception.
      (str "Missing or incorrect ending symbol. Expected "
      sym " but found " (last coll) "." ) )) ) )

(defmacro defn-tst [ & args ]
  `(defn ~@(bookend (symbol "defn-tst") args) ) )

; Notes on lazy macro

(defn nested-lazy-seq
  []
  (lazy-seq (cons 1
    (lazy-seq (cons 2
      (lazy-seq (cons 3
        () )) )) )) )


(defmacro lazy
  "Returns a lazy sequence of calls to (f). Arguments evaluated once and as needed.
   (lazy f a b c d) --> (a, (f a b), (f (f a b) c)), (f (f (f a b) c) d)"
  ([f x]
    `(lazy-seq
      (list ~x)) )
  ([f x form & more]
    `(lazy-seq
      (let [x# ~x]
        (cons x#
          (lazy ~f (~f x# ~form) ~@more)))) ) )


(defn lazy-fn
  "Returns a lazy sequence of calls to (f). Arguments evaluated once and as needed.
   (lazy f a b c d) --> (a, (f a b), (f (f a b) c)), (f (f (f a b) c) d)"
  [f form & more])


(defmacro lazy->  [& args] `(lazy ->  ~@args))
(defmacro lazy->> [& args] `(lazy ->> ~@args))


(def ex1 (lazy + 1 2 3 4 5 6))
(def ex2 (lazy * 1 2 3 4 5 6))
(def ex3 (lazy-> 1 (+ 1) (+ 4) (- 2)))
(def ex4
  (lazy->>
    (#(do (println "(Evaluating first)") 1))
    (#(do (println "(Evaluating second)") (+ 1 %)))
    (#(do (println "(Evaluating third)") (- 2 %)))))


; Question: How is state managed? Callbacks will almost always change the state
; and it will happen that the state will be branched and need to be merged. I
; would like to describe policies for specific physical branches of the state.
; For example, the default policy for the entire state might be to use the most
; recent change only. Some data in the state will use the least recent change.
; Some others will use a merge function to combine changes.

(defn aengine
  "Analytiyc engine: used to repeatedly process callback functions. Callback
  functions return nil, a single function or a collection of functions."
  [fns]
  (when-not (empty? fns)
    (recur (keep (flatten (map #(%) fns)))) ) )

;; Yet more macro code for conveniently defining recursive constraints.
(defn make-bodies [args vals fns]
  (for [[n a & body] fns]
    `(~n ~a (let [~@(interleave args vals)] ~@body))))

(defmacro x [name args & fns]
 `(defmacro ~name ~args
    `(reify clojure.lang.IDeref
       ~@(make-bodies '~args ~args '~fns))))

(defn make-bodies [args get-values fns]
  (for [[n a & body] fns]
    `(~n ~a (let [~args @~get-values] ~@body))))

(defmacro x [name args & fns]
  (let [get-values (gensym)]
    `(defmacro ~name ~args
       `(let [~'~get-values (delay ~~args)]
          (reify clojure.lang.IDeref
            ~@(make-bodies '~args '~get-values '~fns))))))

; Careful! blindly expands all symbol names. This may break body.
(defmacro crude-symbol-macro-let [bindings body]
  (clojure.walk/prewalk-replace (apply hash-map bindings) body))

; Works like normal let but the bound expressions are not evaulated
; until they are first referenced.
(defmacro lazy-let [bindings & body]
  (let [binding-pairs (for [[k v] (partition 2 bindings)
                            :let [temp-name (gensym (str k))]]
                        {:let-bindings [temp-name `(delay ~v)]
                         :macro-bindings [k `(deref ~temp-name)]})]
    `(let [~@(mapcat :let-bindings binding-pairs)]
       (crude-symbol-macro-let [~@(mapcat :macro-bindings binding-pairs)]
          (do ~@body)))))

; Works like lazy-let but just wraps each bound value in a delay so
; deferencing is required.
(defmacro let-delay [bindings & body]
  `(let [~@(mapcat (fn [[k v]] [k `(delay ~v)]) (partition 2 bindings))]
    ~@body))

; Syntax quote stuff (should be defined elsewhere)
(defn synquote* [x]
  (letfn [(recurse [coll] (doall (map synquote* coll)))]
    (cond
      (seq? x) (condp = (first x)
                 `unquote `(list ~(second x))
                 `unquote-splicing (second x)
                 `(list (concat ~@(recurse x))))
      (vector? x) `(list (vec (concat ~@(recurse x))))
      (map? x) `(list (apply array-map (concat ~@(recurse (apply concat x)))))
      :else `(list '~x))))

(defmacro synquote [x]
  `(first ~(synquote* x)))

(defmacro let-synquote [specs & body]
  (letfn [(replace-quote [x]
            (clojure.walk/postwalk-replace `{quote synquote} x))]
    (eval
      `(let [~@(replace-quote specs)]
        (synquote (do ~@body))))))

(let-synquote [
    deftsc (fn [name args & fns]
        (let [
            ; Define the default functions
            default-fns [
                '(init [_] [nil Satisfied])
                '(match [_ s t] [nil Satisfied])
                '(model [_ s] ni)]

            ; Make a map of the default functions using their names as keys.
            default-fns (into {}
                (map (fn [x] [(first x) x]) default-fns))]
          '(def ~name
            (reify [~@args]
              ~@(map #(get default-fns (first %1) %1) fns)))))])

