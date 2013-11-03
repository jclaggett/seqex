(ns n01se.syntax
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [n01se.seqex :as se]
            [n01se.seqex.util :refer [->when ->when-not]])
  (:refer-clojure :exclude [name and not or]))
(alias 'clj 'clojure.core)

;; Define the 'API' for defining clojure syntax.

;; terminals
(def form (vary-meta se/n1 assoc :terminal 'form))
(def name (vary-meta symbol? assoc :terminal 'name))
(def string (vary-meta string? assoc :terminal 'string))

;; groupings
(defn cat [& forms]
  (with-meta (apply se/ord forms)
    {:bnf :cat}))

(defn or [& forms]
  (with-meta (apply se/or forms)
    {:bnf :or}))

(defn and [& forms]
  (with-meta (apply se/and forms)
    {:bnf :and}))

(defn opt [& forms]
  (with-meta (apply se/opt forms)
    {:bnf :opt}))

(defn rep [x & forms]
  (with-meta (apply se/qty x forms)
    {:bnf :rep}))

(defn rep+ [& forms]
  (with-meta (apply se/qty+ forms)
    {:bnf :rep+}))

(defn rep* [& forms]
  (with-meta (apply se/qty* forms)
    {:bnf :rep*}))

(defn rule [rule form]
  (vary-meta form assoc :rule rule))

;; Grammar generation related code

;; ANSI codes
;; Credit: Chouser wrote this

;; Define vars that contain strings.  The strings are only useful when
;; passed to the ansi function, which takes either a single code
;; string or a vector of them.
(def color-names '[black red green yellow blue magenta cyan white])
(def style-names '[reset bold faint italic underline blink blink-fast invert])

(defn define-ansi-codes [code-prefix name-prefix names]
  (doseq [[i name] (map-indexed list names)]
    (intern *ns* (symbol (str name-prefix name)) (str code-prefix i))))

(define-ansi-codes "3" "" color-names)
(define-ansi-codes "4" "bg-" color-names)
(define-ansi-codes "" "" style-names)

(defn ansi
  ([codes] (symbol (str \o33 \[ (if (coll? codes) (str/join \; codes) codes) \m)))
  ([codes & text] (symbol (str (ansi codes) (apply str text) (ansi reset)))))

(defn parse-forms
  "Executes seqex against forms returning any model(s). Prints a
  syntax error if the forms do not conform to seqex."
  [seqex forms]
  (let [out-forms (se/parse seqex forms)]
    (case (count out-forms)
      0 nil
      1 (first out-forms)
      `(do ~@out-forms))))

;; Define a special syntax expression to act both as a (trivial) seqex
;; and as a callable macro. TODO figure out how to do varargs.
(defrecord SyntaxEx [seqex]
  n01se.seqex.SeqEx
  (begin- [_] (se/begin- seqex))
  (continue- [_ state token] (se/continue- seqex state token))
  (model- [_ state] (se/model- seqex state))
  (error- [_ state] (se/error- seqex state))
  clojure.lang.IFn
  (invoke [_ forms] (parse-forms seqex forms)))

;; TODO add this as a protocol method of SeqEx. It's hardly like
;; walking a seqex tree is unique to this application.
(defn child-seqexes
  "Get the child seqexes"
  [seqex]
  (if (= n01se.seqex.Serial (type seqex))
        (.inferior-ses seqex)
        (if (= n01se.seqex.LogicEx (type seqex))
          (.seqexes seqex)
          (if (= SyntaxEx (type seqex))
            [(.seqex seqex)]
            nil))))

(defn str-items [before between after items]
  (apply str
         (concat [before]
                 (interpose between items)
                 [after])))

(defn rule-name [seqex]
  (-> seqex meta :rule))

(declare rule-str)

(defn sub-rules [before between after items]
  (str-items (ansi [bold blue] before)
             (ansi [bold blue] between)
             (ansi [bold blue] after)
             (map #(rule-str %) items)))

(defn rule-str [seqex & [body?]]
  (if-let [rule-name (when-not body? (-> seqex meta :rule))]
    (ansi [green] rule-name)
    (if-let [terminal-name (-> seqex meta :terminal)]
      (ansi [bold cyan] terminal-name)
      (case (-> seqex meta :bnf)
        :or (sub-rules "(" " | " ")" (child-seqexes seqex))
        :and (sub-rules "(" " & " ")" (child-seqexes seqex))
        :cat (sub-rules "" ", " "" (child-seqexes seqex))
        :opt (sub-rules "[" " | " "]" (child-seqexes seqex))
        :rep (sub-rules "{" " | " "}" (child-seqexes seqex))
        :rep* (sub-rules "{" " | " "}" (child-seqexes seqex))
        :rep+ (str
               (sub-rules "(" " | " ")" (child-seqexes seqex))
               " "
               (sub-rules "{" " | " "}" (child-seqexes seqex)))
        :syn (rule-str (first (child-seqexes seqex)))
        nil (if (satisfies? se/LitEx seqex)
              (pr-str seqex)
              (ansi red (pr-str seqex)))
        ))))

(defn find-rules
  "Find all rule nodes in seqex tree and return them breadth first."
  [seqex]
  (let [sub-rules (mapcat find-rules (child-seqexes seqex))]
    (if (-> seqex meta :rule)
      (cons seqex sub-rules)
      sub-rules)))

(defn trim-rules
  "Remove duplicate rules (keeping last-most rules)."
  [seqexes]
  (-> (reduce (fn [state seqex]
                (-> state
                  (->when-not (contains? (:found-rules state) (rule-name seqex))
                              (update-in [:found-rules] conj (rule-name seqex))
                              (update-in [:new-rules] conj seqex))))
              {:new-rules () :found-rules #{}}
              (reverse seqexes))
    :new-rules))

(defn str-rules
  "Create a single string containing all rules."
  [seqexes]
  (let [len-max (apply max (map #(-> % rule-name str count) seqexes))]
    (str/join "\n"
              (for [seqex seqexes]
                (let [name (str (rule-name seqex))
                      pad (str/join "" (repeat (- len-max (count name)) " "))]
                  (str pad
                       (ansi green name)
                       (ansi [bold blue] " = ")
                       (rule-str seqex true)      ))))))

(defn syn*
  "Print the syntax of seqex. Generally assumes that seqex is
  decorated with syntax metadata."
  [seqex]
  (-> seqex
    (->when (clj/and (not= :syn (-> seqex meta :bnf))
                     (clj/not (rule-name seqex)))
                (vary-meta assoc :rule 'syntax))
    find-rules
    trim-rules
    str-rules
    println))


(defmacro syn
  [x]
  `(syn* ~(if (symbol? x)
            (if-let [v (resolve x)]
              (if-let [seqex (-> v meta :seqex)]
                `(-> (var ~x) meta :seqex)
                x)
              x)
            x)))

(let [seqex (cat (se/cap name)
                 (se/cap (opt string))
                 (se/cap form))]
  (defmacro defsyntax
    "Return a macro defined so that seqex is applied to the macro's
  arguments and the returned models from seqex are the results."
    {:seqex seqex}
    [& forms]
    (parse-forms
     (se/recap seqex
               (fn [[macro-name] doc [body]]
                 `(let [c-seqex# (vary-meta ~body assoc :rule '~macro-name)]
                    (defmacro ~macro-name ~@doc
                      {:seqex c-seqex#}
                      [& c-forms#]
                      (parse-forms c-seqex# c-forms#)))))
     forms)))


;; make some test expressions
(def b (rule 'b-syntax (cat 1 (rule 'bfoo (opt 2)) 3)))
(def a (rep* (rule 'top
                   (cat (rule 'sub1
                              (cat (and form
                                        form*)
                                   b))
                        (or (rule 'sub2
                                  (opt 3
                                       b))
                            b)))))
