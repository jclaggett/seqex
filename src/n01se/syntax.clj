(ns n01se.syntax
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [n01se.seqex :as se]
            [n01se.seqex.util :refer [->when ->when-not]])
  (:refer-clojure :exclude [vector symbol and not or]))
(alias 'clj 'clojure.core)

;; Define the 'API' for defining clojure syntax.

;; terminals
(def form (vary-meta se/n1 assoc :terminal 'form))
(def symbol (vary-meta symbol? assoc :terminal 'symbol))
(def string (vary-meta string? assoc :terminal 'string))
(def vector (vary-meta vector? assoc :terminal 'vector))

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

(defn vform [sub-form]
  (with-meta (and vector? (se/subex sub-form))
    {:bnf :vec}))

(defn lform [sub-form]
  (with-meta (and list? (se/subex sub-form))
    {:bnf :list}))

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
    (intern *ns* (clj/symbol (str name-prefix name)) (str code-prefix i))))

(define-ansi-codes "3" "" color-names)
(define-ansi-codes "4" "bg-" color-names)
(define-ansi-codes "" "" style-names)

(defn ansi
  ([codes] (clj/symbol (str \o33 \[ (if (coll? codes) (str/join \; codes) codes) \m)))
  ([codes & text] (clj/symbol (str (ansi codes) (apply str text) (ansi reset)))))

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

(defn items-str [before between after items]
  (apply str
         (concat [before]
                 (interpose between items)
                 [after])))

(defn rule-name [seqex]
  (-> seqex meta :rule))

(defn format-rule [before between after items & [suffix]]
  (let [hl (partial ansi [bold blue])]
    (str (if (= 1 (count items))
           (first items)
           (items-str (hl before)
                      (hl between)
                      (hl after)
                      items))
         (when suffix
           (hl suffix)))))

(defn clj-format [seqex & [body?]]
  (if-let [rule-name (when-not body? (-> seqex meta :rule))]
    (ansi [green] rule-name)
    (if-let [terminal-name (-> seqex meta :terminal)]
      (ansi [bold cyan] terminal-name)
      (let [sub-rules (map clj-format (se/children seqex))]
        (case (when (instance? clojure.lang.IMeta seqex)
                (-> seqex meta :bnf))
          :or (format-rule "(" " | " ")" sub-rules)
          :and (format-rule "(" " & " ")" sub-rules)
          :cat (format-rule "" " " "" sub-rules)
          :opt (format-rule "(" " | " ")" sub-rules "?")
          :rep+ (format-rule "(" " | " ")" sub-rules "+")
          :rep* (format-rule "(" " | " ")" sub-rules "*")
          :vec (str "[" (second sub-rules) "]")
          :list (str "(" (second sub-rules) ")")
          nil (if (satisfies? se/LitEx seqex)
                (pr-str seqex)
                (if (clj/and (satisfies? se/Tree seqex)
                             (seq (se/children seqex)))
                  (items-str "" "" "" sub-rules)
                  (ansi red (pr-str seqex))))
          )))))

(defn find-rules
  "Find all rule nodes in seqex tree and return them breadth first."
  [seqex]
  (let [sub-rules (mapcat find-rules (se/children seqex))]
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

(defn format-rules
  "Create a single string containing all rules."
  [seqexes format]
  (let [format-fn (get {:clj clj-format} format clj-format)
        len-max (apply max (map #(-> % rule-name str count) (rest seqexes)))]
    (str/join "\n"
              (cons (format-fn (first seqexes) true)
                    (for [seqex (rest seqexes)]
                      (let [name (str (rule-name seqex))
                            pad (str/join "" (repeat (- len-max (count name)) " "))]
                        (str "    " pad
                             (ansi green name)
                             (ansi [bold blue] " ::= ")
                             (format-fn seqex true)      )))))))

(defn syn*
  "Print the syntax of seqex. Generally assumes that seqex is
  decorated with syntax metadata."
  [seqex & {:keys [format color]
            :or {format :clj color true}}]
  (-> seqex
    (->when (clj/and (not= :syn (-> seqex meta :bnf))
                     (clj/not (rule-name seqex)))
                (vary-meta assoc :rule 'syntax))
    find-rules
    trim-rules
    (format-rules :clj)
    println))


(defmacro syn
  [x & opts]
  `(syn* ~(if (symbol? x)
            (if-let [v (resolve x)]
              (if-let [seqex (-> v meta :seqex)]
                `(-> (var ~x) meta :seqex)
                x)
              x)
            x)
         ~@opts))

(def def-seqex (cat (se/cap symbol)
                    (se/cap (opt (rule 'doc string)))
                    (se/cap form)))

(defmacro defsyntax
  "Return a macro defined so that seqex is applied to the macro's
  arguments and the returned models from seqex are the results."
  {:seqex (rule 'defsyntax (lform (cat 'defsyntax def-seqex)))}
  [& forms]
  (parse-forms
   (se/recap def-seqex
             (fn [[macro-name] doc [body]]
               `(let [body# ~body]
                  (defmacro ~macro-name ~@doc
                    {:seqex (rule '~macro-name
                                  (lform (cat '~macro-name body#)))}
                    [& forms#]
                    (parse-forms body# forms#)))))
   forms))

(defsyntax defrule
  (se/recap def-seqex
            (fn [[rule-name] doc [body]]
              `(let [body# (rule '~rule-name ~body)]
                 (def ~rule-name ~@doc body#)))))

(defrule bindings (vform (rep* (cat form form))))
(defrule body (rep* form))
(defsyntax let2 (cat bindings body))

;; make some test expressions
(def b (rule 'b-syntax (cat 1 (rule 'bfoo (opt 2)) 3)))
(def a (rep* (rule 'top
                   (cat (rule 'sub1
                              (cat (or form
                                        form)
                                   b))
                        (or (rule 'sub2
                                  (opt 3
                                       b))
                            b)))))