(ns panini.core
  "Define syntax with plain clojure.spec.alpha forms."
  (:refer-clojure :exclude [compile])
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [panini.pretty :as pretty]))

(def ^:private invalid ::invalid)
(def ^:private rule-kind :panini/rule)
(def ^:private syntax-kind :panini/syntax)

(declare pretty-grammar)

(defn- parse-definition-args [args]
  (when (odd? (count args))
    (throw (IllegalArgumentException.
            (str "Expected an even number of keyword arguments, got "
                 (pr-str args)))))
  (let [opts (apply hash-map args)]
    (when-not (every? keyword? (take-nth 2 args))
      (throw (IllegalArgumentException.
              (str "Expected keyword arguments, got " (pr-str args)))))
    opts))

(defn- split-docstring [args]
  (if (string? (first args))
    [(first args) (next args)]
    [nil args]))

(defn- parse-rule-args [name args]
  (let [[doc args] (split-docstring args)]
    (when-not (= 1 (count args))
      (throw (IllegalArgumentException.
              (str "define-rule requires a single grammar form for " name))))
    {:doc doc :grammar (first args)}))

(defn- parse-syntax-args [args]
  (let [[doc args] (split-docstring args)]
    (assoc (parse-definition-args args) :doc doc)))

(defn- spec-keyword [ns-sym name-sym]
  (keyword (str ns-sym) (str name-sym)))

(defn- definition-from-var [v]
  (let [value @v]
    (cond
      (and (map? value) (:name value)) value
      :else (let [definition-ref (:syntax/definition-var (meta v))]
              (cond
                (and (map? definition-ref) (:name definition-ref)) definition-ref
                (symbol? definition-ref) (some-> definition-ref resolve definition-from-var)
                :else nil)))))

(defn- unique-definition-by-name [sym]
  (when (symbol? sym)
    (let [matches (->> (all-ns)
                       (map #(ns-resolve % sym))
                       (remove nil?)
                       (map definition-from-var)
                       (remove nil?))]
      (when (= 1 (count matches))
        (first matches)))))

(defn- find-definition [x]
  (cond
    (and (map? x) (:name x)) x
    (keyword? x) (when-let [ns-part (namespace x)]
                   (some-> (symbol ns-part (name x))
                           find-definition))
    (var? x) (definition-from-var x)
    (symbol? x) (or (when-let [v (ns-resolve *ns* x)]
                      (find-definition v))
                    (when-let [v (resolve x)]
                      (find-definition v))
                    (unique-definition-by-name x))
    :else nil))

(defn- macro-definition [x]
  (cond
    (var? x) (or (find-definition x)
                 (let [definition-ref (:syntax/definition-var (meta x))]
                   (cond
                     (and (map? definition-ref) (:name definition-ref)) definition-ref
                     (symbol? definition-ref) (some-> definition-ref resolve find-definition)
                     :else nil)))
    (symbol? x) (or (find-definition x)
                    (when-let [v (resolve x)]
                      (macro-definition v)))
    :else nil))

(defn- definition-of [x]
  (or (find-definition x)
      (macro-definition x)))

(defn rule?
  "True when x resolves to a rule definition."
  [x]
  (= rule-kind (:kind (definition-of x))))

(defn syntax?
  "True when x resolves to a syntax definition."
  [x]
  (= syntax-kind (:kind (definition-of x))))

(defn grammar
  "Return the quoted grammar originally used to define a rule or syntax."
  [definition]
  (let [resolved (definition-of definition)]
    (when-not resolved
      (throw (ex-info "Unknown syntax definition" {:value definition})))
    (:spec resolved)))

(defn- parse-forms [syntax forms]
  (let [{:keys [name] :as definition} (definition-of syntax)]
    (when-not definition
      (throw (ex-info "Unknown syntax definition" {:value syntax})))
    (let [conformed (s/conform name forms)]
      (if (= ::s/invalid conformed) invalid conformed))))

(defn- split-call-form [form]
  (when-not (seq? form)
    (throw (ex-info "Expected a non-empty list form" {:value form})))
  (let [head (first form)
        args (rest form)]
    (when-not (symbol? head)
      (throw (ex-info "Expected a symbol in call position" {:value form})))
    [head args]))

(defn parse
  "Parse a syntax call form. Returns ::invalid on failure."
  [form]
  (let [[head args]    (split-call-form form)
        {:keys [name]} (definition-of head)
        conformed      (parse-forms head args)]
    (if (= invalid conformed)
      invalid
      (cond-> conformed (map? conformed) (assoc :node name)))))

(defn pretty-grammar
  "Print a rule or syntax definition and its referenced grammar rules."
  [definition]
  (let [resolved (definition-of definition)]
    (when-not resolved
      (throw (ex-info "Unknown syntax definition" {:value definition})))
    (pretty/pretty-grammar find-definition resolved)))

(defn- explain-data [syntax forms]
  (let [{:keys [doc name symbol]
         :as   definition}       (definition-of syntax)]
    (when-not definition
      (throw (ex-info "Unknown syntax definition" {:value syntax})))
    (when (= invalid (parse-forms syntax forms))
      {:syntax       symbol
       :name         name
       :doc          doc
       :usage        (pretty/grammar-summary find-definition definition)
       :forms        forms
       :explain-data (s/explain-data name forms)})))

(defn- indent-lines [s]
  (->> (str/split-lines s)
       (map #(str "  " %))
       (str/join "\n")))

(defn- explain-message [syntax forms]
  (let [{:keys [doc symbol name]
         :as   definition}       (definition-of syntax)]
    (when-not definition
      (throw (ex-info "Unknown syntax definition" {:value syntax})))
    (if-let [data (explain-data syntax forms)]
      (str
       "Syntax did not match " symbol
       (when doc (str "\n\n" doc))
       "\n\nUsage:\n" (indent-lines (:usage data))
       "\n\nSpec explain:\n"
       (with-out-str (s/explain name forms)))
      (str "Syntax matched " symbol "."))))

(defn- compile-forms
  ([syntax forms]
   (compile-forms syntax forms {}))
  ([syntax forms {:keys [on-error]
                  :or   {on-error :throw}}]
   (let [conformed  (parse-forms syntax forms)
         definition (definition-of syntax)]
     (if (= invalid conformed)
       (if (= :invalid on-error)
         invalid
         (throw (ex-info (explain-message syntax forms)
                         (explain-data syntax forms))))
       ((or (:transform definition) identity) conformed)))))

(defn compile
  "Compile a syntax call form.

  By default, invalid input throws ex-info. Pass {:on-error :invalid}
  to return ::invalid instead."
  ([form]
   (compile form {}))
  ([form opts]
   (let [[head args] (split-call-form form)
         result      (compile-forms head args opts)]
     (if (= invalid result)
       invalid
       result))))

(defmacro define-rule
  "Define a named grammar fragment in plain Spec."
  [name & args]
  (let [{:keys [doc grammar]} (parse-rule-args name args)
        definition-name       (spec-keyword (ns-name *ns*) name)
        definition-symbol     (symbol (str (ns-name *ns*)) (str name))]
    `(do
       (s/def ~definition-name ~grammar)
       (def ~name
         {:kind      ~rule-kind
          :symbol    '~definition-symbol
          :name      ~definition-name
          :spec      '~grammar
          :doc       ~(or doc (str "Rule " name "."))
          :transform identity})
       (var ~name))))

(defmacro define-syntax
  "Define a macro backed by a Spec grammar."
  [name & args]
  (let [{:keys [doc grammar target]} (parse-syntax-args args)
        definition-name              (spec-keyword (ns-name *ns*) name)
        macro-symbol                 (symbol (str (ns-name *ns*)) (str name))
        definition-symbol            (symbol (str name "--syntax"))
        transform                    (or target identity)]
    (when-not grammar
      (throw (IllegalArgumentException.
              (str "define-syntax requires :grammar for " name))))
    `(do
       (s/def ~definition-name ~grammar)
       (def ~definition-symbol
         {:kind              ~syntax-kind
          :symbol            '~macro-symbol
          :definition-symbol '~(symbol (str (ns-name *ns*)) (str definition-symbol))
          :name              ~definition-name
          :spec              '~grammar
          :doc               ~(or doc (str "Syntax " name "."))
          :transform         ~transform})
       (s/fdef ~name :args ~definition-name)
       (defmacro ~(with-meta name {:syntax/definition-var (symbol (str (ns-name *ns*)) (str definition-symbol))})
         [& forms#]
         (compile (list* '~macro-symbol forms#)))
       (var ~name))))
