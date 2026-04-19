(ns panini.core
  "Define syntax with plain clojure.spec.alpha forms, then layer on
  registration, docs, transform hooks, and friendlier error reporting."
  (:refer-clojure :exclude [compile])
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(defonce ^:private registry*
  (atom {:by-var {}
         :by-spec {}}))

(def ^:private invalid ::invalid)
(def rule-kind :syntax/rule)
(def syntax-kind :syntax/syntax)

(declare parse)
(declare parse-forms)
(declare compile-forms)
(declare syntax-doc)

(defn- unqualify-name [x]
  (cond
    (keyword? x) (name x)
    (symbol? x) (name x)
    :else (str x)))

(defn- spec-op [form]
  (when (seq? form)
    (some-> form first unqualify-name symbol)))

(defn- spec-op= [form op]
  (= (spec-op form) op))

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

(defn- spec-keyword [ns-sym name-sym]
  (keyword (str ns-sym) (str name-sym)))

(defn register-definition! [definition]
  (swap! registry*
         (fn [registry]
           (-> registry
               (assoc-in [:by-var (:symbol definition)] definition)
               (assoc-in [:by-spec (:name definition)] definition))))
  definition)

(defn definitions
  "Return every registered rule and syntax definition."
  []
  (->> @registry* :by-var vals (sort-by :symbol)))

(defn- unique-definition-by-name [sym]
  (when (symbol? sym)
    (let [matches (->> @registry* :by-var vals
                       (filter #(= (name (:symbol %)) (name sym))))]
      (when (= 1 (count matches))
        (first matches)))))

(defn find-definition
  "Resolve a definition from a var symbol, spec keyword, var, or definition map."
  [x]
  (cond
    (and (map? x) (:name x)) x
    (keyword? x) (get-in @registry* [:by-spec x])
    (var? x) (let [value @x]
               (when (and (map? value) (:name value))
                 value))
    (symbol? x) (or (get-in @registry* [:by-var x])
                    (when-let [v (resolve x)]
                      (find-definition v))
                    (unique-definition-by-name x))
    :else nil))

(defn macro-definition
  "Resolve the registered syntax definition for a macro symbol or var."
  [x]
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

(defn syntax?
  "True when x resolves to a syntax definition."
  [x]
  (= syntax-kind (:kind (or (find-definition x)
                            (macro-definition x)))))

(defn rule?
  "True when x resolves to a rule definition."
  [x]
  (= rule-kind (:kind (or (find-definition x)
                          (macro-definition x)))))

(defn valid-syntax?
  "Return true when forms conform to the syntax definition."
  [syntax forms]
  (not= invalid (parse-forms syntax forms)))

(defn- parse-forms
  "Parse forms with a syntax definition. Returns ::invalid on failure."
  [syntax forms]
  (let [{:keys [name] :as definition} (or (find-definition syntax)
                                          (macro-definition syntax))]
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
  "Parse a syntax call form. Returns ::invalid on failure.

  Example:
    (parse '(my-let [x 1 y 2] (+ x y)))"
  [form]
  (let [[head args] (split-call-form form)]
    (parse-forms head args)))

(defn transform-with-syntax
  "Apply the syntax transform to conformed data."
  [syntax conformed]
  (let [{:keys [transform] :as definition} (or (find-definition syntax)
                                               (macro-definition syntax))]
    (when-not definition
      (throw (ex-info "Unknown syntax definition" {:value syntax})))
    ((or transform identity) conformed)))

(defn explain-syntax-data
  "Return structured error data for forms that do not conform."
  [syntax forms]
  (let [{:keys [doc name symbol] :as definition} (or (find-definition syntax)
                                                     (macro-definition syntax))]
    (when-not definition
      (throw (ex-info "Unknown syntax definition" {:value syntax})))
    (when (= invalid (parse-forms syntax forms))
      {:syntax symbol
       :name name
       :doc doc
       :usage (syntax-doc syntax)
       :forms forms
       :explain-data (s/explain-data name forms)})))

(defn explain-syntax
  "Return a friendly explain string."
  [syntax forms]
  (let [{:keys [doc symbol name] :as definition} (or (find-definition syntax)
                                                     (macro-definition syntax))]
    (when-not definition
      (throw (ex-info "Unknown syntax definition" {:value syntax})))
    (if-let [data (explain-syntax-data syntax forms)]
      (str
       "Syntax did not match " symbol
       (when doc (str "\n\n" doc))
       "\n\nUsage:\n  " (:usage data)
       "\n\nSpec explain:\n"
       (with-out-str (s/explain name forms)))
      (str "Syntax matched " symbol "."))))

(defn compile-forms
  "Internal helper used by `define-syntax` macros.

  Parse forms and apply the syntax transform.

  By default, throws ex-info when the forms do not conform.
  Pass {:on-error :invalid} to return ::invalid instead."
  ([syntax forms]
   (compile-forms syntax forms {}))
  ([syntax forms {:keys [on-error] :or {on-error :throw}}]
   (let [conformed (parse-forms syntax forms)]
     (if (= invalid conformed)
       (if (= :invalid on-error)
         invalid
         (throw (ex-info (explain-syntax syntax forms)
                         (explain-syntax-data syntax forms))))
       (transform-with-syntax syntax conformed)))))

(defn compile
  "Compile a syntax call form by parsing it and applying the syntax transform.

  Example:
    (compile '(my-let [x 1 y 2] (+ x y)))"
  ([form]
   (compile form {}))
  ([form opts]
   (let [[head args] (split-call-form form)]
     (compile-forms head args opts))))

(defn spec-form
  "Return the quoted spec form originally used to define a rule or syntax."
  [definition]
  (:spec (or (find-definition definition)
             (macro-definition definition))))

(defn- spec-ref-label [x]
  (if-let [definition (find-definition x)]
    (some-> definition :symbol name)
    (name x)))

(declare render-spec)

(def ^:private predicate-labels
  {'any? "form"
   'some? "form"
   'symbol? "symbol"
   'simple-symbol? "symbol"
   'string? "string"
   'keyword? "keyword"
   'simple-keyword? "keyword"
   'vector? "vector"
   'list? "list"
   'seq? "seq"
   'map? "map"
   'set? "set"
   'boolean? "boolean"
   'int? "int"
   'integer? "int"
   'number? "number"})

(defn- render-set [xs]
  (if (= 1 (count xs))
    (render-spec (first xs))
    (str "(" (str/join " | " (map render-spec xs)) ")")))

(defn- named-entry [label rendered]
  (if (or (nil? label)
          (= "_" (name label))
          (= (name label) rendered))
    rendered
    (str (name label) ":" rendered)))

(defn- grouped [rendered]
  (if (or (str/includes? rendered " ")
          (str/includes? rendered " | "))
    (str "(" rendered ")")
    rendered))

(defn- render-cat [args]
  (->> (partition 2 args)
       (map (fn [[label form]]
              (named-entry label (render-spec form))))
       (str/join " ")))

(defn- render-alt [args]
  (->> (partition 2 args)
       (map (fn [[label form]]
              (named-entry label (render-spec form))))
       (str/join " | ")))

(defn- render-and [args]
  (let [[first-arg second-arg & rest-args] args]
    (cond
      (and (= 2 (count args))
           (= 'vector? first-arg)
           (spec-op= second-arg 'spec))
      (str "[" (render-spec (second second-arg)) "]")

      (and (= 2 (count args))
           (= 'list? first-arg)
           (spec-op= second-arg 'spec))
      (str "(" (render-spec (second second-arg)) ")")

      (and (= 2 (count args))
           (= 'map? first-arg)
           (spec-op= second-arg 'spec))
      (str "{" (render-spec (second second-arg)) "}")

      :else
      (str/join " & " (map render-spec (cons first-arg (cons second-arg rest-args)))))))

(defn render-spec
  "Render a quoted Spec form as compact syntax documentation."
  [form]
  (cond
    (keyword? form)
    (if (namespace form)
      (spec-ref-label form)
      (str form))

    (symbol? form)
    (or (get predicate-labels form)
        (if-let [definition (find-definition form)]
          (name (:symbol definition))
          (name form)))

    (set? form)
    (render-set (sort-by pr-str form))

    (seq? form)
    (case (spec-op form)
      cat (render-cat (rest form))
      alt (render-alt (rest form))
      or  (render-alt (rest form))
      ?   (str (grouped (render-spec (second form))) "?")
      *   (str (grouped (render-spec (second form))) "*")
      +   (str (grouped (render-spec (second form))) "+")
      and (render-and (rest form))
      spec (render-spec (second form))
      tuple (str "[" (str/join " " (map render-spec (rest form))) "]")
      nilable (str (grouped (render-spec (second form))) "?")
      (pr-str form))

    :else
    (pr-str form)))

(defn syntax-doc
  "Render a rule or syntax definition as a compact usage string."
  [definition]
  (let [{:keys [doc spec symbol] :as resolved} (or (find-definition definition)
                                                   (macro-definition definition))]
    (when-not resolved
      (throw (ex-info "Unknown syntax definition" {:value definition})))
    (str
     (name symbol)
     " => "
     (render-spec spec)
     (when doc
       (str "\n" doc)))))

(defmacro define-rule
  "Define a named grammar fragment in plain Spec.

  Examples:

    (define-rule binding-pair
      :grammar (s/cat :binding symbol?
                      :value any?))

    (define-rule binding-pair
      :doc \"A single binding pair\"
      :grammar (s/cat :binding symbol?
                      :value any?))"
  [name & args]
  (let [{:keys [doc grammar] :as opts} (parse-definition-args args)
        definition-name (spec-keyword (ns-name *ns*) name)
        definition-symbol (symbol (str (ns-name *ns*)) (str name))]
    (when-not grammar
      (throw (IllegalArgumentException.
              (str "define-rule requires :grammar for " name))))
    `(do
       (s/def ~definition-name ~grammar)
       (def ~name
         (register-definition!
          {:kind rule-kind
           :symbol '~definition-symbol
           :name ~definition-name
           :spec '~grammar
           :doc ~(or doc (str "Rule " name "."))
           :transform identity}))
       (var ~name))))

(defmacro define-syntax
  "Define a macro backed by a Spec grammar.

  Options:
  `:doc`       A human-readable description.
  `:grammar`   Spec grammar for the macro arguments.
  `:target`    Function applied to conformed data to produce the macro expansion."
  [name & args]
  (let [{:keys [doc grammar target] :as opts} (parse-definition-args args)
        definition-name (spec-keyword (ns-name *ns*) name)
        macro-symbol (symbol (str (ns-name *ns*)) (str name))
        definition-symbol (symbol (str name "--syntax"))
        transform (or target identity)]
    (when-not grammar
      (throw (IllegalArgumentException.
              (str "define-syntax requires :grammar for " name))))
    `(do
       (s/def ~definition-name ~grammar)
       (def ~definition-symbol
         (register-definition!
          {:kind syntax-kind
           :symbol '~macro-symbol
           :definition-symbol '~(symbol (str (ns-name *ns*)) (str definition-symbol))
           :name ~definition-name
           :spec '~grammar
           :doc ~(or doc (str "Syntax " name "."))
           :transform ~transform}))
       (s/fdef ~name :args ~definition-name)
       (defmacro ~(with-meta name {:syntax/definition-var (symbol (str (ns-name *ns*)) (str definition-symbol))})
         [& forms#]
         (compile-forms ~definition-symbol forms#))
       (var ~name))))
