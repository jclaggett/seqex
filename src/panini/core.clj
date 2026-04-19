(ns panini.core
  "Define syntax with plain clojure.spec.alpha forms, then layer on
  registration, docs, transform hooks, and friendlier error reporting."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(defonce ^:private registry*
  (atom {:by-var {}
         :by-spec {}}))

(def ^:private invalid ::invalid)
(def rule-kind :syntax/rule)
(def syntax-kind :syntax/syntax)

(declare conform-with-syntax)
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
                      (find-definition v)))
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
  (not= invalid (conform-with-syntax syntax forms)))

(defn conform-with-syntax
  "Conform forms with a syntax definition. Returns ::invalid on failure."
  [syntax forms]
  (let [{:keys [name] :as definition} (or (find-definition syntax)
                                          (macro-definition syntax))]
    (when-not definition
      (throw (ex-info "Unknown syntax definition" {:value syntax})))
    (let [conformed (s/conform name forms)]
      (if (= ::s/invalid conformed) invalid conformed))))

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
    (when (= invalid (conform-with-syntax syntax forms))
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

(defn parse-with-syntax
  "Conform forms and apply the syntax transform.

  By default, throws ex-info when the forms do not conform.
  Pass {:on-error :invalid} to return ::invalid instead."
  ([syntax forms]
   (parse-with-syntax syntax forms {}))
  ([syntax forms {:keys [on-error] :or {on-error :throw}}]
   (let [conformed (conform-with-syntax syntax forms)]
     (if (= invalid conformed)
       (if (= :invalid on-error)
         invalid
         (throw (ex-info (explain-syntax syntax forms)
                         (explain-syntax-data syntax forms))))
       (transform-with-syntax syntax conformed)))))

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
         (parse-with-syntax ~definition-symbol forms#))
       (var ~name))))

(comment
  ;; Example 1: a tiny let-like syntax macro.

  (define-rule binding-pair
    :doc "A single name/value binding."
    :grammar (s/cat :name symbol?
                    :value any?))

  (define-syntax my-let
    :doc "Bindings followed by one or more body forms."
    :grammar (s/cat :bindings (s/and vector?
                                     (s/spec (s/* ::binding-pair)))
                    :body (s/+ any?))
    :target (fn [{:keys [bindings body]}]
              `(let ~(vec (mapcat (fn [{:keys [name value]}]
                                    [name value])
                                  bindings))
                 ~@body)))

  (syntax-doc my-let)
  ;; => "my-let => bindings:[binding-pair*] body:form+\nBindings followed by one or more body forms."

  (macroexpand '(my-let [x 1 y 2] (+ x y)))
  ;; => (let [x 1 y 2] (+ x y))


  ;; Example 2: a command macro with a richer transform.

  (define-syntax defcommand
    :doc "A command name, optional docstring, and one or more clauses."
    :grammar (s/cat :name symbol?
                    :docstring (s/? string?)
                    :clauses (s/+ list?))
    :target (fn [{:keys [name docstring clauses]}]
              `(def ~name
                 {:name '~name
                  :doc ~docstring
                  :clauses '~clauses})))

  (macroexpand '(defcommand deploy "Ship it" (run [:build]) (run [:release])))
  ;; => (def deploy {:name 'deploy, :doc "Ship it", :clauses '((run [:build]) (run [:release]))})


  ;; Example 3: a routing macro that compiles into plain data.

  (define-syntax route
    :doc "HTTP method, route path, and handler symbol."
    :grammar (s/cat :method #{:get :post :put :delete}
                    :path string?
                    :handler symbol?)
    :target (fn [{:keys [method path handler]}]
              `{:method ~method
                :segments ~(vec (remove str/blank? (str/split path #"/")))
                :handler '~handler}))

  (macroexpand '(route :get "/users/:id" handle-user))
  ;; => {:method :get, :segments ["users" ":id"], :handler 'handle-user}

  (println (explain-syntax route '(42 "/users" handle-user)))
  )
