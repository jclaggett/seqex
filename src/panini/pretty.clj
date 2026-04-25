(ns panini.pretty
  "Render Panini definitions as human-facing grammar."
  (:require [clojure.string :as str]))

(def ^:private ansi-reset "\u001b[0m")
(def ^:private ansi-green "\u001b[32m")
(def ^:private ansi-cyan "\u001b[36m")
(def ^:private ansi-blue "\u001b[34m")
(def ^:private ansi-gray "\u001b[90m")

(defn- colorize [ansi s]
  (str ansi s ansi-reset))

(defn- unqualify-name [x]
  (cond
    (keyword? x) (name x)
    (symbol? x)  (name x)
    :else        (str x)))

(defn- spec-op [form]
  (when (seq? form)
    (some-> form first unqualify-name symbol)))

(defn- spec-op= [form op]
  (= (spec-op form) op))

(declare render-grammar)

(def ^:private predicate-labels
  {'any?            "form"
   'some?           "form"
   'symbol?         "symbol"
   'simple-symbol?  "symbol"
   'string?         "string"
   'keyword?        "keyword"
   'simple-keyword? "keyword"
   'vector?         "vector"
   'list?           "list"
   'seq?            "seq"
   'map?            "map"
   'set?            "set"
   'boolean?        "boolean"
   'nil?            "nil"
   'int?            "int"
   'integer?        "int"
   'number?         "number"})

(defn- rule-label [find-definition x]
  (some-> (find-definition x) :symbol name))

(defn- render-keyword [find-definition x]
  (if (namespace x)
    (colorize ansi-green (or (rule-label find-definition x)
                             (name x)))
    (colorize ansi-green (str x))))

(defn- render-symbol [find-definition x]
  (colorize ansi-cyan
            (or (get predicate-labels x)
                (rule-label find-definition x)
                (name x))))

(defn- render-set [find-definition xs]
  (let [rendered (map #(render-grammar find-definition %) (sort-by pr-str xs))]
    (if (= 1 (count xs))
      (first rendered)
      (str "(" (str/join " | " rendered) ")"))))

(defn- grouped [rendered]
  (if (or (str/includes? rendered " ")
          (str/includes? rendered " | "))
    (str "(" rendered ")")
    rendered))

(defn- render-cat [find-definition args]
  (->> (partition 2 args)
       (map (fn [[_label form]]
              (render-grammar find-definition form)))
       (str/join " ")))

(defn- render-alt [find-definition args]
  (->> (partition 2 args)
       (map (fn [[_label form]]
              (render-grammar find-definition form)))
       (str/join (colorize ansi-gray " | "))))

(defn- render-and [find-definition args]
  (let [[first-arg second-arg & rest-args] args]
    (cond
      (and (= 2 (count args))
           (= 'vector? first-arg)
           (spec-op= second-arg 'spec))
      (str "[" (render-grammar find-definition (second second-arg)) "]")

      (and (= 2 (count args))
           (= 'list? first-arg)
           (spec-op= second-arg 'spec))
      (str "(" (render-grammar find-definition (second second-arg)) ")")

      (and (= 2 (count args))
           (= 'map? first-arg)
           (spec-op= second-arg 'spec))
      (str "{" (render-grammar find-definition (second second-arg)) "}")

      (and (= 2 (count args))
           (= 'map? first-arg)
           (spec-op= second-arg 'map-of))
      (render-grammar find-definition second-arg)

      (and (contains? predicate-labels first-arg)
           (seq (cons second-arg rest-args)))
      (render-grammar find-definition first-arg)

      :else
      (str/join " & " (map #(render-grammar find-definition %)
                           (cons first-arg (cons second-arg rest-args)))))))

(defn- render-grammar [find-definition form]
  (cond
    (keyword? form)
    (render-keyword find-definition form)

    (symbol? form)
    (render-symbol find-definition form)

    (set? form)
    (render-set find-definition form)

    (seq? form)
    (case (spec-op form)
      quote (render-grammar find-definition (second form))
      cat (render-cat find-definition (rest form))
      alt (render-alt find-definition (rest form))
      or  (render-alt find-definition (rest form))
      ?   (str (grouped (render-grammar find-definition (second form))) "?")
      *   (str (grouped (render-grammar find-definition (second form))) "*")
      +   (str (grouped (render-grammar find-definition (second form))) "+")
      and (render-and find-definition (rest form))
      map-of (str "{" (render-grammar find-definition (second form))
                  " "
                  (render-grammar find-definition (nth form 2))
                  "}*")
      spec (render-grammar find-definition (second form))
      tuple (str "[" (str/join " " (map #(render-grammar find-definition %)
                                         (rest form))) "]")
      nilable (str (grouped (render-grammar find-definition (second form))) "?")
      fn* (colorize ansi-cyan "predicate")
      (pr-str form))

    :else
    (pr-str form)))

(defn- walk-grammar [form]
  (tree-seq
   (fn [x]
     (or (seq? x)
         (vector? x)
         (map? x)
         (set? x)))
   (fn [x]
     (cond
       (map? x)     (mapcat identity x)
       (set? x)     (sort-by pr-str x)
       (seqable? x) x
       :else        nil))
   form))

(defn- grammar-references [find-definition form]
  (->> (walk-grammar form)
       (keep find-definition)
       (distinct)))

(defn- reachable-definitions [find-definition definition]
  (let [seen (atom #{})]
    (letfn [(visit [{:keys [name spec] :as resolved}]
              (when-not (@seen name)
                (swap! seen conj name)
                (cons resolved
                      (mapcat visit (grammar-references find-definition spec)))))]
      (visit definition))))

(defn- pad-left [width s]
  (str (apply str (repeat (max 0 (- width (count s))) " ")) s))

(defn- definition-line [find-definition width {:keys [spec symbol]}]
  (let [symbol-name (name symbol)]
    (str
     (colorize ansi-green (pad-left width symbol-name))
     " "
     (colorize ansi-blue "=>")
     " "
     (render-grammar find-definition spec))))

(defn- default-doc? [{:keys [doc symbol]}]
  (contains?
   #{(str "Rule " (name symbol) ".")
     (str "Syntax " (name symbol) ".")}
   doc))

(defn- definition-doc [{:keys [doc] :as definition}]
  (when (and doc (not (default-doc? definition)))
    (colorize ansi-gray doc)))

(defn grammar-summary
  "Render a resolved Panini definition and its referenced grammar rules."
  [find-definition definition]
  (let [definitions (reachable-definitions find-definition definition)
        name-width  (apply max (map (comp count name :symbol) definitions))
        doc         (definition-doc (first definitions))]
    (str
     (str/join "\n" (map (partial definition-line find-definition name-width)
                         definitions))
     (when doc
       (str "\n" doc)))))

(defn pretty-grammar
  "Print a resolved Panini definition and its referenced grammar rules."
  [find-definition definition]
  (println (grammar-summary find-definition definition)))
