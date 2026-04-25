(ns panini.core-test
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [panini.core :as panini :refer [define-rule define-syntax]]))

(defn strip-ansi [s]
  (str/replace s #"\u001b\[[0-9;]*m" ""))

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

(define-syntax defcommand
  :doc "A command name, optional docstring, and one or more clauses."
  :grammar (s/cat :name symbol?
                  :docstring (s/? string?)
                  :clauses (s/+ list?))
  :target (fn [{:keys [name docstring clauses]}]
            `(def ~name
               {:name    '~name
                :doc     ~docstring
                :clauses '~clauses})))

(define-syntax route
  :doc "HTTP method, route path, and handler symbol."
  :grammar (s/cat :method #{:get :post :put :delete}
                  :path string?
                  :handler symbol?)
  :target (fn [{:keys [method path handler]}]
            `{:method   ~method
              :segments ~(vec (remove str/blank? (str/split path #"/")))
              :handler  '~handler}))

(define-rule lookup
  :grammar (s/map-of keyword? ::binding-pair))

(define-rule optional-symbol
  :grammar (s/and symbol? #(not= % '&)))

(define-rule pattern
  :grammar symbol?)

(define-rule seq-pattern
  :grammar (s/and list?
                  (s/spec
                   (s/cat :_tag    #{:seq}
                          :content (s/and vector?
                                          (s/spec
                                           (s/cat :elements (s/* ::pattern)
                                                  :rest     (s/? (s/cat :amp #{'&}
                                                                        :rest-pattern ::pattern)))))))))

(deftest definition-registration
  (is (panini/rule? binding-pair))
  (is (panini/syntax? #'my-let))
  (is (= :panini/rule (:kind binding-pair)))
  (is (= :panini/syntax (:kind @#'route--syntax))))

(deftest parsing-transform-and-expansion
  (is (= '(clojure.core/let [x 1 y 2] (+ x y))
         (panini/compile '(my-let [x 1 y 2] (+ x y)))))
  (is (= '(def deploy {:name    'deploy
                       :doc     "Ship it"
                       :clauses '((run [:build]) (run [:release]))})
         (panini/compile '(defcommand deploy "Ship it" (run [:build]) (run [:release])))))
  (is (= {:method   :get
          :segments ["users" ":id"]
          :handler  '(quote handle-user)}
         (panini/compile '(route :get "/users/:id" handle-user))))
  (is (= '(clojure.core/let [x 1] x)
         (binding [*ns* (the-ns 'user)]
           (macroexpand-1 '(panini.core-test/my-let [x 1] x)))))
  (is (= {:node     ::my-let
          :bindings [{:name  'x 
                      :value 1}
                     {:name  'y 
                      :value 2}]
          :body     ['(+ x y)]}
         (panini/parse '(my-let [x 1 y 2] (+ x y)))))
  (is (true? (:macro (meta #'my-let))))
  (is (true? (:macro (meta #'defcommand))))
  (is (true? (:macro (meta #'route)))))

(deftest validation-and-errors
  (is (= {:node      ::defcommand
          :name      'deploy
          :docstring "Ship it"
          :clauses   ['(run [:build]) '(run [:release])]}
         (panini/parse '(defcommand deploy "Ship it" (run [:build]) (run [:release])))))
  (is (= ::panini/invalid
         (panini/parse '(route 42 "/users" handle-user))))
  (is (= ::panini/invalid
         (panini/compile '(route 42 "/users" handle-user)
                         {:on-error :invalid})))
  (try
    (panini/compile '(route 42 "/users" handle-user))
    (is false "compile should throw on invalid input")
    (catch clojure.lang.ExceptionInfo ex
      (let [message (ex-message ex)]
        (is (str/includes? message "Syntax did not match"))
        (is (str/includes? message "Usage:"))
        (is (str/includes? message "HTTP method, route path, and handler symbol."))))))

(deftest rendered-docs
  (let [doc (strip-ansi (panini/pretty-grammar #'my-let))]
    (is (str/includes? doc "my-let =>"))
    (is (str/includes? doc "binding-pair =>"))
    (is (str/includes? doc "binding-pair"))
    (is (str/includes? doc "[binding-pair*] form+"))))

(deftest rendered-spec-helpers
  (is (str/includes? (strip-ansi (panini/pretty-grammar lookup))
                     "lookup => {keyword binding-pair}*"))
  (is (str/includes? (strip-ansi (panini/pretty-grammar optional-symbol))
                     "optional-symbol => symbol"))
  (is (str/includes? (strip-ansi (panini/pretty-grammar seq-pattern))
                     "seq-pattern => (:seq [pattern* (& pattern)?])")))

(deftest invalid-syntax-usage-includes-referenced-rules
  (try
    (panini/compile '(my-let 42 (+ x y)))
    (is false "compile should throw on invalid input")
    (catch clojure.lang.ExceptionInfo ex
      (let [message (strip-ansi (ex-message ex))]
        (is (str/includes? message "Usage:"))
        (is (str/includes? message "my-let =>"))
        (is (str/includes? message "binding-pair =>"))))))

(deftest define-syntax-registers-macro-args-spec
  (is (some? (s/get-spec 'panini.core-test/defcommand))))
