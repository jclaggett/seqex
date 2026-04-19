(ns panini.core-test
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [panini.core :as panini :refer [define-rule define-syntax]]))

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
               {:name '~name
                :doc ~docstring
                :clauses '~clauses})))

(define-syntax route
  :doc "HTTP method, route path, and handler symbol."
  :grammar (s/cat :method #{:get :post :put :delete}
                  :path string?
                  :handler symbol?)
  :target (fn [{:keys [method path handler]}]
            `{:method ~method
              :segments ~(vec (remove str/blank? (str/split path #"/")))
              :handler '~handler}))

(deftest definition-registration
  (is (panini/rule? binding-pair))
  (is (panini/syntax? #'my-let))
  (is (contains? (set (map :symbol (panini/definitions)))
                 'panini.core-test/route)))

(deftest parsing-transform-and-expansion
  (is (= '(clojure.core/let [x 1 y 2] (+ x y))
         (panini/parse-with-syntax #'my-let '([x 1 y 2] (+ x y)))))
  (is (= '(def deploy {:name 'deploy
                       :doc "Ship it"
                       :clauses '((run [:build]) (run [:release]))})
         (panini/parse-with-syntax #'defcommand '(deploy "Ship it" (run [:build]) (run [:release])))))
  (is (= {:method :get
          :segments ["users" ":id"]
          :handler '(quote handle-user)}
         (panini/parse-with-syntax #'route '(:get "/users/:id" handle-user))))
  (is (true? (:macro (meta #'my-let))))
  (is (true? (:macro (meta #'defcommand))))
  (is (true? (:macro (meta #'route)))))

(deftest validation-and-errors
  (is (panini/valid-syntax? #'defcommand
                            '(deploy "Ship it" (run [:build]) (run [:release]))))
  (is (= ::panini/invalid
         (panini/parse-with-syntax #'route '(42 "/users" handle-user)
                                   {:on-error :invalid})))
  (let [message (panini/explain-syntax #'route '(42 "/users" handle-user))]
    (is (str/includes? message "Syntax did not match"))
    (is (str/includes? message "Usage:"))
    (is (str/includes? message "HTTP method, route path, and handler symbol."))))

(deftest rendered-docs
  (let [doc (panini/syntax-doc #'my-let)]
    (is (str/includes? doc "my-let =>"))
    (is (str/includes? doc "binding-pair"))
    (is (str/includes? doc "body:form+"))))

(deftest define-syntax-registers-macro-args-spec
  (is (some? (s/get-spec 'panini.core-test/defcommand))))
