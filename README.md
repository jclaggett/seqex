# panini

Panini is a small Clojure library for defining macro syntax with
`clojure.spec.alpha`.

It comes out of Chris Houser and Jonathan Claggett's talk
["Illuminated Macros"](https://www.youtube.com/watch?v=o75g9ZRoLaw).

That talk was published on 2014.01.07, and it still feels right to me. Macros
should not be mysterious. You should be able to see the grammar, get useful
error messages, and understand what shape of input a macro expects.

Jonathan Claggett built `seqex` around that idea. I built my own small pattern
matching library, Akar, on top of it. `seqex` never really caught on, though,
and there have been no commits there since 2018. Panini is my attempt to keep
the same basic idea alive.

The main difference is technical. `seqex` had to provide its own structural
parser for Lisp forms. Panini does not. We have `clojure.spec.alpha` now, so
Panini is mostly a thin layer over Spec for defining syntax, showing grammar,
and compiling forms.

The public namespace is `panini.core`.

## Example

```clojure
(require '[clojure.spec.alpha :as s])
(require '[panini.core :as panini :refer [define-rule define-syntax]])

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
```

Inspect the grammar:

```clojure
(panini/rule? binding-pair)
;; true

(panini/syntax? #'my-let)
;; true

(panini/grammar #'my-let)
;; (s/cat :bindings (s/and vector? (s/spec (s/* :user/binding-pair)))
;;        :body (s/+ any?))

(println (panini/pretty-grammar #'my-let))
;; prints a colourized grammar summary in the terminal
;; my-let => bindings:[binding-pair*] body:form+
;; Bindings followed by one or more body forms.
```

Parse and compile a form:

```clojure
(panini/parse '(my-let [x 1 y 2] (+ x y)))
;; {:bindings [{:name x, :value 1}
;;             {:name y, :value 2}],
;;  :body [(+ x y)]}

(panini/compile '(my-let [x 1 y 2] (+ x y)))
;; (clojure.core/let [x 1 y 2] (+ x y))

(macroexpand '(my-let [x 1 y 2] (+ x y)))
;; (clojure.core/let [x 1 y 2] (+ x y))
```

Invalid input returns `::panini/invalid` from `parse`, and either throws or
returns `::panini/invalid` from `compile`:

```clojure
(panini/parse '(my-let 42 (+ x y)))
;; :panini.core/invalid

(panini/compile '(my-let 42 (+ x y)) {:on-error :invalid})
;; :panini.core/invalid
```

## License

Copyright (C) 2013 Jonathan Claggett

Copyright (C) 2026 Rahul Goma Phulore

Distributed under the Eclipse Public License v1.0. See `LICENSE` and
`epl-v10.html`.
