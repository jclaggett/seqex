# panini

Panini is a small Clojure library for defining macro syntax with
`clojure.spec.alpha`.

It comes out of Chris Houser and Jonathan Claggett's talk
"Illuminated Macros":

https://www.youtube.com/watch?v=o75g9ZRoLaw

That talk was published on 2014-01-07, and it still feels right to me. Macros
should not be mysterious. You should be able to see the grammar, get useful
error messages, and understand what shape of input a macro expects.

Jonathan Claggett built `seqex` around that idea. I built my own small pattern
matching library, Akar, on top of it. `seqex` never really caught on, though,
and there have been no commits there since 2018. Panini is my attempt to keep
the same basic idea alive.

The main difference is technical. `seqex` had to provide its own structural
parser for Lisp forms. Panini does not. We have `clojure.spec.alpha` now, so
Panini is mostly a thin layer over Spec for defining syntax, reusing grammar
fragments, rendering docs, and producing better syntax errors.

The public namespace is `panini.core`.

## One Example

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

That one definition is enough to show most of the intended API.

## Using It

Identify what Panini registered:

```clojure
(panini/rule? binding-pair)
;; true

(panini/syntax? #'my-let)
;; true

(panini/definitions)
;; ({:kind :syntax/rule, ...}
;;  {:kind :syntax/syntax, ...})

(panini/find-definition 'my-let)
;; {:kind :syntax/syntax, :symbol user/my-let, ...}

(panini/macro-definition #'my-let)
;; {:kind :syntax/syntax, :symbol user/my-let, ...}
```

Inspect the syntax itself:

```clojure
(panini/spec-form #'my-let)
;; (s/cat :bindings (s/and vector? (s/spec (s/* :user/binding-pair)))
;;        :body (s/+ any?))

(panini/render-spec (panini/spec-form #'my-let))
;; "bindings:[binding-pair*] body:form+"

(panini/syntax-doc #'my-let)
;; "my-let => bindings:[binding-pair*] body:form+
;; Bindings followed by one or more body forms."
```

Parse and compile a form:

```clojure
(panini/valid-syntax? #'my-let '([x 1 y 2] (+ x y)))
;; true

(panini/parse '(my-let [x 1 y 2] (+ x y)))
;; {:bindings [{:name x, :value 1}
;;             {:name y, :value 2}],
;;  :body [(+ x y)]}

(panini/transform-with-syntax
  #'my-let
  (panini/parse '(my-let [x 1 y 2] (+ x y))))
;; (clojure.core/let [x 1 y 2] (+ x y))

(panini/compile '(my-let [x 1 y 2] (+ x y)))
;; (clojure.core/let [x 1 y 2] (+ x y))

(macroexpand '(my-let [x 1 y 2] (+ x y)))
;; (clojure.core/let [x 1 y 2] (+ x y))
```

See what happens on bad input:

```clojure
(panini/valid-syntax? #'my-let '(42 (+ x y)))
;; false

(panini/explain-syntax-data #'my-let '(42 (+ x y)))
;; {:syntax user/my-let, :usage "my-let => ...", :explain-data ...}

(panini/explain-syntax #'my-let '(42 (+ x y)))
;; "Syntax did not match user/my-let
;;
;; Bindings followed by one or more body forms.
;;
;; Usage:
;;   my-let => bindings:[binding-pair*] body:form+
;; ..."
```

## API

The intended user-facing functions are:

- `define-rule`
- `define-syntax`
- `rule?`
- `syntax?`
- `definitions`
- `find-definition`
- `macro-definition`
- `spec-form`
- `render-spec`
- `syntax-doc`
- `valid-syntax?`
- `parse`
- `transform-with-syntax`
- `compile`
- `explain-syntax-data`
- `explain-syntax`

There are also a few lower-level names currently public, such as
`compile-forms`, `register-definition!`, `rule-kind`, and `syntax-kind`. Those
are not the main API and are not documented here as stable entry points.

## Provenance

Panini comes from Jonathan Claggett's `seqex`:

https://github.com/jclaggett/seqex

It is not the same implementation. It is a simpler rewrite built around
`clojure.spec.alpha`.

## License

Copyright (C) 2013 Jonathan Claggett

Copyright (C) 2026 Rahul Goma Phulore

Distributed under the Eclipse Public License v1.0. See `LICENSE` and
`epl-v10.html`.
