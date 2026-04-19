# panini

Panini is a Clojure library for defining macro syntax with
`clojure.spec.alpha`.

You write reusable grammar fragments with `define-rule`, define actual macros
with `define-syntax`, and provide a `:target` function that turns conformed
arguments into the final macro expansion.

## Status

This is an early rewrite in progress.

The current public namespace is `panini.core`.

## Quick Start

```clojure
(require '[clojure.spec.alpha :as s])
(require '[clojure.pprint :refer [pprint]])
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

Inspect the generated syntax object:

```clojure
(pprint binding-pair)
;; {:transform #object[clojure.core$identity ...]
;;  :symbol user/binding-pair
;;  :name :user/binding-pair
;;  :spec (s/cat :name symbol? :value any?)
;;  :kind :syntax/rule
;;  :doc "A single name/value binding."}
```

Render docs, conform inputs, and expand the macro:

```clojure
(panini/syntax-doc #'my-let)
;; "my-let => bindings:[binding-pair*] body:form+
;; Bindings followed by one or more body forms."

(panini/conform-with-syntax #'my-let '([x 1 y 2] (+ x y)))

(panini/parse-with-syntax #'my-let '([x 1 y 2] (+ x y)))
;; (clojure.core/let [x 1 y 2] (+ x y))

(macroexpand '(my-let [x 1 y 2] (+ x y)))
;; (clojure.core/let [x 1 y 2] (+ x y))
```

## Core Ideas

- `define-rule` registers a reusable Spec grammar fragment.
- `define-syntax` defines a macro backed by a Spec grammar.
- `:grammar` describes valid forms.
- `:target` turns conformed forms into macro expansion.
- `syntax-doc` and `explain-syntax` provide a friendlier presentation layer over
  Spec.

## Provenance

Panini began as a fork of Jonathan Claggett's `seqex`, which was distributed
under the Eclipse Public License v1.0. The original project is here:
https://github.com/jclaggett/seqex

This repository now carries forward a different library design and namespace
layout, but retains EPL licensing and acknowledges the original project as its
starting point.

## License

Copyright (C) Jonathan Claggett and contributors to `seqex`

Copyright (C) Rahul and contributors to `panini`

Distributed under the Eclipse Public License v1.0.

See `LICENSE` and `epl-v10.html`.
