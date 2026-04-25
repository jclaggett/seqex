# panini

<img src="assets/logo.png" width="270">

[![Clojars Project](https://img.shields.io/clojars/v/london.rahul/panini.svg)](https://clojars.org/london.rahul/panini)

Panini is a library for writing "illuminated macros" in Clojure.

In a Clojure Conj 2013 talk, also named ["Illuminated Macros"](https://www.youtube.com/watch?v=o75g9ZRoLaw), Chris Houser and Jonathan Claggett argued that macros were essentially tiny languages/compilers, and as such, should:
- expose their grammar tangibly, ideally as BNF notation, allowing users to visually grasp how to use the macro
- produce clear feedback detailing the nature of a syntax mistake, exactly where it occurred, and examples of what valid input looks like
- have access to tooling to build these features natively so users don't have to write custom checkers by hand

They built the library `seqex` around that idea. I used it to power my pattern matching library `akar`. Unfortunately, `seqex` never really caught on, and there have been no commits there since 2018. So I decided to fork the project, modernise it, and maintain it. Really, the only real "modernisation" I did was replace the custom sequence expressions utilities with `clojure.spec.alpha`.

This fork is named after Pāṇini (\[päː.ɳi.n̪i]), the ancient Sanskrit grammarian whose Aṣṭādhyāyī (\[ɐʂʈaːd̪.d̪ʱjaːjiː]) is one of the earliest known formal systems for describing grammar, and is often compared to later notations such as BNF and EBNF.

## Example

```clojure
(require '[clojure.spec.alpha :as s])
(require '[panini.core :as panini :refer [define-rule define-syntax]])

(define-rule binding-pair
  "A single name/value binding."
  (s/cat :name symbol?
         :value any?))

(define-syntax my-let
  "Bindings followed by one or more body forms."
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

(panini/pretty-grammar #'my-let)
;; prints a colourized grammar summary in the terminal
;;       my-let => [binding-pair*] form+
;; binding-pair => symbol form
;; Bindings followed by one or more body forms.
```

Parse and compile a form:

```clojure
(panini/parse '(my-let [x 1 y 2] (+ x y)))
;; {:node :user/my-let,
;;  :bindings [{:name x, :value 1}
;;             {:name y, :value 2}],
;;  :body [(+ x y)]}

(panini/compile '(my-let [x 1 y 2] (+ x y)))
;; (clojure.core/let [x 1 y 2] (+ x y))

(macroexpand '(my-let [x 1 y 2] (+ x y)))
;; (clojure.core/let [x 1 y 2] (+ x y))
```

Invalid input returns `::panini/invalid` from `parse`, and either throws or returns `::panini/invalid` from `compile`:

```clojure
(panini/parse '(my-let 42 (+ x y)))
;; :panini.core/invalid

(panini/compile '(my-let 42 (+ x y)) {:on-error :invalid})
;; :panini.core/invalid
```

## License

Copyright (C) 2013 Jonathan Claggett

Copyright (C) 2026 Rahul Goma Phulore

Distributed under the Eclipse Public License v1.0. See `LICENSE` and `epl-v10.html`.
