(ns n01se.syntax.repl
  (:require [n01se.seqex :refer [cap recap]]
            [n01se.syntax :refer [defterminal defrule defsyntax
                                  cat opt alt rep* rep+
                                  vec-form, map-form, map-pair, list-form
                                  rule sym form string]])
  (:refer-clojure :exclude [let defn cat]))
(alias 'clj 'clojure.core)

;; I guess it's okay to define syndoc here. I'd rather put in syntax.clj
(defsyntax syndoc
  (recap (cat (cap form first)
              (cap (rep* (cat :format keyword?)
                         (cat :color form))))
         (fn [x opts]
           `(n01se.syntax/syndoc*
              ~(if (symbol? x)
                 (if-let [v (resolve x)]
                   (if-let [seqex (-> v meta :seqex)]
                     `(-> (var ~x) meta :seqex)
                     x)
                   x)
                 x)
              ~@opts))))


;; let (and destructuring) syntax
(defterminal prepost-map map?)
(defterminal attr-map map?)
(defterminal doc-string string?)

(declare binding-form)

(defrule binding-vec
  (vec-form (cat (rep* (delay binding-form))
                 (opt (cat '& sym))
                 (opt (cat :as sym)))))

(defrule binding-map
  (map-form (rep* (map-pair (delay binding-form) form)
                  (map-pair :as sym)
                  (rule 'keys
                        (map-pair (alt :keys :strs :syms)
                                  (vec-form (rep* sym))))
                  (rule 'defaults
                        (map-pair :or
                                  (map-form (rep* (map-pair sym
                                                            form))))))))

(defrule binding-form
  (alt sym binding-vec binding-map))

(defrule binding-pair
  (cat binding-form form))

(defrule binding-pair2
  (recap (cat (cap binding-form) (cap form))
         (fn [[bound] [value]]
           {:bound bound
            :value value})))

(defsyntax let
  (cap (cat (vec-form (rep* binding-pair))
            (rep* form))
       (fn [forms] `(clj/let ~@forms))))

(defrule sig-body
  (cat binding-vec (opt prepost-map) (rep* form)))

(defterminal var-name symbol?)

(defsyntax defn
  (cap (cat var-name
            (opt doc-string)
            (opt attr-map)
            (alt sig-body
                 (rep+ (list-form sig-body))))
       (fn [forms] `(clj/defn ~@forms))))


