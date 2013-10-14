(ns n01se.negset
  (:require [clojure.set :as s])
  (:refer-clojure :exclude [complement]))

(use 'clojure.repl)

(defprotocol Complement
  (complement [s] "Return the complement of s (U \\ s)"))

(defprotocol Negative "Something not positive?" )

(deftype NegSet [^clojure.lang.IPersistentSet non-elems]
  clojure.lang.IPersistentSet
  (contains [_ e] (not (contains? non-elems e)))
  (disjoin [_ e] (NegSet. (conj non-elems e)))
  (cons [_ e] (NegSet. (disj non-elems e)))

  (seq [_] (throw "Unable to seq a negative set"))
  (count [_] (throw "Unable to count a negative set"))

  Complement
  (complement [_] non-elems)

  Negative)

(defmethod print-method NegSet [x w]
  (binding [*out* w]
    (print (apply str (concat ["#-{"]
                              (interpose " " (.non-elems x))
                              ["}"])))))

(extend-type clojure.lang.IPersistentSet
  Complement
  (complement [s] (NegSet. s)))

(defn separate [pred coll]
  ((juxt remove filter) pred coll))

(defn separate-pos-neg [sets]
  (separate (partial satisfies? Negative) sets))

(defn union [& sets]
  (let [[pos neg :as both] (separate-pos-neg sets)]
    (case (map empty? both)
      [false false] (complement (s/difference
                                  (apply s/intersection (map complement neg))
                                  (apply s/union pos)))
      [false true ] (apply s/union pos)
      [true  false] (complement (apply s/intersection (map complement neg)))
      [true  true ] nil))
  )

(defn intersection [& sets]
  (let [[pos neg :as both] (separate-pos-neg sets)]
    (case (map empty? both)
      [false false] (s/difference
                      (apply s/intersection pos)
                      (apply s/union (map complement neg)))
      [false true ] (apply s/intersection pos)
      [true  false] (complement (apply s/union (map complement neg)))
      [true  true ] nil)))

(defn difference [a & sets]
  )
(defn subset? [a b])
(defn superset? [a b])
