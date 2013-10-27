(ns n01se.negset
  (:require [clojure.set :as s])
  (:refer-clojure :exclude [complement]))

(use 'clojure.repl)

(defprotocol Complement
  (complement [s] "Return the complement of s (U \\ s)"))

(defprotocol Negative "A value defined in terms of what it is not" )

(deftype NegSet [^clojure.lang.IPersistentSet non-elems]
  clojure.lang.IPersistentSet
  (contains [_ e] (not (contains? non-elems e)))
  (disjoin [_ e] (NegSet. (conj non-elems e)))
  (cons [_ e] (NegSet. (disj non-elems e)))

  (seq [_] (throw "Unable to seq a negative set (infinitely large)"))
  (count [_] (throw "Unable to count a negative set (infinitely large)"))

  (equiv [_ other] (and (instance? NegSet other)
                        (= non-elems (.non-elems other))))

  Complement
  (complement [_] non-elems)

  Negative)

#_(alter-var-root #'*data-readers* assoc 'neg ->NegSet)

(defmethod print-method NegSet [x w]
  (binding [*out* w]
    (print (apply str (concat ["#-{"]
                              (interpose " " (.non-elems x))
                              ["}"])))))

(extend-type clojure.lang.IPersistentSet
  Complement
  (complement [s] (NegSet. s)))

;; define s-* operators that only work with positive sets
;; consider them a part of the clojure.set library
(defn s-intersect?
  "Does set1 intersect set2?"
  [set1 set2]
  (or (if (< (count set1) (count set2))
        (some #(contains? set2 %) set1)
        (some #(contains? set1 %) set2))
      false))

(defn s-disjoint?
  "Is set1 disjoint from set2?"
  [set1 set2]
  (not (s-intersect? set1 set2)))

;; Convenience macro for defining the behavior of various combinations
;; of positive and negative sets.
(defmacro case-sets [s1 s2 c1 c2 c3 c4]
  `(case [(satisfies? Negative ~s1)
          (satisfies? Negative ~s2)]
     [false false] ~c1
     [false true ] ~c2
     [true  false] ~c3
     [true  true ] ~c4))

(defn intersect?
  "Is at least one element of set1 shared with set2?"
  [set1 set2]
  (case-sets set1 set2
    (s-intersect? set1 set2)
    (not (s/subset? set1 (complement set2)))
    (not (s/subset? set2 (complement set1)))
    true)) ;; always intersect on finite computers

(defn disjoint?
  "Are no elements in set1 shared with set2?"
  [set1 set2]
  (not (intersect? set1 set2)))

(defn subset? [set1 set2]
  "Are all elements in set1 also in set2?"
  (case-sets set1 set2
    (s/subset? set1 set2)
    (not (s-intersect? set1 (complement set2)))
    false ;; never a subset on finite computers
    (s/subset? (complement set2) (complement set1))))

(defn superset?
  "Are all elements in set2 also in set1?"
  [set1 set2]
  (subset? set2 set1))

(defn union
  "Return a set containing elements in all sets."
  [& sets]
  (reduce
   (fn
     ([] nil)
     ([set1 set2]
        (if (= (complement #{}) set1)
          (reduced set1)
          (case-sets set1 set2
            (s/union set1 set2)
            (complement (s/difference (complement set2) set1))
            (complement (s/difference (complement set1) set2))
            (complement (s/intersection (complement set1) (complement set2)))))))
   sets))

(defn intersection
  "Return a set containing only elements that are members of all sets."
  [& sets]
  (reduce
   (fn
     ([] nil)
     ([set1 set2]
        (if (= #{} set1)
          (reduced set1)
          (case-sets set1 set2
            (s/intersection set1 set2)
            (s/difference set1 (complement set2))
            (s/difference set2 (complement set1))
            (complement (s/union (complement set1) (complement set2)))))))
   sets))

(defn difference
  "Return a set containing elements of the first set that are not also elements
  in the following sets."
  [& sets]
  (reduce
   (fn
     ([] nil)
     ([set1 set2]
        (if (= #{} set1)
          (reduced set1)
          (case-sets set1 set2
            (s/difference set1 set2)
            (s/intersection set1 (complement set2))
            (complement (s/union (complement set1) set2))
            (s/difference (complement set2) (complement set1))))))
   sets))

;; Define other set operators for completeness
(defn symetric-difference
  "Return a set of elements that are found in set1 or in set2 but not in both."
  [set1 set2]
  (difference (union set1 set2)
              (intersection set1 set2)))

(defn proper-subset?
  [set1 set2]
  (and (not= set1 set2)
       (subset? set1 set2)))

(defn proper-superset? [set1 set2]
  (proper-subset? set2 set1))

;; Define set notation. Because Unicode.

(def ∁ complement)
(def ∪ union)
(def ∩ intersection)
(def ∖ difference)
(def ⊖ symetric-difference)
(def △ symetric-difference)
(def ∅ #{})
(def ⊆ subset?)
(def ⊇ superset?)
(def ⊂ proper-subset?)
(def ⊃ proper-superset?)
(def ∋ contains?)
(def U (∁ ∅))

