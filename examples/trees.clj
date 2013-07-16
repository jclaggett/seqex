(ns n01se.seqex.examples.trees
  (:require [n01se.seqex :as se]))

(def sample-tree
  [ :a
   '( :b
      :c )
    [ :d
      '( :e )
      :f
      :g ]
    :h ])

(def to-symbols
  "Convert all keywords to symbols in tree."
  (se/qty* ;; zero or more
    (se/cap-one keyword? (comp symbol name)) ;; capture and convert keywords
    (se/subex (delay to-symbols)))) ;; descend through tree

(def flatten-lists
  "Flatten any lists in tree."
  (let [descend (se/subex (delay flatten-lists))] ;; descend through tree
    (se/qty* ;; zero or more
      (se/cap-one keyword?) ;; capture keywords
      (se/and list? (se/recap descend first)) ;; flatten lists
      descend))) ;; descend into non-lists too

(assert (= '(a (b c) (d (e) f g) h)
           (se/model to-symbols sample-tree)))
(assert (= '(:a :b :c (:d :e :f :g) :h)
           (se/model flatten-lists sample-tree)))
