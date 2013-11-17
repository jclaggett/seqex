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
    (se/cap keyword? #(-> % first name symbol)) ;; capture and convert keywords
    (se/subex (se/recap (delay to-symbols) list)))) ;; descend through tree

(def flatten-lists
  "Flatten any lists in tree."
  (let [descend (se/subex (delay flatten-lists))] ;; descend through tree
    (se/qty* ;; zero or more
      (se/cap keyword? first) ;; capture keywords
      (se/and list? descend) ;; flatten lists
      (se/recap descend list)))) ;; descend into non-lists too

(assert (= '(a (b c) (d (e) f g) h)
           (se/parse to-symbols sample-tree)))
(assert (= '(:a :b :c (:d :e :f :g) :h)
           (se/parse flatten-lists sample-tree)))
