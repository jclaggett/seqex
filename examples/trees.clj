(ns n01se.seqex.examples.trees)
(require '[n01se.seqex :as se])

#_(def any se/n1)

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
  (se/qty*
    (se/cap keyword?
            (comp symbol name first))
    (se/subex (delay to-symbols))))

(def flatten-lists
  (se/qty*
    (se/cap keyword? first)
    (se/and list?
            (se/subex (delay flatten-lists)))
    (se/and vector?
            (se/recap (se/subex (delay flatten-lists))
                      first))))

