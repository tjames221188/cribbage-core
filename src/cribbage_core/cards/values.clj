(ns cribbage-core.cards.values)


(def ace-low
  {:ace     1
   :two     2
   :three   3
   :four    4
   :five    5
   :six     6
   :seven   7
   :eight   8
   :nine    9
   :ten     10
   :jack    11
   :queen   12
   :king    13})

(def crib
  (merge ace-low
         {:jack 10
          :queen 10
          :king 10}))

(def ace-high
  (assoc ace-low :ace 14))