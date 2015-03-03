(ns cribbage-core.core
  (:require [cribbage-core.cards.pure :as p]
            [cribbage-core.cards.values :refer [ace-low crib]]
            [cribbage-core.game :as g]))

(def game-state
  (atom {}))







(comment

  "Example Game State"
  (def game-state
    (atom {}))
  (new-game! game-state 2)

  )

