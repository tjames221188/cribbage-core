(ns cribbage-core.core-test
  (:require [clojure.test :refer :all]
            [cribbage-core.core :refer :all]
            [cribbage-core.game :as g]
            [cribbage-core.cards.pure :as p]))

(defn cards-in-play
  [state]
  (let [cards (concat (:deck state)
                (apply concat (:hands state))
                (:box state))]
    (if (:turn-card state)
      (conj cards (:turn-card state))
      cards)))

(defn valid-deck?
  [state]
  (= 52 (count (distinct (cards-in-play state)))))

(deftest three-handed-test
  "Make sure a card is dealt into the box when three-handed"
  (let [state (g/new-game 3)]
    (is (= 1 (count (:box state))))))

(deftest cards-maintained?
  (testing
    "Ensures no cards are lost throughout the flow of the game"
    (let [state (atom nil)]
      "Initial state"
      (g/new-game! state 3)
      (is (valid-deck? @state))
      "Select box cards"
      (g/populate-box! [0 1 2] state)
      (is (valid-deck? @state))
      "Turn over the middle card"
      (g/turn-card! state)
      (is (valid-deck? @state))))
  )


