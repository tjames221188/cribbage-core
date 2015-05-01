(ns cribbage-core.core-test
  (:require [clojure.test :refer :all]
            [cribbage-core.core :refer :all]
            [cribbage-core.game :as g]
            [cribbage-core.cards.pure :as p]))

(defn cards-in-play
  [state]
  (let [cards (concat (:deck state)
                (apply concat (:hands state))
                (apply concat (:played-cards state))
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
    "Make sure no cards are lost throughout the flow of the game"
    (let [state (atom nil)]
      "Initial state"
      (g/new-game! state 3)
      (is (valid-deck? @state))
      "Select box cards"
      (g/populate-box! [[0] [1] [2]] state)
      (is (valid-deck? @state))
      "Turn over the middle card"
      (g/turn-card! state)
      (is (valid-deck? @state)))))

(deftest test-box
  "Make sure populate-box works for all numbers of players"
  (let [two-players   (g/new-game 2)
        three-players (g/new-game 3)
        four-players  (g/new-game 4)]
    (is (and (valid-deck? (g/populate-box [[0 1] [2 3]] two-players))
             (valid-deck? (g/populate-box [[0] [1] [2]] three-players))
             (valid-deck? (g/populate-box [[0] [1] [2] [3]] four-players))))))

(deftest test-in-played-order
  (let [played-cards4 [[{:index 1} {:index 5}]
                      [{:index 2} {:index 6}]
                      [{:index 3} {:index 7}]
                      [{:index 4} {:index 8}]]
        played-cards3 [[{:index 1} {:index 4} {:index 7}]
                       [{:index 2} {:index 5} {:index 8}]
                       [{:index 3} {:index 6}]]
        played-cards2 [[{:index 1} {:index 3} {:index 5} {:index 7}]
                       [{:index 2} {:index 4} {:index 6} {:index 8}]]]
    (is (= [1 2 3 4 5 6 7 8]
           (map :index (g/in-played-order played-cards4 3))
           (map :index (g/in-played-order played-cards3 2))
           (map :index (g/in-played-order played-cards2 1))))))

(deftest test-peg-pair-score
  "Make sure pegging pairs/triples are awarded correctly"
  (let [pair    [[{:index 1} {:index 4}]
                 [{:index 2} {:index 4}]
                 [{:index 3}]]
        triple  [[{:index 1} {:index 4}]
                 [{:index 2} {:index 4}]
                 [{:index 4}]]
        quad    [[{:index 1} {:index 4}]
                 [{:index 4} {:index 4}]
                 [{:index 4}]]]
    (is (= 2 (g/peg-pair-score {:played-cards pair :dealer 2})))
    (is (= 6 (g/peg-pair-score {:played-cards triple :dealer 2})))
    (is (= 12 (g/peg-pair-score {:played-cards quad :dealer 2})))))

(deftest test-peg-run-score
  "Make sure that pegging runs are awarded correctly"
  (let [played-cards1 [[{:index 9} {:index 3}]
                       [{:index 2}]
                       [{:index 4}]]]
    (is (= 3 (g/peg-run-score {:played-cards played-cards1 :dealer 2}))))
  )


