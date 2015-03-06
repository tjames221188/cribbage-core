(ns cribbage-core.game
  (:require [cribbage-core.cards.pure :as p]
            [cribbage-core.cards.values :refer [crib ace-low]]))




(defn deal
  [{:keys [num-players] :as state}]
  (let [cards (shuffle (p/new-deck crib ace-low))
        cards-each (if (= 2 num-players) 6 5)
        three-handed? (= 3 num-players)
        num-cards-to-deal (let [n (* cards-each num-players)]
                            (if three-handed? (inc n) n))
        to-deal (take num-cards-to-deal cards)
        deck (drop num-cards-to-deal cards)]
    (-> (merge state
               {:deck  deck
                :hands (p/deal-equal num-players to-deal)})
        (assoc :box (if three-handed? [(last to-deal)] [])))))



(defn next-hand
  [{:keys [num-players dealer hand-number] :as state}]
  (-> (assoc state :hand-number (inc hand-number)
                   :turn-number 0)
      deal))

(defn new-game
  [num-players]
  (let [new-state {:dealer         0
                   :running-scores (vec (repeat num-players 0))
                   :num-players num-players
                   :hand-number 0
                   :turn-number 0}]
    (deal new-state)))

(defn new-game!
  [state num-players]
  (reset! state (new-game num-players)))

; TODO this does not work for 2 players as they need to throw 2 cards into the box
(defn populate-box
  [indices {:keys [hands box] :as state}]
  (let [new-hands (map p/drop-nth indices hands)
        new-box (concat box (map nth hands indices))]
    (assoc state :hands new-hands
                 :box new-box)))

(defn populate-box!
  [indices state]
  (swap! state (partial populate-box indices)))

(defn add-to-score
  [{:keys [running-scores] :as state} player amount]
  (let [current-score (get running-scores player)
        new-scores (assoc running-scores player (+ amount current-score))]
    (assoc state :running-scores new-scores)))

(defn two-for-his-heels?
  [state]
  (= (:rank (:turn-card state)) :jack))

(defn turn-card
  [{:keys [deck dealer] :as state}]
  (let [n (rand-int (count deck))
        card (nth deck n)
        new-deck (p/drop-nth n deck)
        new-state (assoc state :deck new-deck
                               :turn-card card)]
    (if (two-for-his-heels? new-state)
      (add-to-score state dealer 2)
      new-state)))

(defn turn-card!
  [state]
  (swap! state turn-card))

