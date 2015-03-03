(ns cribbage-core.game
  (:require [cribbage-core.cards.pure :as p]
            [cribbage-core.cards.values :refer [crib ace-low]]))


(defn deal-equal
  "Deals an equal number of cards to each player.
  Any remainder cards are dropped"
  [num-players cards]
  (apply map vector (partition num-players cards)))



(defn new-game
  [num-players]
  (let [cards (shuffle (p/new-deck crib ace-low))
        cards-each (if (= 2 num-players) 5 4)
        three-handed? (= 3 num-players)
        num-cards-to-deal (let [n (* cards-each num-players)]
                            (if three-handed? (inc n) n))
        to-deal (take num-cards-to-deal cards)
        deck (drop num-cards-to-deal cards)
        new-state {:deck deck
                   :hands (deal-equal num-players to-deal)}]
    (assoc new-state :box (if three-handed? [(last to-deal)] []))))

(defn new-game!
  [state num-players]
  (reset! state (new-game num-players)))

(defn populate-box
  [indices {:keys [hands box] :as state}]
  (let [new-hands (map p/drop-nth indices hands)
        new-box (concat box (map nth hands indices))]
    (assoc state :hands new-hands
                 :box new-box)))

(defn populate-box!
  [indices state]
  (swap! state (partial populate-box indices)))

(defn turn-card
  [{:keys [deck] :as state}]
  (let [n 10 #_(rand (count deck))
        card (nth deck n)
        new-deck (p/drop-nth n deck)]
    (assoc state :deck new-deck
                 :turn-card card)))

(defn turn-card!
  [state]
  (swap! state turn-card))

