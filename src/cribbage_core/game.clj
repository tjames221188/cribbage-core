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
    (assoc state :deck deck
                 :hands (p/deal-equal num-players to-deal)
                 :box (if three-handed? [(last to-deal)] [])
                 :played-cards (repeat num-players [])
                 :dead-cards (repeat num-players []))))



(defn next-hand
  [{:keys [hand-number] :as state}]
  (-> (assoc state :hand-number (inc hand-number)
                   :turn-number 0
                   ; TODO - next dealer
                   )
      deal))

(defn next-hand!
  [state]
  (swap! state next-hand))

(defn new-game
  [num-players]
  (let [new-state {:dealer          0
                   :running-scores  (vec (repeat num-players 0))
                   :num-players     num-players
                   :hand-number     0
                   :turn-number     0}]
    (deal new-state)))

(defn new-game!
  [state num-players]
  (reset! state (new-game num-players)))

(defn discard-box-cards
  [hand indices]
  (let [discarded (map (partial get hand) indices)
        remaining (remove (set discarded) hand)]
    (vector discarded remaining)))

(defn populate-box
  [indices {:keys [hands box] :as state}]
  (let [x (map discard-box-cards hands indices)
        new-box (mapcat first x)
        new-hands (map second x)]
    (assoc state :hands new-hands
                 :box (concat box new-box))))

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
        new-deck (p/drop-nth deck n)
        new-state (assoc state :deck new-deck
                               :turn-card card)]
    (if (two-for-his-heels? new-state)
      (add-to-score state dealer 2)
      new-state)))

(defn turn-card!
  [state]
  (swap! state turn-card))

(defn last-card-score
  [{:keys [played-cards hands] :as state}]
  (cond (p/thirty-one? played-cards) 2
        (p/no-moves-remaining? state) 1
        :else 0))

(defn peg-pair-score
  [{:keys [played-cards]}]
  )

(defn peg-run-score
  [{:keys [played-cards]}]
  )

(defn peg-score
  [played-cards]
  (let [score 0]
    ))

(defn take-turn
  [{:keys [turn-number num-players played-cards hands] :as state} card]
  (if (p/can-go? played-cards card)
    (let [turn (p/whos-turn? turn-number num-players)
          new-hands (p/update hands turn (partial remove #{card}))
          new-played-cards (p/update played-cards turn (partial cons card))]
      )
    state))

