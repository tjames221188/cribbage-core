(ns cribbage-core.cards.pure
  (:require [cribbage-core.cards.values :as values]))

(def suits
  [:clubs :diamonds :hearts :spades])
(def ranks
  [:ace :two :three :four :five :six :seven
   :eight :nine :ten :jack :queen :king])

(def card-keys
  [:suit :rank :value])

(defn drop-nth
  [n coll]
  (concat (take n coll) (drop (inc n) coll)))

(defn update
  [m k f]
  (assoc m k (f (get m k))))

(defn cartesian-product [colls]
  (if (empty? colls)
    '(())
    (for [x (first colls)
          more (cartesian-product (rest colls))]
      (cons x more))))

#(flatten (reverse (split-at (mod % (count %2)) %2)))

(defn rotate
  [coll n]
  (let [pivot (mod n (count coll))]
    (->> (split-at pivot coll)
        reverse
        (apply concat))))

(defn value
  [rank-values card]
  (get rank-values (:rank card)))

(defn index
  [indices card]
  (get indices (:rank card)))

(defn new-deck
  [rank-values rank-indices]
  (->> (cartesian-product [suits ranks])
      (map (partial zipmap card-keys))
      (map #(assoc %1 :value (value rank-values %1)))
      (map #(assoc %1 :index (index rank-indices %1)))))

(defn deal-equal
  "Deals an equal number of cards to each player.
  Any remainder cards are dropped"
  [num-players cards]
  (apply map vector (partition num-players cards)))

(defn powerset [coll]
  (reduce (fn [a x]
            (->> a
                 (map #(set (concat #{x} %)))
                 (concat a)
                 set))
          #{#{}} coll))

(defn fifteen?
  [cards]
  (->> (map :value cards)
       (apply +)
       (= 15)))

(defn subset?
  "Returns true if `x` is a subset of `y`, false otherwise."
  [x y]
  (->> (map (partial contains? x) y)
       (reduce = true)))

(defn num-fifteens
  [hand]
  (->>  (powerset hand)
        (filter fifteen?)
        count))

(defn all-pairs
  [hand]
  (when-let [s (next hand)]
    (lazy-cat (for [y s] [(first hand) y])
              (all-pairs s))))

(defn matching-pair?
  [[first second]]
  (= (:rank first) (:rank second)))

(defn num-matching-pairs
  [hand]
  (->> (all-pairs hand)
      (filter matching-pair?)
      count))

(defn multiple-runs
  [occurences run]
  (let [times (->> (map occurences run)
                   (filter (partial < 1)))
        total-times (if (empty? times)
                      1
                      (reduce + times))]
    (repeat total-times run)))

(defn points-from-runs
  [min-length hand]
  (let [ignoring-suits (map #(dissoc % :suit) hand)
        unique-ranks (distinct ignoring-suits)
        occurrences  (frequencies ignoring-suits)]
    (->> (sort-by :index unique-ranks)
         (partition 2 1)
         (filter (fn [[a b]] (>= 1 (- (:index b) (:index a)))))
         (map (comp distinct flatten))
         (mapcat (partial multiple-runs occurrences))
         (filter #(<= min-length (count %)))
         (map count)
         (reduce +))))

(defn longest-flush
  [min-length hand]
  (->> (sort-by :suit hand)
       (partition-by :suit)
       (filter #(>= (count %) min-length))
       (sort-by count)
       last))

; crib specific

(defn one-for-'is-nob?
  [hand]
  (when-not (= :jack (:rank (last hand)))
    (let [s (:suit (last hand))]
      (some (fn [{:keys [suit rank]}]
              (and (= suit s) (= rank :jack))) hand))))

(defn score
  [hand]
  (+ (* 2 (num-fifteens hand))
     (* 2 (num-matching-pairs hand))
     (points-from-runs 3 hand)
     (count (longest-flush 4 hand))
     (if (one-for-'is-nob? hand)
       1 0)))

(defn whos-turn?
  [turn-number num-players]
  (first (rotate (range num-players) turn-number)))

(def whos-turn?-memo
  (memoize whos-turn?))


(comment

  "A hand with multiple runs"
  (def hand
    [{:suit :spades :rank :two :value 2 :index 2}
     {:suit :hearts :rank :two :value 2 :index 2}
     {:suit :spades :rank :three :value 3 :index 3}
     {:suit :spades :rank :four :value 4 :index 4}
     {:suit :diamonds :rank :two :value 2 :index 2}])

  )