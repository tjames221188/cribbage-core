(ns cribbage-core.card.pure-test
  (:require [clojure.test :refer :all]
            [cribbage-core.cards.pure :refer :all]))

(deftest test-subset?
  (let [x #{1 2 3 4 5}
        y #{3 4 5}
        z #{4 5 6}]
    (testing
      "y is a subset of x"
      (is (subset? y x))
      "z is not a subset of x or y"
      (is (and (not (subset? z x))
               (not (subset? z y)))))))

(deftest test-all-empty?
  (let [empty [[] [] [] []]
        not-empty [[] [] [] [] [1]]]
    (is (and (all-empty? empty)
             (not (all-empty? not-empty))))))
