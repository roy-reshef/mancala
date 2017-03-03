(ns mancala.core-test
  (:require [clojure.test :refer :all]
            [mancala.core :refer :all]
            [mancala.tui :as tui])
  (:import [mancala.tui Board]))

(deftest get-winner-test-user
  (testing "testing get-winner function. user win"
    (let [board (tui/Board. 0 0 [0 0 0 0 0 0 10 0 0 0 0 0 0 7] :user-move nil nil)]
      (is (= :user (:winner (get-winner board)))))))

(deftest get-winner-test-tie
  (testing "testing get-winner function. tie"
    (let [board (tui/Board. 0 0 [0 0 0 0 0 0 10 0 0 0 0 0 0 10] :user-move nil nil)]
      (is (= :tie (:winner (get-winner board)))))))

(deftest get-winner-test-no-winner
  (testing "testing get-winner function. no one"
    (let [board (tui/Board. 0 0 [0 0 0 0 0 1 10 0 0 0 0 0 0 10] :user-move nil nil)]
      (is (= nil (:winner (get-winner board)))))))

(deftest move-stones-test
  (testing "testing move-stones function"
    (let [board (tui/Board. 0 0 [4 4 4 4 4 4 0 4 4 4 4 4 4 0] :user-move nil nil)
          expected-board (tui/Board. 0 0 [0 5 5 5 5 4 0 4 4 4 4 4 4 0] :user-move nil 4)
          new-board (move-stones 0 board 6)]
      (is (and (= (:last-hole expected-board) (:last-hole new-board))
               (= (:model expected-board) (:model new-board)))))))

(deftest move-stones-to-home-test
  (testing "testing move-stones-to-home function"
    (let [board (tui/Board. 0 0 [4 4 4 4 4 4 0 4 4 4 4 4 4 0] :user-move nil nil)
          expected-board (tui/Board. 0 0 [0 0 4 4 4 4 8 4 4 4 4 4 4 0] :user-move nil nil)
          new-board (move-stones-to-home board [0 1])]
      (println "new board:" new-board)
      (is (= (:model expected-board) (:model new-board))))))

(deftest get-next-target-hole-test
  (testing "testing get-next-target-hole function"
    (is (= 0 (get-next-target-hole 13 6)))))
