(ns mancala.tui
  (:gen-class)
  (:require [lanterna.screen :as s]))

(def scr (s/get-screen))
(defrecord Board [board-w board-h model state winner last-hole])

(defn get-key-blocking []
  (s/get-key-blocking scr))

(defn get-size []
  (s/get-size scr))

(defn start []
  (s/start scr))

(defn stop []
  (s/stop scr))

(defn print-string [x y str]
  (s/put-string scr x y str))

(defn print-moves [moves]
  (let [idv (map vector (iterate inc 0) moves)]
    (doseq [[index value] idv] (print-string 0 (+ 30 index) value))))

(defn draw-hole
  "left-x and left-y are upper left point
   w - hole width
   h - hole height
   id - the id of the hole
   q - the quatity of rocks in hole"
  [left-x left-y w h id q]

  ; abstract
  (loop [x left-x y left-y n 0]
    (when (< n w)
      (print-string (+ x n) y "*")
      (recur x y (inc n))))

  (loop [x left-x y (+ left-y (dec h)) n 0]
    (when (< n w)
      (print-string (+ x n) y "*")
      (recur x y (inc n))))

  (loop [x left-x y (inc left-y) n 0]
    (when (< n (dec h))
      (print-string x (+ y n) "*")
      (recur x y (inc n))))

  (loop [x (+ left-x (dec w)) y (inc left-y) n 0]
    (when (< n (dec h))
      (print-string x (+ y n) "*")
      (recur x y (inc n))))

  ; print hole id
  (print-string (+ left-x (/ w 2)) (+ left-y (dec h)) (str id))

  ; print quantity
  (s/put-string scr (+ left-x (/ w 2)) (+ left-y (/ h 2)) (str q) {:fg :black :bg :yellow}))

(defmulti draw-board
          (fn [board] (:state board)))

(defmethod draw-board :quit [board]
  (s/clear scr)
  (print-string 0 0 "And we're done... press any key")
  (s/redraw scr))

(defmethod draw-board :finish [board]
  (s/clear scr)
  (cond
    (= nil (:winner board)) (print-string 0 0 "Game was quitted! Goodbye")
    (= :user (:winner board)) (print-string 0 0 "You have won!!! Good work")
    (= :comp (:winner board)) (print-string 0 0 "You have lost :-( Maybe next time!"))
  (s/redraw scr))

(defn draw-move [board]
  (s/clear scr)
  (print-string 0 0 "What's your move?")
  (let [mid-x (/ (:board-w board) 2)
        mid-y (/ (:board-h board) 2)
        up-row-y (- mid-y 8)
        low-row-y (+ mid-y 1)
        start-x (- mid-x 32)
        model (:model board)]

    ; comp home
    (draw-hole start-x (- (/ (:board-h board) 2) 8) 7 16 14 (nth model 13))

    ; lower row
    (doseq [x (range 6)]
      (draw-hole (+ start-x (* 8 (inc x))) low-row-y 7 7 (inc x) (nth model x)))

    ; user home
    (draw-hole (+ start-x 56) (- (/ (:board-h board) 2) 8) 7 16 7 (nth model 6))

    ; upper row
    (doseq [x (range 12 6 -1)]
      (draw-hole (+ start-x (* 8 (- 13 x))) up-row-y 7 7 (inc x) (nth model x)))

    ; TODO: move board to left and pring moves on right
    (print-moves ["first" "second" "therd" "forth"])
    (s/redraw scr)))

(defmethod draw-board :comp-move [board]
  (draw-move board))

(defmethod draw-board :user-move [board]
  (draw-move board))