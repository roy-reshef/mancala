(ns mancala.core
  (:gen-class)
  (:require [mancala.tui :as tui])
  (:import [mancala.tui Board]))

(def ^:const user-home 6)
(def ^:const comp-home 13)

(defn valid-user-hole-number? [v]
  (and (char? v) (<= 49 (int v) 64)))

(defn get-next-target-hole [current-target-hole skip-hole]
  (if (= (inc current-target-hole) 14)                      ; if larger then vector size
    (get-next-target-hole -1 skip-hole)
    (if (= (inc current-target-hole) skip-hole)             ; if next hole is the one which should be skiped
      (get-next-target-hole (inc current-target-hole) skip-hole)
      (inc current-target-hole))))

(defn move-stones [hole board skip-hole]
  (loop [new-model (:model board)
         num (nth new-model hole)
         last-hole hole
         target-hole (inc hole)]
    (println "moving: num-" num ", target-hole-" target-hole "new-model-" new-model)
    (if (> num 0)
      (recur (update-in (update-in new-model [target-hole] inc) [hole] dec)
             (dec num)
             target-hole
             (get-next-target-hole target-hole skip-hole))
      (do
        (println "model after move:" new-model)
        (assoc (assoc board :model new-model) :last-hole last-hole)))))

(defn get-winner [board]
  (let [model (:model board)
        user-state (reduce + (subvec model 0 user-home))
        comp-state (reduce + (subvec model 7 comp-home))
        user-done (true? (= user-state 0))
        comp-done (true? (= comp-state 0))
        user-score (nth model user-home)
        comp-score (nth model comp-home)]

    (if (and user-done comp-done)
      (assoc (assoc board :state :finish) :winner (if (= user-score comp-score)
                                                    :tie
                                                    (if (> user-score comp-score)
                                                      :user
                                                      :comp)))
      board)))

(defn clear-last-hole [board]
  (assoc board :last-hole nil))

(def opos-hole-f #(- % (* 2 (- % user-home))))

(defn move-from-hole-to-hole [model source-hole target-hole]
  (assoc (update-in model [target-hole] + (get model source-hole)) source-hole 0))

(defmulti move-stones-to-home
          (fn [board source-holes] (:state board)))

;(reduce (fn [m v] (move-from-hole-to-hole m v 4)) [4 4 4 4 0] [0 1])
(defmethod move-stones-to-home :user-move [board source-holes]
  (println "moving stones to user home")
  (assoc board :model (reduce (fn [m v] (move-from-hole-to-hole m v user-home)) (:model board) source-holes)))

(defmethod move-stones-to-home :comp-move [board source-holes]
  (println "moving stones to comp home")
  (assoc board :model (reduce (fn [m v] (move-from-hole-to-hole m v comp-home)) (:model board) source-holes)))

(defn check-oposite-hole [board hole-num]
  (let [opos-hole (opos-hole-f hole-num)
        model (:model board)]
    (println "checking oposite hole. hole:" hole-num ", opos hole:" opos-hole)
    (if (and (= (get model hole-num) 1) (> (get model opos-hole) 0)) ; if now hole has only 1 stone
      (move-stones-to-home board [hole-num opos-hole])
      board)))

(defn post-move [board]
  (println "post move. board:" board)
  (let [board (get-winner board)
        state (:state board)
        last-hole (:last-hole board)]
    (if (nil? (:winner board))
      (do
        (println "should check last move")
        (if (= :user-move state)
          (do
            (println "performing post user move checks")
            (if (= last-hole user-home)
              (do (println "user last hole was home. user has another move") (clear-last-hole board))
              (clear-last-hole (assoc (check-oposite-hole board last-hole) :state :comp-move))))
          (do                                               ; comp move
            (println "performing post computer move checks")
            (if (= last-hole comp-home)
              (do (println "computer last hole was home. computer has another move") (clear-last-hole board))
              (clear-last-hole (assoc (check-oposite-hole board last-hole) :state :user-move))))))
      (clear-last-hole board))))

(defn has-move [target-holes]
  (not-empty (filter #(not= 0 %) target-holes)))

(defn generate-move [board]
  (println "generating computer move")

  ; naive strategy - play the hole with the most stones if there is
  ; one that has more then 0 stones
  (let [comp-holes (subvec (:model board) 7 comp-home)]
    (if (has-move comp-holes)
      ; use transducing?
      (move-stones (+ 7 (first (apply max-key second (map-indexed vector comp-holes)))) board user-home)
      (do
        (println "no available play for computer")
        board))))

(defn user-move [hole board]
  (let [hole-number (dec (Integer/parseInt (str hole)))]
    (move-stones hole-number board comp-home)))

(defn user-input [board]
  (let [user-holes (subvec (:model board) 0 user-home)]
    (if (has-move user-holes)
      (let [k (tui/get-key-blocking)]
        (cond
          (= k :escape) (assoc board :state :quit)
          (valid-user-hole-number? k) (post-move (user-move k board))
          :else (println "invlalid key input!")))
      (do
        (println "no available play for user")
        (assoc board :state :comp-move)))))

(defmulti handle-state (fn [board] (:state board)))

(defmethod handle-state :quit [board]
  (println "handling quit state")
  (assoc board :state :finish))

(defmethod handle-state :user-move [board]
  (println "handling user move state")
  (user-input board))

(defmethod handle-state :comp-move [board]
  (println "handling comp move state")
  (post-move (generate-move board)))

(def initial-board [4 4 4 4 4 4 0 4 4 4 4 4 4 0])

(defn run-game [w h]
  (tui/start)
  ; create opening screen
  (tui/print-string 0 0 "Welcome to mancala")

  (loop [board (tui/Board. w h initial-board :user-move nil nil)]
    ;(Thread/sleep 1000)
    (println "drawing board:" board)
    (tui/draw-board board)

    (if (= :finish (:state board))
      (println "game has ended")
      (recur (handle-state board))))

  (tui/get-key-blocking)
  (tui/stop))

(defn -main [& args]
  (let [[w h] (tui/get-size)]
    (run-game w h)))