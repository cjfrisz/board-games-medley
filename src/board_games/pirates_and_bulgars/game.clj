;;----------------------------------------------------------------------
;; File game.clj
;; Written by Chris Frisz
;; 
;; Created 27 Oct 2013
;; Last modified 18 Nov 2013
;; 
;; 
;;----------------------------------------------------------------------

(ns board-games.pirates-and-bulgars.game
  (:require [clojure.string :as string]
            [board-games.pirates-and-bulgars.board :as board]
            [board-games.pirates-and-bulgars.game-env :as game-env]
            [board-games.pirates-and-bulgars.piece :as piece]
            [board-games.pirates-and-bulgars.render :as render]))

(defn space-occupied?
  [game-env coords]
  (some (comp (partial = coords) piece/get-coords)
    (apply concat ((juxt game-env/get-pirate*
                     game-env/get-bulgar*)
                    game-env))))

(defn is-adjacent-move?
  [board src-coords dst-coords]
  (some #{src-coords}
    (board/get-adj* (board/get-space board dst-coords))))

(defn get-mid-coords
  [[row1 col1] [row2 col2]]
  [(/ (+ row1 row2) 2) (/ (+ col1 col2) 2)])

(defn is-jump-move?
  [board src-coords dst-coords]
  (let [mid-coords (get-mid-coords src-coords dst-coords)]
    (and (some #{mid-coords}
           (board/get-adj* (board/get-space board src-coords)))
         (some #{mid-coords}
           (board/get-adj* (board/get-space board dst-coords)))
         (let [[src-row src-col] src-coords
               [mid-row mid-col] mid-coords
               [dst-row dst-col] dst-coords]
           (or (= src-row mid-row dst-row)
               (= src-col mid-col dst-col)
               (let [denom1 (- mid-row src-row)
                     denom2 (- dst-row mid-row)]
                 (and (not-any? zero? [denom1 denom2])
                      (let [slope1 (/ (- mid-col src-col) denom1)
                            slope2 (/ (- dst-col mid-col) denom2)]
                        (or (= slope1 slope2 1)
                            (= slope1 slope2 -1))))))))))

(defn get-adjacent-moves
  [game-env piece]
  (let [board (game-env/get-board game-env)]
    (filter (comp not (partial space-occupied? board))
      (board/get-adj* (board/get-space board
                        (piece/get-coords piece))))))

(defn enumerate-pirate-moves
  [game-env piece]
  (assert (some #{piece} (game-env/get-pirate* game-env)))
  (get-adjacent-moves game-env piece))

(defn enumerate-bulgar-moves
  [game-env piece]
  (assert (some #{piece} (game-env/get-bulgar* game-env)))
  (let [piece-coords (piece/get-coords piece)
        board (game-env/get-board game-env)
        jump-move* (filter (partial is-jump-move? board piece-coords)
                     (remove (partial space-occupied? board)
                       (map board/get-coords board)))]
    (if (nil? (seq jump-move*))
        (get-adjacent-moves game-env piece)
        jump-move*)))

(defn fort-captured?
  [game-env]
  (every? #(some (partial = %)
             (map piece/get-coords (game-env/get-pirate* game-env)))
    (map board/get-coords
      (filter board/get-fort? (game-env/get-board game-env)))))

(defn bulgars-immobilized?
  [game-env]
  (every? (comp nil? seq (partial enumerate-bulgar-moves game-env))
    (game-env/get-bulgar* game-env)))

(defn pirates-win?
  [game-env]
  (or (fort-captured? game-env) (bulgars-immobilized? game-env)))

(defn bulgars-win?
  [game-env]
  (< (count (game-env/get-pirate* game-env)) 9))

(defn get-piece-by-coords
  [game-env coords]
  (some #(and (= (piece/get-coords %) coords) %)
    (apply concat ((juxt game-env/get-pirate*
                     game-env/get-bulgar*)
                    game-env))))

(defn player-input-coords
  [instructions]
  (print instructions)
  (flush)
  (let [input (string/upper-case (string/trim (read-line)))]
    (if (> (count input) 2) 
      (do
        (println "input " input " too long. please try again.")
        (recur instructions))
      (let [row (get input 0)
            col (get input 1)]
        (cond
          (not-any? #{row} "ABCDEFG")
          ,(do
             (println "invalid row \"" row "\"")
             (println "row value must be a letter A-G")
             (recur instructions))
          (not-any? #{col} "1234567")
          ,(do
             (println "invalid column \"" col "\"")
             (println "column value must be a number 1-7")
             (recur instructions))
          :else [(- (int row) (int \A)) (- (int col) (int \1))])))))

(defn coords->string
  [coords]
  (str (char (+ (first coords) (int \A)))
    (char (+ (fnext coords) (int \1)))))

(defn do-setup
  [game-env msg next-state]
  (println msg)
  (let [[row col] (player-input-coords
                   "enter coordinates to place a bulgar: ")
        try-again! (fn [msg]
                     (println msg)
                     (render/render-game game-env))]
    (cond
     (space-occupied? game-env [row col]) (do
                                            (try-again!
                                              "space is already occupied")
                                            (recur game-env msg next-state))
     (not (board/fort-space? [row col])) (do
                                           (try-again!
                                             (str "starting location must be "
                                               "within the fortress"))
                                           (recur game-env msg next-state))
     :else (-> game-env
               (game-env/add-bulgar (piece/make-piece [row col]))
               (game-env/update-state next-state)))))

;; NB: this smacks of a macro
(defn $do-turn
  [game-env team-string piece-pred? handle-move]
    (let [input-coords (player-input-coords (str "enter coordinates of "
                                              team-string
                                              " to move: "))]
      (if (space-occupied? game-env input-coords)
        (let [piece (get-piece-by-coords game-env input-coords)]
          (if (piece-pred? game-env piece)
            (handle-move game-env  piece
              (player-input-coords
                (str "enter space to move " team-string ": ")))
            (do
              (println "selected piece is not a " team-string)
              (recur game-env team-string piece-pred? handle-move))))
        (do
          (println "no piece at those coordinates")
          (recur game-env team-string piece-pred? handle-move)))))

(declare do-turn)

(defn handle-pirate-move
  [game-env piece move-coords]
  (let [valid-move* (enumerate-pirate-moves game-env piece)]
    (if (some #{move-coords} valid-move*)
      (-> game-env
          (game-env/update-pirate*
           (replace
            {piece
             (piece/update-coords piece move-coords)}
            (game-env/get-pirate* game-env)))
          (game-env/update-state :bulgar-turn))
      (do
        (println "not a valid move for that piece")
        (do-turn game-env :pirate-turn)))))

(defn handle-bulgar-move
  [game-env piece move-coords]
  (let [board (game-env/get-board game-env)
        piece-coords (piece/get-coords piece)]
    (let [valid-move* (enumerate-bulgar-moves game-env piece)]
      (if (some #{move-coords} valid-move*)
          (if (is-jump-move? board piece-coords move-coords)
            (let [mid-coords (get-mid-coords piece-coords move-coords)
                  captured (get-piece-by-coords game-env mid-coords)]
              (if (game-env/pirate? game-env captured)
                  (do
                    (println "captured pirate at" (coords->string mid-coords))
                    (-> game-env
                      (game-env/update-pirate*
                        (remove #{captured} (game-env/get-pirate* game-env)))
                      (game-env/update-bulgar*
                        (replace {piece (piece/update-coords piece move-coords)}
                          (game-env/get-bulgar* game-env)))
                      (game-env/update-state :pirate-turn)))
                  (do
                    (println "cannot jump over another bulgar")
                    (do-turn game-env :bulgar-turn))))
              (-> game-env
                (game-env/update-bulgar*
                 (replace {piece (piece/update-coords piece move-coords)}
                   (game-env/get-bulgar* game-env)))
                (game-env/update-state :pirate-turn)))
          (do
            (if (is-adjacent-move? (game-env/get-board game-env)
                  piece-coords
                  move-coords)
                (println "selected bulgar has jump move(s) available")
                (println "not a valid move for that piece"))
            (do-turn game-env :bulgar-turn))))))

(defn do-turn
  [game-env turn]
  (apply $do-turn game-env
    (case turn
      :pirate-turn ["pirate" game-env/pirate? handle-pirate-move]
      :bulgar-turn ["bulgar" game-env/bulgar? handle-bulgar-move]
      (throw (Exception. (str "invalid turn type " turn))))))
 
(defn run-game
  [game-env]
  (render/render-game game-env)
  (case (game-env/get-state game-env)
    :setup1 (recur (do-setup game-env
                     "choose location for first bulgar"
                     :setup2))
    :setup2 (recur (do-setup game-env
                     "choose location for second bulgar"
                     :pirate-turn))
    :pirate-turn (let [new-game-env (do-turn game-env :pirate-turn)]
                   (if (pirates-win? new-game-env)
                       (println "pirates win!")
                       (recur new-game-env)))
    :bulgar-turn (let [new-game-env (do-turn game-env :bulgar-turn)]
                   (if (bulgars-win? new-game-env)
                       (println "bulgars win!")
                       (recur new-game-env)))))
