;;----------------------------------------------------------------------
;; File game.clj
;; Written by Chris Frisz
;; 
;; Created 27 Oct 2013
;; Last modified 17 Nov 2013
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

(defn is-jump-move?
  [board src-coords dst-coords]
  (let [[src-row src-col] src-coords
        [dst-row dst-col] dst-coords
        mid-coords [(/ (+ src-row dst-row) 2) (/ (+ src-col dst-col) 2)]]
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

;; NB: might be obviated by enumerate-pirate-moves
#_(defn is-valid-pirate-move?
  [game-env pirate-coords dst-coords]
  (and (is-adjacent-move? (game-env/get-board game-env)
         pirate-coords
         dst-coords)
       (not (space-occupied? game-env dst-coords))))

;; NB: might be obviated by enumerate-bulgar-moves
#_(defn is-valid-bulgar-move?
  [game-env bulgar-coords dst-coords]
  (and (not (space-occupied? game-env dst-coords))
       (or (is-adjacent-move? board bulgar-coords dst-coords)
           (let [[bulgar-row bulgar-col] bulgar-coords
                 [dst-row dst-col] dst-coords]
             (is-jump-move? board bulgar-coords dst-coords)))))

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
        jump-move* (for [space-coords (map board/get-coords board)
                         :when (and (not (space-occupied? board
                                           space-coords))
                                    (is-jump-move? board
                                      piece-coords
                                      space-coords))]
                     space-coords)]
    (if (nil? (seq jump-move*))
        jump-move*
        (get-adjacent-moves game-env piece))))

(defn fort-captured?
  [game-env]
  (every? #(some (partial = %) (game-env/get-pirate* game-env))
    (map board/get-coords
      (filter board/get-fort? (game-env/get-board game-env)))))

(defn bulgars-immobilized?
  [game-env]
  ;; NB: no such function as enumerate moves!
  (every? (comp nil? seq enumerate-bulgar-moves)
    (game-env/get-bulgar*)))

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

(defn do-pirate-turn
  [game-env]
  (let [input-coords (player-input-coords
                       "enter coordinates of pirate to move: ")]
    (if (space-occupied? game-env input-coords)
      (let [piece (get-piece-by-coords game-env input-coords)]
        (if (game-env/pirate? game-env piece)
          (let [move-coords (player-input-coords "enter space to move pirate: ")
                valid-moves (enumerate-pirate-moves game-env piece)]
            (if (some #{move-coords} valid-moves)
              (-> game-env
                  (game-env/update-pirate*
                   (replace
                    {piece
                     (piece/update-coords piece  move-coords)}
                    (game-env/get-pirate* game-env)))
                  (game-env/update-state :bulgar-turn))
              (do
                (println "not a valid move for that piece")
                (recur game-env))))
          (do
            (println "selected piece is not a pirate")
            (recur game-env))))
      (do
        (println "no piece at those coordinates")
        (recur game-env)))))
 
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
    :pirate-turn (recur (do-pirate-turn game-env))
    :bulgar-turn (println "bulgar turns not implemented yet!")))
