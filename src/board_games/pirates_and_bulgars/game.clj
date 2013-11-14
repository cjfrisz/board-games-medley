;;----------------------------------------------------------------------
;; File Pirates_And_Bulgars.clj
;; Written by Chris Frisz
;; 
;; Created 27 Oct 2013
;; Last modified 13 Nov 2013
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
  ;; NB: consider rewriting this in terms of set intersections
  (and (some (partial is-adjacent-move? board src-coords)
         (board/get-adj* (board/get-space board dst-coords)))
       (let [[src-row src-col] src-coords
             [dst-row dst-col] dst-coords]
         (or (= src-row dst-row)
             (= src-col dst-col)
             ;; middle school math!
             (let [slope (/ (- dst-col src-col) (- dst-row src-row))]
               (or (= slope 1) (= slope -1)))))))

(defn is-valid-pirate-move?
  [game-env pirate-coords dst-coords]
  (and (is-adjacent-move? (game-env/get-board game-env)
         pirate-coords
         dst-coords)
       (not (space-occupied? game-env dst-coords))))

(defn is-valid-bulgar-move?
  [game-env bulgar-coords dst-coords]
  (and (not (space-occupied? game-env dst-coords))
       ((some-fn is-adjacent-move? is-jump-move?)
         (game-env/get-board game-env) bulgar-coords dst-coords)))

(defn player-input-coords []
  (print "enter coordinates to place a bulgar: ")
  (flush)
  (let [input (string/upper-case (string/trim (read-line)))]
    (if (> (count input) 2) 
      (do
        (println "input " input " too long. please try again.")
        (recur))
      (let [row (get input 0)
            col (get input 1)]
        (cond
          (not-any? #{row} "ABCDEFG")
          ,(do
             (println "invalid row \"" row "\"")
             (println "row value must be a letter A-G")
             (recur))
          (not-any? #{col} "1234567")
          ,(do
             (println "invalid column \"" col "\"")
             (println "column value must be a number 1-7")
             (recur))
          :else [(- (int row) (int \A)) (- (int col) (int \1))])))))
 
(defn run-game
  [game-env]
  (render/render-game game-env)
  (letfn [(do-setup [msg next-state]
            (println msg)
            (let [[row col] (player-input-coords)
                  try-again! (fn [msg]
                               (println msg)
                               (render/render-game game-env))]
              (cond
                (space-occupied? game-env [row col])
                ,(do
                   (try-again! "space is already occupied")
                   (recur msg next-state))
                (not (board/fort-space? [row col]))
                ,(do
                   (try-again! "starting location must be within the fortress")
                   (recur msg next-state))
                :else (-> game-env
                        (game-env/add-bulgar (piece/make-piece [row col]))
                        (game-env/update-state next-state)))))]
    (case (game-env/get-state game-env)
      :setup1 (let [new-game-env (do-setup
                                   "choose location for first bulgar"
                                   :setup2)]
                (recur new-game-env))
      :setup2 (let [new-game-env (do-setup
                                   "choose location for second bulgar"
                                   :pirate-turn)]
                (recur new-game-env))
      :pirate-turn (println "setup complete\n"))))
