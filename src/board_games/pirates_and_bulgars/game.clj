;;----------------------------------------------------------------------
;; File Pirates_And_Bulgars.clj
;; Written by Chris Frisz
;; 
;; Created 27 Oct 2013
;; Last modified  3 Nov 2013
;; 
;; 
;;----------------------------------------------------------------------

(ns board-games.pirates-and-bulgars.game
  (:require [board-games.pirates-and-bulgars.game-env :as game-env]
            [board-games.pirates-and-bulgars.piece :as piece]))

(defn player-input-coords []
  (print "enter row to place bulgar: ")
  (flush)
  (let [row (Integer. (read-line))]
    (print "enter column to place bulgar: ")
    (flush)
    (let [col (Integer. (read-line))]
      [row col])))
    
(defn run-game
  [game-env]
  (case (game-env/get-state game-env)
    :setup1 (do
              (println "choose location for first bulgar")
              (let [[row col] (player-input-coords)]
                (-> game-env
                    (game-env/add-bulgar (piece/make-piece row col))
                    (game-env/update-state :setup2)
                    recur)))
    :setup2 (do
              (println "choose location for second bulgar")
              (let [[row col] (player-input-coords)]
                (-> game-env
                    (game-env/add-bulgar (piece/make-piece row col))
                    (game-env/update-state :pirate-turn)
                    recur)))
    :pirate-turn (println "setup complete\n")))
