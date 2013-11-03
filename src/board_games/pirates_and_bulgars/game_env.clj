;;----------------------------------------------------------------------
;; File game_env.clj
;; Written by Chris Frisz
;; 
;; Created  3 Nov 2013
;; Last modified  3 Nov 2013
;; 
;; 
;;----------------------------------------------------------------------

(ns board-games.pirates-and-bulgars.game-env
  (:require [board-games.pirates-and-bulgars.utils :refer [defrecord+]]
            [board-games.pirates-and-bulgars.board :as board]
            [board-games.pirates-and-bulgars.piece :as piece]))

(defrecord+ GameEnv [state board pirate* bulgar* turn*])

(defn init-pirate* [board]
  (mapv (comp (partial apply piece/make-piece) 
          (juxt piece/get-row piece/get-col))
    (filterv (comp not (partial apply board/fort-space?) 
               (juxt board/get-row board/get-col))
      board)))

(defn make-fresh-game-env []
  (let [board (board/make-game-board)]
    (make-game-env :setup1
      board
      (init-pirate* board)
      []
      [])))

(defn add-bulgar
  [game-env bulgar]
  (update-bulgar* game-env (conj (get-bulgar* game-env) bulgar)))

(defn remove-pirate
  [game-env pirate]
  (update-pirate* game-env (filter (partial = pirate) (get-pirate* game-env))))
