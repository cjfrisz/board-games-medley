;;----------------------------------------------------------------------
;; File core.clj
;; Written by Chris Frisz
;; 
;; Created 27 Oct 2013
;; Last modified  3 Nov 2013
;; 
;; 
;;----------------------------------------------------------------------

(ns board-games.core
  (:require [board-games.pirates-and-bulgars.game :as game]
            [board-games.pirates-and-bulgars.game-env :as game-env]))

(defn -main
  [& args]
  (game/run-game (game-env/make-fresh-game-env)))
