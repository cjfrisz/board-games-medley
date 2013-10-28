;;----------------------------------------------------------------------
;; File core.clj
;; Written by Chris Frisz
;; 
;; Created 27 Oct 2013
;; Last modified 27 Oct 2013
;; 
;; 
;;----------------------------------------------------------------------

(ns board-games.core
  (:require [board-games.pirates-and-bulgars :as pb]))

(defn -main
  [& args]
  (pb/verify-board (pb/make-game-board)))
