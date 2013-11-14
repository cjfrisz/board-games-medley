;;----------------------------------------------------------------------
;; File piece.clj
;; Written by Chris Frisz
;; 
;; Created  3 Nov 2013
;; Last modified 13 Nov 2013
;; 
;; 
;;----------------------------------------------------------------------

(ns board-games.pirates-and-bulgars.piece
  (:require [board-games.pirates-and-bulgars.record :refer [defrecord+]]))

(defrecord+ Piece [coords])
