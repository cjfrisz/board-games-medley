;;----------------------------------------------------------------------
;; File piece.clj
;; Written by Chris Frisz
;; 
;; Created  3 Nov 2013
;; Last modified  3 Nov 2013
;; 
;; 
;;----------------------------------------------------------------------

(ns board-games.pirates-and-bulgars.piece
  (:require [board-games.pirates-and-bulgars.utils :refer [defrecord+]]))

(defrecord+ Piece [row col])
