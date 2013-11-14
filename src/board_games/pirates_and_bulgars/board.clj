;;----------------------------------------------------------------------
;; File board.clj
;; Written by Chris Frisz
;; 
;; Created  3 Nov 2013
;; Last modified 13 Nov 2013
;; 
;; 
;;----------------------------------------------------------------------

(ns board-games.pirates-and-bulgars.board
  (:require [board-games.pirates-and-bulgars.record :refer [defrecord+]]))

(defrecord+ Space [row col adj* fort?])

;; Board layout
;; Normal (sea) spaces represented by *
;; Fort spaces represented by ^
;; Spaces connected by -, |, \, or / indicates they are connected
;;
;;         * - * - *
;;         | \ | / |
;;         * - * - *
;;         | / | \ |
;; * - * - * - * - * - * - * 
;; | \ | / | \ | / | \ | / |
;; * - * - * - * - * - * - *
;; | / | \ | / | \ | / | \ |
;; * - * - ^ - ^ - ^ - * - *
;;         | \ | / |
;;         ^ - ^ - ^
;;         | / | \ |
;;         ^ - ^ - ^
  
(defn make-game-board []
  (mapv (partial apply make-space)
    [[0 2 [[0 3] [1 3] [1 2]] false] 
     [0 3 [[0 4] [1 3] [0 2]] false]
     [0 4 [[1 4] [1 3] [0 3]] false]

     [1 2 [[0 2] [1 3] [2 2]] false] 
     [1 3 [[0 3] [0 4] [1 4] [2 4] [2 3] [2 2] [1 2] [0 2]] false]
     [1 4 [[0 4] [2 4] [1 3]] false]

     [2 0 [[2 1] [3 1] [3 0]] false] 
     [2 1 [[2 2] [3 1] [2 0]] false]
     [2 2 [[1 2] [1 3] [2 3] [3 3] [3 2] [3 1] [2 1]] false]
     [2 3 [[1 3] [2 4] [3 3] [2 2]] false] 
     [2 4 [[1 4] [2 5] [3 5] [3 4] [3 3] [2 3] [1 3]] false]
     [2 5 [[2 6] [3 5] [2 4]] false]
     [2 6 [[3 6] [3 5] [2 5]] false]

     [3 0 [[2 0] [3 1] [4 0]] false] 
     [3 1 [[2 1] [2 2] [3 2] [4 2] [4 1] [4 0] [3 0] [2 0]] false]
     [3 2 [[2 2] [3 3] [4 2] [3 1]] false]
     [3 3 [[2 3] [2 4] [3 4] [4 4] [4 3] [4 2] [3 2] [2 2]] false] 
     [3 4 [[2 4] [3 5] [4 4] [3 3]] false]
     [3 5 [[2 5] [2 6] [3 6] [4 6] [4 5] [4 4] [3 4] [2 4]] false]
     [3 6 [[2 6] [4 6] [3 5]] false]

     [4 0 [[3 0] [3 1] [4 1]] false] 
     [4 1 [[3 1] [4 2] [4 0]] false]
     [4 2 [[3 2] [3 3] [4 3] [5 3] [5 2] [4 1] [3 1]] true]
     [4 3 [[3 3] [4 4] [5 3] [4 2]] true] 
     [4 4 [[3 4] [3 5] [4 5] [5 4] [5 3] [4 3] [3 3]] true]
     [4 5 [[3 5] [4 6] [4 4]] false]
     [4 6 [[3 6] [4 5] [3 5]] false]

     [5 2 [[4 2] [5 3] [6 2]] true] 
     [5 3 [[4 3] [4 4] [5 4] [6 4] [6 3] [6 2] [5 2] [4 2]] true]
     [5 4 [[4 4] [6 4] [5 3]] true]

     [6 2 [[5 2] [5 3] [6 3]] true] 
     [6 3 [[5 3] [6 4] [6 2]] true]
     [6 4 [[5 4] [6 3] [5 3]] true]]))

(defn get-space
  [board row col]
  (some #(when (= ((juxt get-row get-col) %) [row col]) %) board))

(defn fort-space?
  [row col]
  (or (some #{row} [5 6])
      (and (= row 4) (some #{col} [2 3 4]))))

(defn verify-board
  [board]
  (when-not (= (count board) 33)
    (throw (Exception. (str "incorrect number of spacse: " (count board)
                         " (expected 33)"))))
  ;; transitive closure of adjacency
  (doseq [space board
          :let [coords ((juxt get-row get-col) space)]]
    (doseq [[adj-row adj-col] (get-adj* space)
            :let [adj-space (get-space board adj-row adj-col)]]
      (when-not (some (partial = coords) (get-adj* adj-space))
        (throw (Exception. (str "space at " [adj-row adj-col] " disputes"
                             " adjacency with " coords))))))
  ;; fort spaces
  (doseq [space board
          :let [fort? (get-fort? space)
                [row col] ((juxt get-row get-col) space)]]
    (when (and (not fort?) (fort-space? row col))
      (throw (Exception. (str "space " [row col] " incorrectly labelled"
                           " as non-fort space"))))
    (when (and fort? (not (fort-space? row col)))
      (throw (Exception. (str "space " [row col] " incorrectly labelled"
                           " as fort space"))))))
