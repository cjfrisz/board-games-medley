;;----------------------------------------------------------------------
;; File pirates_and_bulgars.clj
;; Written by Chris Frisz
;; 
;; Created 27 Oct 2013
;; Last modified 27 Oct 2013
;; 
;; 
;;----------------------------------------------------------------------

(ns board-games.pirates-and-bulgars
  (:require [clojure.string :as string]))

(defn make-factory-name
  [name]
  (symbol 
   (str "make" 
     (string/lower-case (string/replace name #"([A-Z])" "-$1")))))

(defmacro defrecord+
  [name field* & spec*]
  `(do
     (defrecord ~name ~(vec field*) ~@spec*)
     (defn ~(make-factory-name name)
       ~(vec field*)
       (~(symbol (str name ".")) ~@field*))
     ~@(for [field field*]
         `(def ~(symbol (str "get-" field)) ~(keyword field)))
     ~@(for [field field*]
         `(defn ~(symbol (str "update-" field))
            [record# new-val#]
            (assoc record# ~(keyword field) new-val#)))))

(defrecord+ Space [row col adj* fort?])

;; Board layout
;; Normal (sea) spaces represented by *
;; Fort spaces represented by F
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
;; * - * - F - F - F - * - *
;;         | \ | / |
;;         F - F - F
;;         | / | \ |
;;         F - F - F
  
(defn make-game-board []
  (mapv (partial apply make-space)
    [[0 0 [[0 1] [1 1] [1 0]] false] 
     [0 1 [[0 2] [1 1] [0 0]] false]
     [0 2 [[1 2] [1 1] [0 1]] false]

     [1 0 [[0 0] [1 1] [2 2]] false] 
     [1 1 [[0 1] [0 2] [1 2] [2 4] [2 3] [2 2] [1 0] [0 0]] false]
     [1 2 [[0 2] [2 4] [1 1]] false]

     [2 0 [[2 1] [3 1] [3 0]] false] 
     [2 1 [[2 2] [3 1] [2 0]] false]
     [2 2 [[1 0] [1 1] [2 3] [3 3] [3 2] [3 1] [2 1]] false]
     [2 3 [[1 1] [2 4] [3 3] [2 2]] false] 
     [2 4 [[1 2] [2 5] [3 5] [3 4] [3 3] [2 3] [1 1]] false]
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
     [4 2 [[3 2] [3 3] [4 3] [5 1] [5 0] [4 1] [3 1]] true]
     [4 3 [[3 3] [4 4] [5 1] [4 2]] true] 
     [4 4 [[3 4] [3 5] [4 5] [5 2] [5 1] [4 3] [3 3]] true]
     [4 5 [[3 5] [4 6] [4 4]] false]
     [4 6 [[3 6] [4 5] [3 5]] false]

     [5 0 [[4 2] [5 1] [6 0]] true] 
     [5 1 [[4 3] [4 4] [5 2] [6 2] [6 1] [6 0] [5 0] [4 2]] true]
     [5 2 [[4 4] [6 2] [5 1]] true]

     [6 0 [[5 0] [5 1] [6 1]] true] 
     [6 1 [[5 1] [6 2] [6 0]] true]
     [6 2 [[5 2] [6 1] [5 1]] true]]))

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

(defrecord Piece [row col])

(defn make-piece [row col] (Piece. row col))
(def get-piece-row :row)
(def get-piece-col :col)
(defn update-piece-row [piece new-row] (assoc piece :row new-row))
(defn update-piece-col [piece new-col] (assoc piece :col new-col))

(defn init-pirate* [board]
  (mapv (comp (partial apply make-piece) (juxt get-row get-col))
    (filterv (comp (partial not fort-space?) (juxt get-row get-col))
      board)))

;; NB: according to the rules, this depends on player choice
(defn init-bulgar* [board]
  (mapv (partial apply make-piece)
    (take 2
      (shuffle (filterv (comp fort-space? 
                          (juxt get-row get-col)) 
                 board)))))

(defrecord+ GameEnvironment [board pirate* bulgar* turn*])
                               
