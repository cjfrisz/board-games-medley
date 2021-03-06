;;----------------------------------------------------------------------
;; File render.clj
;; Written by Chris Frisz
;; 
;; Created  3 Nov 2013
;; Last modified 26 Nov 2013
;; 
;; 
;;----------------------------------------------------------------------

(ns board-games.pirates-and-bulgars.render
  (:require [board-games.pirates-and-bulgars.game-env :as game-env]
            [board-games.pirates-and-bulgars.board :as board]
            [board-games.pirates-and-bulgars.piece :as piece]))

;; NB: someday this will be more complicated when we render messages
;; NB: to something other than a console
(def game-msg println)

(defn render-game
  [game-env]
  (let [board (game-env/get-board game-env)
        pirate* (game-env/get-pirate* game-env)
        bulgar* (game-env/get-bulgar* game-env)
        row-connector* ["            | \\ | / |"
                        "            | / | \\ |"
                        "    | \\ | / | \\ | / | \\ | / |"
                        "    | / | \\ | / | \\ | / | \\ |"
                        "            | \\ | / |"
                        "            | / | \\ |"]
        row-label* ["A" "B" "C" "D" "E" "F" "G"]]
    ;; column labels
    (println "    1   2   3   4   5   6   7")
    (newline)
    ;; NB: representation dependence
    (doseq [row-idx (range 0 (inc (reduce max 0 (map board/get-row board))))]
      (print (str (get row-label* row-idx) "   "))
      ;; NB: double representation dependence
      (let [col-max (reduce max 0 
                      (map board/get-row
                        (filter (comp (partial = row-idx) board/get-col)
                          board)))]
        (loop [col-idx 0
               rendered-space? false]
          (when-not (> col-idx col-max)
            (when rendered-space? (print " - "))
            (if-let [space (board/get-space board [row-idx col-idx])]
              (let [on-this-space? (partial some 
                                     (comp (partial = [row-idx col-idx])
                                       piece/get-coords))]
                (cond
                  (on-this-space? pirate*)              (print "P")
                  (on-this-space? bulgar*)              (print "B")
                  (board/fort-space? [row-idx col-idx]) (print "^")
                  :else                                 (print "*"))
                (recur (inc col-idx) true))
              (do (print "    ") (recur (inc col-idx) false))))))
      (newline)
      (when-let [row-connector (get row-connector* row-idx)]
        (println row-connector)))))
      
        
          
