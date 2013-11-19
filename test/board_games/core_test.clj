(ns board-games.core-test
  (:require [clojure.test :refer :all]
            [clojure.set :as set]
            [board-games.pirates-and-bulgars.board :as board]
            [board-games.pirates-and-bulgars.game :as game]
            [board-games.pirates-and-bulgars.game-env :as game-env]))

(deftest verify-board
  (let [board (game-env/get-board (game-env/make-fresh-game-env))]
    (testing "correct number of spaces for board"
      (is (= (count board) 33)))
    (testing "transitive closure of adjacency"
      (doseq [space board
              :let [coords (board/get-coords space)]]
        (doseq [adj-coords (board/get-adj* space)
                :let [adj-space (board/get-space board adj-coords)]]
          (is (some (partial = coords) (board/get-adj* adj-space))))))
    (testing "fort spaces are correctly labelled"
      (doseq [space board
              :let [fort? (board/get-fort? space)
                    coords (board/get-coords space)]]
        (is (not (and fort? (not (board/fort-space? coords)))))
        (is (not (and (not fort?) (board/fort-space? coords))))))))

(deftest test-moves
  (let [board (game-env/get-board (game-env/make-fresh-game-env))]
    (testing "adjacent spaces are valid adjacent moves"
      (doseq [space board
              :let [coords (board/get-coords space)]]
        (doseq [adj-coords (board/get-adj* space)]
          (is (game/is-adjacent-move? board coords adj-coords)))))
    (testing "non-adjacent spaces aren't adjacent moves"
      (doseq [space board
              :let [coords (board/get-coords space)
                    adj* (board/get-adj* space)]]
        (doseq [non-adj-space (filter #(or (some #{%} adj*) (= % space))
                                (map board/get-coords board))
                :let [non-adj-coords (board/get-coords non-adj-space)]]
          (is (not (game/is-adjacent-move? board coords non-adj-coords))))))
    (testing "valid jump moves"
      (doseq [space board
              :let [coords (board/get-coords space)]]
        (let [mod-vec [(comp dec dec) identity (partial + 2)]
              jump-space* (for [row-mod mod-vec
                                col-mod mod-vec
                                :let [jump-coords [(row-mod (first coords))
                                                   (col-mod (fnext coords))]
                                      jump-space (board/get-space board
                                                   jump-coords)]
                                :when (and jump-space
                                           (not (= jump-coords coords)))]
                            jump-space)]
            (let [[src-row src-col] coords
                  non-jump-space* (map board/get-coords
                                    (filter (set (conj jump-space* space))
                                      board))]
              (doseq [jump-space jump-space*
                      :let [jump-coords (board/get-coords jump-space)
                            [dst-row dst-col] jump-coords]]
                (if (and (or (= src-row dst-row)
                             (= src-col dst-col)
                             (let [slope (/ (- dst-col src-col)
                                            (- dst-row src-row))]
                               (or (= slope 1) (= slope -1))))
                         (not (nil? (seq (clojure.set/intersection
                                           (set (board/get-adj* space))
                                           (set (board/get-adj* jump-space)))))))
                     (is (game/is-jump-move? board coords jump-coords))
                     (is (not (game/is-jump-move? board coords jump-coords)))))))))))
