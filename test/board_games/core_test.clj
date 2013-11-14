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
    ;; TODO: combine the following four tests into one
    ;; TODO: test that *all* spaces not two spaces away along adjacency
    ;;       lines return false for is-jump-move
    (testing "left-to-right jumps are valid"
      ;; NB: pretty nasty representation dependence
      (doseq [space (filter (comp (partial board/get-space board)
                              (juxt first (comp inc inc fnext))
                              board/get-coords)
                      board)
              :let [[row col] (board/get-coords space)]]
        (is (game/is-jump-move? board [row col] [row (+ col 2)]))))
    (testing "right-to-left jumps are valid"
      ;; NB: pretty nasty representation dependence
      (doseq [space (filter (comp (partial board/get-space board)
                              (juxt first (comp dec dec fnext))
                              board/get-coords)
                      board)
              :let [[row col] (board/get-coords space)]]
        (is (game/is-jump-move? board [row col] [row (- col 2)]))))
    (testing "top-to-bottom jumps are valid"
      ;; NB: pretty nasty representation dependence
      (doseq [space (filter (comp (partial board/get-space board)
                              (juxt (comp inc inc first) fnext)
                              board/get-coords)
                      board)
              :let [[row col] (board/get-coords space)]]
        (is (game/is-jump-move? board [row col] [(+ row 2) col]))))
    (testing "bottom-to-top jumps are valid"
      ;; NB: pretty nasty representation dependence
      (doseq [space (filter (comp (partial board/get-space board)
                              (juxt (comp dec dec first) fnext)
                              board/get-coords)
                      board)
              :let [[row col] (board/get-coords space)]]
        (is (game/is-jump-move? board [row col] [(- row 2) col]))))
    (testing "down-and-right jumps are valid only along adjacency lines"
      (let [make-target-coords (juxt (comp inc inc first)
                                 (comp inc inc fnext))]
        ;; NB: pretty nasty representation dependence
        (doseq [space (filter (comp (partial board/get-space board)
                                make-target-coords
                                board/get-coords)
                        board)
                :let [coords (board/get-coords space)
                      target-coords (make-target-coords coords)]]
          ;; could pull this into filter, but things get messy
          (let [target (board/get-space board target-coords)]
            (if (nil? (seq (apply set/intersection
                             (map (comp set board/get-adj*)
                               [space target]))))
                (is (not (game/is-jump-move? board coords target-coords)))
                (is (game/is-jump-move? board coords target-coords)))))))
    (testing "down-and-left jumps are valid only along adjacency lines"
      (let [make-target-coords (juxt (comp inc inc first)
                                 (comp dec dec fnext))]
        ;; NB: pretty nasty representation dependence
        (doseq [space (filter (comp (partial board/get-space board)
                                make-target-coords
                                board/get-coords)
                        board)
                :let [coords (board/get-coords space)
                      target-coords (make-target-coords coords)]]
          ;; could pull this into filter, but things get messy
          (let [target (board/get-space board target-coords)]
            (if (nil? (seq (apply set/intersection
                             (map (comp set board/get-adj*)
                               [space target]))))
                (is (not (game/is-jump-move? board coords target-coords)))
                (is (game/is-jump-move? board coords target-coords)))))))
    (testing "up-and-right jumps are valid only along adjacency lines"
      (let [make-target-coords (juxt (comp dec dec first)
                                 (comp inc inc fnext))]
        ;; NB: pretty nasty representation dependence
        (doseq [space (filter (comp (partial board/get-space board)
                                make-target-coords
                                board/get-coords)
                        board)
                :let [coords (board/get-coords space)
                      target-coords (make-target-coords coords)]]
          ;; could pull this into filter, but things get messy
          (let [target (board/get-space board target-coords)]
            (if (nil? (seq (apply set/intersection
                             (map (comp set board/get-adj*)
                               [space target]))))
                (is (not (game/is-jump-move? board coords target-coords)))
                (is (game/is-jump-move? board coords target-coords)))))))
    (testing "up-and-left jumps are valid only along adjacency lines"
      (let [make-target-coords (juxt (comp dec dec first)
                                 (comp dec dec fnext))]
        ;; NB: pretty nasty representation dependence
        (doseq [space (filter (comp (partial board/get-space board)
                                make-target-coords
                                board/get-coords)
                        board)
                :let [coords (board/get-coords space)
                      target-coords (make-target-coords coords)]]
          ;; could pull this into filter, but things get messy
          (let [target (board/get-space board target-coords)]
            (if (nil? (seq (apply set/intersection
                             (map (comp set board/get-adj*)
                               [space target]))))
                (is (not (game/is-jump-move? board coords target-coords)))
                (is (game/is-jump-move? board coords target-coords)))))))))
