(ns advent-of-code.2021.day-4
  (:require [advent-of-code.utils :as utils]
            [clojure.string :as str]))

(defn parse-boards [file-name]
  (for [board (str/split (slurp file-name) #"\n\n")]
    (for [row (str/split board #"\n")]
      (for [entry (str/split (str/trim row) #"\s+")]
        (utils/parse-int entry)))))

(defn generate-boards [file-name]
  (let [original-boards (parse-boards file-name)]
    (concat original-boards
          (for [board original-boards]
            (utils/transpose board)))))

(defn generate-board-pairs [file-name]
  (let [original-boards (parse-boards file-name)]
    (zipmap original-boards
            (for [board original-boards]
              (utils/transpose board)))))

(defn parse-draws [file-name]
  (utils/read-file-with-separator-to-ints file-name #","))

(defn check-for-completed-rows [board]
  (some true? (for [row board] (= 0 (count row)))))

(defn remove-draw [board draw]
  (for [row board] (remove #(= draw %) row)))

(defn apply-draw [boards draw]
  (for [board boards] (remove-draw board draw)))

(defn apply-draw-to-board-pairs [board-pairs draw]
  (reduce
    (fn [m board-pair]
      (assoc m (remove-draw (key board-pair) draw) (remove-draw (val board-pair) draw)))
    {}
    board-pairs))

(defn find-winner [boards draws]
  (reduce
    (fn [boards draw]
      (let [new-boards (apply-draw boards draw)]
        (let [done (first (remove nil?
                                  (for [board new-boards]
                                    (if (check-for-completed-rows board) board nil))))]
          (if done
            (reduced (* draw (apply + (flatten done))))
            new-boards))))
    boards
    draws)
  )

(defn day-four [board-file draws-file]
  (find-winner (generate-boards board-file) (parse-draws draws-file)))

(defn day-four-part-two [board-file draws-file]
  (reduce
    (fn [board-pairs draw]
      (let [new-board-pairs (apply-draw-to-board-pairs board-pairs draw)]
        (let [losers (remove nil?
                             (for [board-pair new-board-pairs]
                               (if (or (check-for-completed-rows (key board-pair))
                                       (check-for-completed-rows (val board-pair))) nil board-pair)))]
          (if (= 0 (count losers))
            (reduced (* draw (apply + (flatten (val (first new-board-pairs))))))
            losers))))
    (generate-board-pairs board-file)
    (parse-draws draws-file)))

