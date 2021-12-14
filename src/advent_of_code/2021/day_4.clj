(ns advent-of-code.2021.day-4
  (:require [advent-of-code.utils :as utils]
            [clojure.string :as str]))

(defn parse-boards
  [file-name]
  (for [board (str/split (slurp file-name) #"\n\n")]
    (for [row (str/split board #"\n")]
      (for [entry (str/split (str/trim row) #"\s+")]
        (utils/parse-int entry)))))

(defn generate-boards
  [file-name]
  (let [original-boards (parse-boards file-name)]
    (concat original-boards
          (for [board original-boards]
            (utils/transpose board)))))

(defn parse-draws
  [file-name]
  (utils/read-file-with-separator-to-ints file-name #","))

(defn check-for-completed-rows
  [board]
  (reduce #(or %1 %2)
          false
          (for [row board]
            (if (= 0 (count row))
              true
              false)))
  )

(defn apply-draw
  [boards draw]
  (for [board boards]
    (for [row board]
      (remove #(= draw %) row)))
  )

(defn day-four
  [board-file draws-file]
  (reduce
    (fn [boards draw]
      (let [new-boards (apply-draw boards draw)]
        (let [done (first (filter some?
                                  (doall (for [board new-boards]
                                           (if (check-for-completed-rows board) board)))))]
          (if done
            (reduced (* draw (apply + (flatten done))))
            new-boards)
          )
        )
      )
    (generate-boards board-file)
    (parse-draws draws-file)
    )
  )

(defn day-four-part-two
  [board-file draws-file]
  (reduce
    (fn [boards draw]
      (let [new-boards (apply-draw boards draw)]
        (let [losers (filter some?
                           (doall (for [board new-boards]
                                    (if (check-for-completed-rows board) nil board))))]
          (if (= 1 (count losers))
            (reduced (* draw (apply + (flatten (first losers)))))
            new-boards)
          )
        )
      )
    (generate-boards board-file)
    (parse-draws draws-file)
    )
  )

