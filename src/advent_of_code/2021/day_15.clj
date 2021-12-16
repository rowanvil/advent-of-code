(ns advent-of-code.2021.day-15
  (:require [advent-of-code.utils :as utils]
            [clojure.string :as str]))

(defn inc-loop [n i]
  (let [value (mod (+ n i) 9)]
    (if (= value 0) 9 value))
  )

(defn next-steps
  [point]
  (for [ofset '([0 1] [1 0] [0 -1] [-1 0])]
    (utils/seq-add point ofset)))

(defn parse-map-to-graph
  [grid-map]
  (zipmap (keys grid-map)
          (for [k (keys grid-map)]
            (select-keys grid-map (next-steps k))
            )))

(defn parse-file-to-map
  [file-name]
  (let [grid (utils/read-file-line-by-line file-name)
        y-range (range (count grid))
        x-range (range (count (nth grid 0)))]
    (apply hash-map (apply concat (for [x x-range y y-range]
                                    (list [x y] (utils/parse-int (str (nth (nth grid y) x))))
                                    ))))
  )

(defn parse-file-to-graph
  [file-name]
  (parse-map-to-graph (parse-file-to-map file-name)))

(defn expand-point [point n]
  (apply merge (for [x (range 5)
         y (range 5)]
                 {[(+ (first (key point)) (* n x))
                   (+ (second (key point)) (* n y))]
                  (inc-loop (val point) (+ x y))}
     ))

  )

(defn parse-and-expand-cave-to-graph
  [file-name]
  (let [n (count (first (utils/read-file-line-by-line file-name)))]
    (parse-map-to-graph (apply merge (for [point (parse-file-to-map file-name)]
                                       (expand-point point n)
                                       ))))

  )

(defn day-15 [file-name]
  (sort (utils/dijkstra (parse-file-to-graph file-name) [0 0]))
  )

(defn day-15-part-two [file-name]
  (let [cave (parse-and-expand-cave-to-graph file-name)]
    (println "cave: " (count cave) )
    (last (sort (utils/dijkstra cave [0 0])))
    )

  )