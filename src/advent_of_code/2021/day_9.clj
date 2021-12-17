(ns advent-of-code.2021.day-9
  (:require [advent-of-code.utils :as utils]))

(defn get-adjacent-values
  [point grid]
  (remove nil? (for [offset [[0 1] [1 0] [-1 0] [0 -1]]]
     (let [[x y] (utils/seq-add point offset)]
       (get (get grid y) x)))))

(defn find-low-point-values
  [grid]
  (for [x (keys (get grid 0)) y (keys grid)]
    (let [point (get (get grid y) x)
          adjacent-points (get-adjacent-values [x y] grid)]
      (if (every? true? (for [adjacent-point adjacent-points]
                             (or (nil? adjacent-point) (> adjacent-point point)))) point))))

(defn day-9 [file-name]
  (let [low-points (remove nil? (find-low-point-values (utils/parse-grid-of-values file-name)))]
    (+ (count low-points) (apply + low-points))))

;----------------------------------------------------------------------------------------------------------------

(defn find-low-points [grid]
  (remove nil? (for [x (range (count (get grid 0))) y (range (count grid))]
                 (let [point (get (get grid y) x)
                       adjacent-points (get-adjacent-values [x y] grid)]
                   (if (every? identity (for [adjacent-point adjacent-points]
                                          (or (nil? adjacent-point) (> adjacent-point point))))
                     [x y])))))

(defn expand-point [point]
  (for [offset [[0 1] [1 0] [-1 0] [0 -1] [0 0]]] (utils/seq-add point offset)))

(defn expand-points [original-points]
  (distinct (apply concat (for [point original-points] (expand-point point)))))

(defn remove-high-points-and-off-grid [points grid]
  (remove #(let [value (get (get grid (second %)) (first %))] (or (= value 9) (nil? value))) points))

(defn expand-basin [basin grid]
  (let [new-basin (remove-high-points-and-off-grid (expand-points basin) grid)]
    (if (= (count basin) (count new-basin)) new-basin (expand-basin new-basin grid))))

(defn find-basin [low-point grid]
  (expand-basin [low-point] grid))

(defn day-9-part-2 [file-name]
  (apply * (take-last 3 (sort (let [grid (utils/parse-grid-of-values file-name)
                                    low-points (find-low-points grid)]
                                (for [point low-points]
                                  (count (find-basin point grid))))))))
