(ns advent-of-code.2021.day-9
  (:require [advent-of-code.utils :as utils]))

(defn parse-input
  [file-name]
  (for [line (utils/read-file-line-by-line file-name)]
    (utils/string-to-list-of-ints line))
  )

(defn left-value [x y grid]
  (if (= x 0)
    nil
    (nth (nth grid y) (- x 1))
    )
  )

(defn right-value [x y grid]
  (if (>= x (- (count (nth grid 0)) 1))
    nil
    (nth (nth grid y) (+ 1 x)))
  )

(defn up-value [x y grid]
  (if (= y 0)
   nil
   (nth (nth grid (- y 1)) x))
  )

(defn down-value [x y grid]
  (if (>= y (- (count grid) 1))
    nil
    (nth (nth grid (+ 1 y)) x))
  )

(defn get-adjacent-values
  [x y grid]
  (list
    (left-value x y grid)
    (right-value x y grid)
    (up-value x y grid)
    (down-value x y grid)
    )
  )

(defn find-low-point-values
  [grid]
  (for [x (range (count (nth grid 0))) y (range (count grid))]
    (let [point (nth (nth grid y) x)
          adjacent-points (get-adjacent-values x y grid)]
      (if (every? identity (for [adjacent-point adjacent-points]
                             (if (nil? adjacent-point)
                               true
                               (if (> adjacent-point point)
                                 true
                                 false))
                             ))
        point)
      ))

  )

(defn day-9
  [file-name]
  (let [low-points (remove nil? (find-low-point-values (parse-input file-name)))]
    (+ (count low-points) (apply + low-points))))

;----------------------------------------------------------------------------------------------------------------

(defn left-location [x y]
  (if (= x 0)
    nil
    (list (- x 1) y)
    )
  )

(defn right-location [x y grid]
  (if (>= x (- (count (nth grid 0)) 1))
    nil
    (list (+ 1 x) y))
  )

(defn up-location [x y]
  (if (= y 0)
    nil
    (list x (- y 1)))
  )

(defn down-location [x y grid]
  (if (>= y (- (count grid) 1))
    nil
    (list x (+ 1 y)))
  )

(defn get-adjacent-locations
  [x y grid]
  (list
    (left-location  x y)
    (right-location  x y grid)
    (up-location  x y)
    (down-location x y grid)
    )
  )

(defn get-adjacent-points-for-group
  [original-points grid]
  (distinct (concat (apply concat (for [point original-points]
                           (remove nil? (get-adjacent-locations (first point) (second point) grid))))
                  original-points))
  )

(defn remove-high-points
  [points grid]
  (remove #(= 9 (nth (nth grid (second %)) (first %))) points)
  )

(defn find-low-points
  [grid]
  (for [x (range (count (nth grid 0))) y (range (count grid))]
    (let [point (nth (nth grid y) x)
          adjacent-points (get-adjacent-values x y grid)]
      (if (every? identity (for [adjacent-point adjacent-points]
                             (if (nil? adjacent-point)
                               true
                               (if (> adjacent-point point)
                                 true
                                 false))
                             ))
        (list x y))
      ))
  )

(defn expand-basin
  [basin grid]
  (let [new-basin (remove-high-points (get-adjacent-points-for-group basin grid) grid)]
    (if (= (count basin) (count new-basin))
      new-basin
      (expand-basin new-basin grid))
    )
  )

(defn find-basin
  [low-point grid]
  (expand-basin (list low-point) grid)
  )

(defn day-9-part-2
  [file-name]
  (apply * (take-last 3 (sort (let [grid (parse-input file-name)
                                    low-points (remove nil? (find-low-points grid))]
                                (for [point low-points]
                                  (count (find-basin point grid)))
                                ))))
  )





