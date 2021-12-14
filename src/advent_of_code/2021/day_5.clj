(ns advent-of-code.2021.day-5
  (:require [advent-of-code.utils :as utils]
            [clojure.string :as str]))

(defn data-line-to-coordinate-map
  [string]
  (for [number (str/split string #" -> |,")]
    (utils/parse-int number)
    ))

(defn horizontal-ends-to-line
  [coordinates]
  (let [range (if (< (nth coordinates 0) (nth coordinates 2))
                (range (nth coordinates 0) (+ 1 (nth coordinates 2)))
                (range (nth coordinates 2) (+ 1 (nth coordinates 0)))
                )]
    (for [x range]
      (list x (nth coordinates 1))
      )
    )
  )

(defn vertical-ends-to-line
  [end-coordinates]
  (let [range (if (< (nth end-coordinates 1) (nth end-coordinates 3))
                (range (nth end-coordinates 1) (+ 1 (nth end-coordinates 3)))
                (range (nth end-coordinates 3) (+ 1 (nth end-coordinates 1)))
                )]
    (for [y range]
      (list (nth end-coordinates 0) y )
      )
    )
  )

(defn join-coordinates
  [x-values y-values]
  (for [n (range (count x-values))]
    (list (nth x-values n) (nth y-values n))))

(defn diagonal-ends-to-line
  [coordinates]
  (let [x-range (if (< (nth coordinates 0) (nth coordinates 2))
                (range (nth coordinates 0) (+ 1 (nth coordinates 2)))
                (range (nth coordinates 0) (- (nth coordinates 2) 1) -1)
                )
        y-range (if (< (nth coordinates 1) (nth coordinates 3))
                 (range (nth coordinates 1) (+ 1 (nth coordinates 3)))
                 (range (nth coordinates 1) (- (nth coordinates 3) 1) -1)
                 )]
    (join-coordinates x-range y-range)
    )
  )

(defn end-coordinates-to-all-point-horizontal-and-vertical-only
  [coordinate-sets]
  (apply concat (for [coordinates coordinate-sets]
                  (cond
                    (= (nth coordinates 1) (nth coordinates 3)) (horizontal-ends-to-line coordinates)
                    (= (nth coordinates 0) (nth coordinates 2)) (vertical-ends-to-line coordinates)
                    )
                  )))

(defn end-coordinates-to-all-point
  [coordinate-sets]
  (apply concat (for [coordinates coordinate-sets]
                  (cond
                    (= (nth coordinates 1) (nth coordinates 3)) (horizontal-ends-to-line coordinates)
                    (= (nth coordinates 0) (nth coordinates 2)) (vertical-ends-to-line coordinates)
                    (= (Math/abs (- (nth coordinates 1) (nth coordinates 3)))
                       (Math/abs (- (nth coordinates 0) (nth coordinates 2))))
                    (diagonal-ends-to-line coordinates)
                    )
                  )))

(defn day-five
  [file-name]
  (count (filter #(< 1 (val %))
                 (frequencies
                   (end-coordinates-to-all-point-horizontal-and-vertical-only
                     (for [line (utils/read-file-line-by-line file-name)]
                       (data-line-to-coordinate-map line))))))
  )

(defn day-five-part-two
  [file-name]
  (count (filter #(< 1 (val %))
                 (frequencies
                   (end-coordinates-to-all-point
                     (for [line (utils/read-file-line-by-line file-name)]
                       (data-line-to-coordinate-map line))))))
  )

