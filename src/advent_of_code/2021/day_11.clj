(ns advent-of-code.2021.day-11
  (:require [advent-of-code.utils :as utils]))

(defn list-to-numbered-hash-map
  [list-to-hash fn]
  (apply sorted-map (flatten
                      (for [n (range (count list-to-hash))]
                        (list n
                              (fn (nth list-to-hash n))))))
  )

(defn list-to-map-of-maps
  [grid]
  (list-to-numbered-hash-map grid #(list-to-numbered-hash-map % identity))
  )

(defn parse-octopuses
  [file-name]
  (list-to-map-of-maps (for [line (utils/read-file-line-by-line file-name)]
                         (utils/string-to-list-of-ints line))))

(defn flashing-octopuses
  [octopus-grid]
  (remove nil? (for [x (range 10)
                     y (range 10)]
                 (if (= 0 (get (get octopus-grid y) x))
                   (list x y)))))

(defn increment-position
  [grid map-entry]
  (let [coordinates (key map-entry) delta (val map-entry)]
    (assoc grid (second coordinates)
                (update (get grid (second coordinates)) (first coordinates)
                        #(if (>= (+ % delta) 10 ) 0 (+ % delta)))))
  )

(defn increment-positions
  [octopus-grid positions]
  (reduce
    increment-position
    octopus-grid
    (frequencies positions))
  )

(defn adjacent-positions
  [position]
  (remove #(or (nil? %) (= position %)) (for [x-dif (range -1 2 1) y-dif (range -1 2 1)]
     (let [x (+ x-dif (first position)) y (+ y-dif (second position))]
       (if (and (>= x 0) (>= y 0) (< x 10) (< y 10))
         (list x y))))))

(defn sub-step
  [octopus-grid-status]
  (let [new-grid (increment-positions
                   (:grid octopus-grid-status)
                   (remove (set (:flashing-octopuses octopus-grid-status))
                           (:octopuses-to-increment octopus-grid-status)))
        new-flashing-octopuses (remove (set (:flashing-octopuses octopus-grid-status)) (flashing-octopuses new-grid))]
    (if (= 0 (count new-flashing-octopuses))
      new-grid
      (sub-step (hash-map
         :grid new-grid
         :flashing-octopuses (distinct (concat (:flashing-octopuses octopus-grid-status) new-flashing-octopuses))
         :octopuses-to-increment (apply concat (for [octopus new-flashing-octopuses] (adjacent-positions octopus)))))
      )
    )
  )

(defn apply-step
  [octopus-grid]
  (sub-step(hash-map
      :grid octopus-grid
      :flashing-octopuses '()
      :octopuses-to-increment (for [x (range (count (first (vals octopus-grid))))
                                    y (range (count octopus-grid))]
                                (list x y))
      )
    )
  )

(defn count-flashes-one-grid
  [grid]
  (apply + (remove nil? (for [line (vals grid)] (get (frequencies (vals line)) 0)))))

(defn count-flashes
  [list-of-outcomes]
  (apply + (remove nil? (for [grid list-of-outcomes] (count-flashes-one-grid grid)))))

(defn day-11
  [file-name step]
  (count-flashes (take (+ 1 step) (iterate apply-step (parse-octopuses file-name)))))

(defn day-11-part-two
  [file-name]
  (reduce
    (fn
      [octopus-grid count]
      (let [new-grid (apply-step octopus-grid)
            flashes (count-flashes-one-grid new-grid)]
          (if (= 100 flashes)
            (reduced (+ 1 count))
            new-grid
            )

        )
      )
    (parse-octopuses file-name)
    (range)
    )
  )