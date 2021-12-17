(ns advent-of-code.2021.day-11
  (:require [advent-of-code.utils :as utils]))

(defn flashing-octopuses [octopus-grid]
  (remove nil? (for [x (range 10)
                     y (range 10)]
                 (if (= 0 (get (get octopus-grid y) x))
                   [x y]))))

(defn increment-position [grid map-entry]
  (let [coordinates (key map-entry) delta (val map-entry)]
    (assoc grid (second coordinates)
                (update (get grid (second coordinates)) (first coordinates)
                        #(if (>= (+ % delta) 10 ) 0 (+ % delta))))))

(defn increment-positions [octopus-grid positions]
  (reduce increment-position octopus-grid (frequencies positions)))

(defn adjacent-positions [position]
  (remove #(or (nil? %) (= position %))
          (for [x-dif (range -1 2 1) y-dif (range -1 2 1)]
            (let [x (+ x-dif (first position)) y (+ y-dif (second position))]
              (if (and (>= x 0) (>= y 0) (< x 10) (< y 10))
                [x y])))))

(defn sub-step [octopus-grid-status]
  (let [new-grid (increment-positions
                   (:grid octopus-grid-status)
                   (remove (set (:flashing-octopuses octopus-grid-status))
                           (:octopuses-to-increment octopus-grid-status)))
        new-flashing-octopuses (remove (set (:flashing-octopuses octopus-grid-status)) (flashing-octopuses new-grid))]
    (if (= 0 (count new-flashing-octopuses))
      new-grid
      (sub-step {:grid                   new-grid
                 :flashing-octopuses     (distinct (concat (:flashing-octopuses octopus-grid-status) new-flashing-octopuses))
                 :octopuses-to-increment (apply concat (for [octopus new-flashing-octopuses] (adjacent-positions octopus)))}))))

(defn apply-step [octopus-grid]
  (sub-step
    {:grid                   octopus-grid
     :flashing-octopuses     '()
     :octopuses-to-increment (for [x (range (count (first (vals octopus-grid))))
                                   y (range (count octopus-grid))]
                               [x y])}))

(defn count-flashes [grid]
  (apply + (remove nil? (for [line (vals grid)] (get (frequencies (vals line)) 0)))))

(defn total-flashes [list-of-grids]
  (apply + (remove nil? (for [grid list-of-grids] (count-flashes grid)))))

(defn day-11 [file-name step]
  (total-flashes (take (inc step) (iterate apply-step (utils/parse-grid-of-values file-name)))))

(defn day-11-part-two [file-name]
  (reduce
    (fn
      [octopus-grid count]
      (let [new-grid (apply-step octopus-grid)
            flashes (count-flashes new-grid)]
          (if (= 100 flashes)
            (reduced (+ 1 count))
            new-grid)))
    (utils/parse-grid-of-values file-name)
    (range)))