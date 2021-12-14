(ns advent-of-code.2021.day-3
  (:require
    [advent-of-code.utils :as utils]))

(defn file-to-list-of-list-of-chars
  [file-name]
  (for [line (utils/read-file-line-by-line file-name)]
    (for [digit (seq line)] digit))
  )

(defn two-to-the_power-of
  [power]
  (reduce * (repeat power 2))
  )

(defn binary-value
  [int-list]
  (apply + (map-indexed #(* %2 (two-to-the_power-of %1)) (reverse int-list
                                                                  ))))

(defn gamma
  [int-list]
  (binary-value (for [x int-list]
     (cond
       (pos? x) 1
       :else 0))))

(defn epsilon
  [int-list]
  (binary-value (for [x int-list]
                  (cond
                    (pos? x) 0
                    :else 1))))

(defn calculate-counts
  [data]
  (for [position-list (utils/transpose data)]
    (reduce
      (fn
        [count data]
        (case data
          \0 (dec count)
          \1 (inc count)))
      0
      position-list
      )))

(defn day-three
  [file-name]
  (let [counts
        (calculate-counts (file-to-list-of-list-of-chars file-name))]
    (* (gamma counts) (epsilon counts)))
  )

(defn rating-generator
  [readings filter-function]
  (first (reduce
     (fn
       [readings entry]
       (if (= 1 (count readings))
         readings
         (let [position-count (nth (calculate-counts readings) entry)
               modal-position-value (if (>= position-count 0) \1 \0)]
           (filter #(filter-function modal-position-value (nth % entry)) readings)
           ))
       )
     readings
     (range 0 (count (first readings)))
     ))
  )

(defn char-list-to-value
  [char-list]
  (binary-value (for [x char-list]
     (if (= x \1) 1 0)))
  )

(defn day-three-part-two
  [file-name]
  (let [readings (file-to-list-of-list-of-chars file-name)]
    (* (char-list-to-value (rating-generator readings =))
       (char-list-to-value (rating-generator readings not=)))
    )
  )
