(ns advent-of-code.2021.day-6
  (:require [advent-of-code.utils :as utils]))

(defn parse-fish-input [file-name]
  (let [fishes (frequencies (utils/read-file-with-separator-to-ints file-name #","))]
    (reduce (fn [m k] (assoc m (keyword (str k)) (or (get  fishes k) 0))) {} (range 9))))

(defn apply-day [fishes]
  {:0 (:1 fishes), :1 (:2 fishes), :2 (:3 fishes), :3 (:4 fishes), :4 (:5 fishes), :5 (:6 fishes),
   :6 (+ (:7 fishes) (:0 fishes)) :7 (:8 fishes) :8 (:0 fishes)})

(defn day-six-part-two [filename generations]
  (apply + (vals (last (take (+ 1 generations) (iterate apply-day (parse-fish-input filename)))))))

