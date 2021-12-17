(ns advent-of-code.2021.day-7
  (:require [advent-of-code.utils :as utils]))

(defn parse-starting-points [file-name]
  (frequencies (utils/read-file-with-separator-to-ints file-name #",")))

(defn calculate-fuel-need [meeting-point point]
  (* (val point) (apply + (utils/inclusive-range (utils/abs-dif meeting-point (key point))))))

(defn day-seven [file-name]
  (let [starting-points (parse-starting-points file-name)]
    (apply min (for [meeting-point (utils/inclusive-range (apply max (keys starting-points)))]
                 (apply + (for [point starting-points]
                            (if (= point meeting-point) 0 (calculate-fuel-need meeting-point point))))))))
