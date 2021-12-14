(ns advent-of-code.2021.day-7
  (:require
    [advent-of-code.utils :as utils]))

(defn parse-starting-points
  [file-name]
  (sort-by val (into (sorted-map) (frequencies (utils/read-file-with-separator-to-ints file-name #",")))))

(defn day-seven
  [file-name]
  (let [starting-points (parse-starting-points file-name)]
    (println starting-points)
    (apply min (for [meeting-point (utils/inclusive-range (apply max (keys starting-points)))]
                 (apply + (for [point starting-points]
                            (if (= point meeting-point)
                              0
                              (* (val point) (apply + (utils/inclusive-range (utils/abs-dif meeting-point (key point)))))
                              )
                            ))
                 )))
  )
