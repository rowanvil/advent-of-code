(ns advent-of-code.2021.day-13
  (:require [advent-of-code.utils :as utils]
            [clojure.string :as str]))

(defn parse-folds
  [file-name]
  (for [line (utils/read-file-line-by-line file-name)]
    {:direction (if (str/includes? line "y") "y" "x")
     :line      (utils/parse-int (second (str/split line #"=")))}))

(defn fold-y
  [dots line]
  (distinct (for [dot dots]
              {:x (:x dot)
               :y (- line (Math/abs (- line (:y dot) )))}
              )))

(defn fold-x
  [dots line]
  (distinct (for [dot dots]
              {:x (- line (Math/abs (- line (:x dot))))
               :y (:y dot)})))

(defn day-13
  [dots-file folds-file]
  (let [dots (utils/read-file-to-coordinates dots-file)
        fold (first (parse-folds folds-file))]
    (count (if (= "y" (:direction fold))
             (fold-y dots (:line fold))
             (fold-x dots (:line fold))
             ))))

(defn print-matrix
  [list-of-points]
  (let [x-max (apply max (for [point list-of-points] (:x point)))
        y-max (apply max (for [point list-of-points] (:y point)))]
    (for [y (range (+ 1 y-max))]
      (str/join " " (for [x (range (+ 1 x-max))]
                        (if (utils/in? list-of-points {:x x :y y}) "0" " "))))))

(defn day-13-part-two
  [dots-file folds-file]
  (let [dots (utils/read-file-to-coordinates dots-file)
        folds (parse-folds folds-file)]
    (print-matrix (reduce
                    (fn [dots fold]
                      (if (= "y" (:direction fold))
                        (fold-y dots (:line fold))
                        (fold-x dots (:line fold))
                        )
                      )
                    dots
                    folds))))

