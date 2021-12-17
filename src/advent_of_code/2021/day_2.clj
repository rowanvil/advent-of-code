(ns advent-of-code.2021.day-2
  (:require [clojure.string :as str]
            [advent-of-code.utils :as utils]))

(defn day-two [data-file]
  (let [result (reduce
                 (fn [directions step]
                   (let [[direction movement] (str/split step #" ")]
                     (update directions direction #(+ % (Integer/parseInt movement)))
                     ))
                 {"forward" 0, "down" 0, "up" 0}
                 (utils/read-file-line-by-line data-file))]
    (* (get result "forward") (- (get result "down") (get result "up") ))))

(defn day-two-part-two [data-file]
  (let [result (reduce
                 (fn [directions step]
                   (let  [[direction movement] (str/split step #" ")]
                     (case direction
                       "up"      (update directions :aim #(- % (Integer/parseInt movement)))
                       "down"    (update directions :aim #(+ % (Integer/parseInt movement)))
                       "forward" {:aim (:aim directions)
                                  :depth (+ (:depth directions) (* (:aim directions) (Integer/parseInt movement)))
                                  :horizontal (+ (:horizontal directions) (Integer/parseInt movement))}
                       )
                     )
                   )
                 {:aim 0, :horizontal 0, :depth 0}
                 (utils/read-file-line-by-line data-file))]
    (* (:depth result) (:horizontal result ))))
