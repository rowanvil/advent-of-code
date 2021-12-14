(ns advent-of-code.2021.day-2
  (:require [clojure.string :as str]
            [advent-of-code.utils :as utils]))

(defn line-to-map
  [line]
  (apply hash-map (str/split line #" "))
  )

(defn read-as-maps
  [data-file]
  (map
    line-to-map
    (utils/read-file-line-by-line data-file))
  )

(defn day-two
  [data-file]
  (let [result (reduce
                 (fn [directions step]
                   (let [direction (first (keys step))]
                     (update directions direction #(+ % (Integer/parseInt (get step direction))))
                     ))
                 {"forward" 0, "down" 0, "up" 0}
                 (read-as-maps data-file))]
    (* (get result "forward") (- (get result "down") (get result "up") )))
  )

(defn day-two-part-two
  [data-file]
  (let [result (reduce
                 (fn [directions step]
                   (let [direction (first (keys step))
                         value (Integer/parseInt (get step direction))]
                     (case direction
                       "up"      (update directions :aim #(- % value))
                       "down"    (update directions :aim #(+ % value))
                       "forward" {:aim (:aim directions)
                                  :depth (+ (:depth directions) (* (:aim directions) value))
                                  :horizontal (+ (:horizontal directions) value)}
                       )
                     )
                   )
                 {:aim 0, :horizontal 0, :depth 0}
                 (read-as-maps data-file))]
    (* (:depth result) (:horizontal result )))
  )
