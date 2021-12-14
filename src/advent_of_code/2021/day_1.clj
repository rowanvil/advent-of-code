(ns advent-of-code.2021.day-1)

(defn day-one
  [measurements]
  (count (filter pos? (map compare (drop 1 measurements) measurements)))
  )

(defn day-one-part-two
  [measurements]
  (day-one (map + measurements (drop 1 measurements) (drop 2 measurements)))
  )
