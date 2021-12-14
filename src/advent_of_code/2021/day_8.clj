(ns advent-of-code.2021.day-8
  (:require [advent-of-code.utils :as utils]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn string-to-list-of-sorted-char-lists
  [string-value]
  (for [entry (str/split (str/trim string-value) #" ")]
    (sort (seq entry))
    )
  )

(defn sort-signals
  "Map the 10 unique signals to the digit they represent"
  [unique-signals]
  (reduce
    (fn
      [known-values signal]
      (case (count signal)
        2 (assoc known-values 1 signal)
        3 (assoc known-values 7 signal)
        4 (assoc known-values 4 signal)
        5 (if (utils/subset? signal (get known-values 1))
            (assoc known-values 3 signal)
            (if (utils/subset?  signal (utils/remove-entries (get known-values 4) (get known-values 1)))
              (assoc known-values 5 signal)
              (assoc known-values 2 signal)
              )
            )
        6 (if (utils/subset? signal (get known-values 3))
            (assoc known-values 9 signal)
            (if (utils/subset? signal (get known-values 1) )
              (assoc known-values 0 signal)
              (assoc known-values 6 signal)
              )
            )
        7 (assoc known-values 8 signal)
        :else known-values))
    {}
    (sort-by count unique-signals))
  )

(defn decode-value
  [signal-map values]
  (let [decoder (set/map-invert signal-map)]
    (for [value values]
      (get decoder value)))
  )

(defn get-output
  [file-name]
  (for [line (utils/read-file-line-by-line file-name)]
    (let [[signals values] (str/split line #"\|")]
      (let [signal-map (sort-signals (string-to-list-of-sorted-char-lists signals))]
        (decode-value signal-map (string-to-list-of-sorted-char-lists values))
        )
      )
    )
  )

(defn day-eight
  [file-name]
  (let [counts (frequencies (flatten (get-output file-name)))]
    (+ (get counts 1) (get counts 4) (get counts 7) (get counts 8)))
  )

(defn day-eight-part-two
  [file-name]
  (apply + (for [output (get-output file-name)]
     (utils/list-of-digits-to-int output)))
  )
