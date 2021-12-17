(ns advent-of-code.2021.day-1
  (:require [advent-of-code.utils :as utils]))

(defn count-increases [measurements]
  (count (filter pos? (map compare (drop 1 measurements) measurements))))

(defn day-one [file-name]
  (let [data (utils/read-file-with-separator-to-ints file-name #"\n")]
    (count-increases data)))

(defn day-one-part-two [file-name]
  (let [data (utils/read-file-with-separator-to-ints file-name #"\n")]
           (count-increases (map + data (drop 1 data) (drop 2 data)))))
