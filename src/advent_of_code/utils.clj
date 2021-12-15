(ns advent-of-code.utils
  (:require  [clojure.java.io :as io]
             [clojure.string :as str])
  )

(defn inclusive-range
  [max]
  (range (+ 1 max )))

(defn abs-dif
  [x y]
  (Math/abs (- x y)))

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn subset?
  [coll-one coll-two]
  (every? identity (for [elm coll-two] (in? coll-one elm)))
  )

(defn remove-entries
  "true if coll contains elm"
  [coll entries-to-remove]
  (remove #(in? entries-to-remove %) coll)
  )

(defn assoc-or-inc
  [m k n]
  (if (in? (keys m) k)
    (update m k #(+ n %))
    (assoc m k n)))

(defn list-of-digits-to-int
  [digits]
  (Integer. (re-find  #"\d+" (apply str digits) ))
  )

(defn parse-int [s] (Integer/parseInt s))

(defn transpose
  [list-of-lists]
  (apply map list list-of-lists))

(defn string-to-list-of-ints
  [digit-string]

  (for [digit (str/split digit-string #"")]  (parse-int digit))

  )

(defn read-file-with-separator-to-strings
  [file-name divisor]
  (str/split (slurp file-name) divisor)
  )

(defn read-file-with-separator-to-ints
  [file-name divisor]
  (for [number (read-file-with-separator-to-strings file-name divisor)]
    (parse-int number))
  )

(defn read-file-line-by-line
  [data-file]
  (with-open [rdr (io/reader data-file)]
    (reduce conj [] (line-seq rdr))))


(defn read-file-to-coordinates
  [file-name]
  (for [line (read-file-line-by-line file-name)]
    (let [[x y] (str/split line #",")]
      {:x (parse-int x) :y (parse-int y)})
    )
  )
