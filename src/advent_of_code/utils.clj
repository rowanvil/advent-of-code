(ns advent-of-code.utils
  (:require  [clojure.java.io :as io]
             [clojure.string :as str]))

(defn seq-add [seq-1 seq-2]
  (for [n (range (count seq-1))] (+ (nth seq-1 n) (nth seq-2 n))))

(defn inclusive-range [max]
  (range (+ 1 max)))

(defn abs-dif [x y]
  (Math/abs (- x y)))

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn subset? [coll-one coll-two]
  (every? identity (for [elm coll-two] (in? coll-one elm))))

(defn remove-entries
  "true if coll contains elm"
  [coll entries-to-remove]
  (remove #(in? entries-to-remove %) coll))

(defn assoc-or-inc [m k n]
  (if (in? (keys m) k) (update m k #(+ n %)) (assoc m k n)))

(defn list-of-digits-to-int
  [digits]
  (Integer. (re-find  #"\d+" (apply str digits))))

(defn parse-int [s] (Integer/parseInt s))

(defn transpose
  [list-of-lists]
  (apply map list list-of-lists))

(defn string-to-list-of-ints [digit-string]
  (for [digit (str/split digit-string #"")]  (parse-int digit)))

(defn read-file-with-separator-to-strings [file-name divisor]
  (str/split (slurp file-name) divisor))

(defn read-file-with-separator-to-ints [file-name divisor]
  (for [number (read-file-with-separator-to-strings file-name divisor)] (parse-int number)))

(defn read-file-line-by-line [data-file]
  (with-open [rdr (io/reader data-file)] (reduce conj [] (line-seq rdr))))

(defn read-file-to-coordinates [file-name]
  (for [line (read-file-line-by-line file-name)]
    (let [[x y] (str/split line #",")]
      {:x (parse-int x) :y (parse-int y)})))

(defn list-to-numbered-hash-map [list-to-hash fn]
  (apply sorted-map (flatten (for [n (range (count list-to-hash))] [n (fn (nth list-to-hash n))]))))

(defn list-to-map-of-maps [grid]
  (list-to-numbered-hash-map grid #(list-to-numbered-hash-map % identity)))

(defn parse-grid-of-values [file-name]
  (list-to-map-of-maps (for [line (read-file-line-by-line file-name)] (string-to-list-of-ints line))))

(defn update-costs [graph costs unvisited curr]
  (let [curr-cost (get costs curr)]
    (reduce-kv
      (fn [c nbr nbr-cost]
        (if (unvisited nbr)
          (update-in c [nbr] min (+ curr-cost nbr-cost))
          c))
      costs
      (get graph curr))))

(defn dijkstra
  ([graph start]
   (dijkstra graph start nil))
  ([graph start end]
   (loop [costs (assoc (zipmap (keys graph) (repeat Double/POSITIVE_INFINITY)) start 0)
          curr start
          unvisited (disj (apply hash-set (keys graph)) start)]
     (cond
       (= curr end) (select-keys costs [end])
       (or (empty? unvisited) (= Double/POSITIVE_INFINITY (get costs curr))) costs

       :else (let [next-costs (update-costs graph costs unvisited curr)
                   next-node (apply min-key next-costs unvisited)]
               (recur next-costs next-node (disj unvisited next-node)))))))
