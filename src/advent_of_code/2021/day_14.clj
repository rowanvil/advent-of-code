(ns advent-of-code.2021.day-14
  (:require [advent-of-code.utils :as utils]
            [clojure.string :as str]))

(defn parse-rules [file-name]
  (apply hash-map
         (apply concat
                (for [line (utils/read-file-line-by-line file-name)]
                  (let [[pattern insert] (str/split line #" -> ")]
                    (list pattern
                          (list (str (first pattern) insert)
                                (str insert (second pattern)))))))))

(defn parse-template [file-name]
  (let [template (slurp file-name)]
    (reduce
      (fn [m k]
        (utils/assoc-or-inc m k 1))
      {}
      (for [x (range (- (count template) 1))]
        (subs template x (+ 2 x))))))

(defn apply-transformation-to-map
  [template-map rules]
  (reduce
    (fn [growing-map entry]
      (let [pattern (key entry)
            count (val entry)]
        (if (utils/in? (keys rules) pattern)
          (reduce (fn [m k] (utils/assoc-or-inc m k count))
                  growing-map
                  (get rules pattern)
                  ))))
    {}
    template-map
    )
  )

(defn pairs-to-chars
  [char-totals pair-entry]
  (reduce (fn [m k] (utils/assoc-or-inc m k (val pair-entry)))
          char-totals
          (seq (key pair-entry))))

(defn fix-ends [m k v]
  (utils/assoc-or-inc m k (if (odd? v) (+ v 1) v)))

(defn pairs-map-to-char-totals
  [pairs-map]
  (let [char-counts (reduce pairs-to-chars {} pairs-map)]
    (let [char-count-with-ends-fixed (reduce-kv fix-ends {} char-counts)]
      (reduce-kv #(utils/assoc-or-inc %1 %2 (/ %3 2)) {} char-count-with-ends-fixed))))

(defn get-max-min
  [counts]
  (- (apply max counts) (apply min counts))
  )

(defn apply-transformation-n-times
  [template rules steps]
  (last (take (+ 1 steps) (iterate #(apply-transformation-to-map % rules) template)))
  )

(defn day-14 [template-file rules-file steps]
  (let [template (parse-template template-file)
        rules (parse-rules rules-file)]
    (get-max-min (vals (pairs-map-to-char-totals (apply-transformation-n-times template rules steps))))
    )
  )

