(ns advent-of-code.2021.day-12
  (:require [advent-of-code.utils :as utils]
            [clojure.string :as str]))


(defn parse-input
  [file-name]
  (apply concat (for [line (utils/read-file-line-by-line file-name)]
     (let [[from-end to-end] (str/split line #"-")]
       (if (or (= "start" from-end) (= "end" to-end))
         (list (hash-map :from-end from-end :to-end to-end))
         (if (or (= "start" to-end) (= "end" from-end))
           (list (hash-map :to-end from-end :from-end to-end))
           (list (hash-map :from-end from-end :to-end to-end)
                 (hash-map :to-end from-end :from-end to-end))))))))


(defn find-connections
  [current-point vector-list]
  (remove nil? (for [path vector-list]
     (if (= current-point (:from-end path))
       (:to-end path)
       )
     ))
  )

(defn explore
  [path-so-far vector-list]
  (let [current-point (last path-so-far)
        possible-next-stops (find-connections current-point vector-list)]
    (apply concat (remove nil? (for [next-stop possible-next-stops]
                    (if (= next-stop "end")
                      (list (conj path-so-far next-stop))
                      (if (= next-stop (str/upper-case next-stop))
                        (explore (conj path-so-far next-stop) vector-list)
                        (if (not (utils/in? path-so-far next-stop))
                          (explore (conj path-so-far next-stop) vector-list)
                          nil
                          )
                        )
                      )
                    )))
    )
  )

(defn has-a-small-cave-been-visited-twice
  [path-so-far]
  (println path-so-far)
  (let [counts (frequencies path-so-far)]
    ;(println (str (and (> (val count) 1) (= (key count) (str/lower-case (key count))))))
    (and (for [count counts]
           (if (and (> (val count) 1) (= (key count) (str/lower-case (key count))))
             false
             true
           ))
    )
  )
  )

(defn explore-with-possible-single-double-visit
  [path-so-far vector-list]
  (let [current-point (last path-so-far)
        possible-next-stops (find-connections current-point vector-list)]
    (apply concat (remove nil? (for [next-stop possible-next-stops]
                                 (if (= next-stop "end")
                                   (list (conj path-so-far next-stop))
                                   (if (= next-stop (str/upper-case next-stop))
                                     (explore-with-possible-single-double-visit (conj path-so-far next-stop) vector-list)
                                     (if (not (utils/in? path-so-far next-stop))
                                       (explore-with-possible-single-double-visit (conj path-so-far next-stop) vector-list)
                                       (explore (conj path-so-far next-stop) vector-list)
                                       )
                                     )
                                   ))))))

(defn day-12
  [file-name]
  (let [vector-list (parse-input file-name)]
    (count (explore (vec '("start")) vector-list))
    )
  )

(defn day-12-part-two
  [file-name]
  (let [vector-list (parse-input file-name)]
    (count (explore-with-possible-single-double-visit (vec '("start")) vector-list))
    )
  )