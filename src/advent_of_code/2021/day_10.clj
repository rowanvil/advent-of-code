(ns advent-of-code.2021.day-10
  (:require [advent-of-code.utils :as utils]))

(def bracket-pair
  {\( \) \) \( \{ \} \} \{ \[ \] \] \[ \< \> \> \<})

(defn process-next-character-part-one [current-list incoming-character]
  (if (utils/in? '(\( \[ \{ \<) incoming-character)
    (conj current-list incoming-character)
    (let [latest-character-of-list (first current-list)]
      (if (= latest-character-of-list (get bracket-pair incoming-character))
        (drop 1 current-list)
        (case incoming-character \) 3 \] 57 \} 1197 \> 25137)))))

(defn reduce-navigation-line-part-one [navigation-line]
  (reduce
    (fn [current-list incoming-character]
      (let [response (process-next-character-part-one current-list incoming-character)]
        (if (int? response)
          (reduced response)
          response)))
    '()
    navigation-line))

(defn process-navigation-line-part-one [navigation-line]
  (let [conclusion (reduce-navigation-line-part-one navigation-line)]
    (if (int? conclusion) conclusion 0)))

(defn day-10 [file-name]
  (apply + (for [line (utils/read-file-line-by-line file-name)]
     (process-navigation-line-part-one (seq line)))))

;---------------------------------------------------------------------------------

(defn process-next-character-part-two [current-list incoming-character]
  (if (utils/in? '(\( \[ \{ \<) incoming-character)
    (conj current-list incoming-character)
    (let [latest-character-of-list (first current-list)]
      (if (= latest-character-of-list (get bracket-pair incoming-character))
        (drop 1 current-list)
        0))))

(defn reduce-navigation-line-part-two [navigation-line]
  (reduce
    (fn [current-list incoming-character]
      (let [response (process-next-character-part-two current-list incoming-character)]
        (if (int? response)
          (reduced response)
          response)))
    '()
    navigation-line))

(defn process-navigation-line-part-two [navigation-line]
  (let [conclusion (reduce-navigation-line-part-two navigation-line)]
    (if (int? conclusion)
      conclusion
      (reduce
        (fn [current-value incoming-character]
          (+ (* 5 current-value)
             (case incoming-character \( 1 \[ 2 \{ 3 \< 4)))
        0
        conclusion))))

(defn day-10-part-two [file-name]
  (let [scores (sort (remove #(= 0 %)
                             (for [line (utils/read-file-line-by-line file-name)]
                                 (process-navigation-line-part-two (seq line)))))]
    (nth scores (/ (- (count scores) 1) 2))))