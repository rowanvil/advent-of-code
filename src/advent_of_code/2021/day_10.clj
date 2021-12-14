(ns advent-of-code.2021.day-10
  (:require [advent-of-code.utils :as utils]))

(def bracket-pair
  {\( \)
   \) \(
   \{ \}
   \} \{
   \[ \]
   \] \[
   \< \>
   \> \<
   }
  )

(defn process-next-character-part-one
  [current-list incoming-character]
  (if (utils/in? '(\( \[ \{ \<) incoming-character)
    (conj current-list incoming-character)
    (let [latest-character-of-list (first current-list)]
      (if (= latest-character-of-list (get bracket-pair incoming-character))
        (drop 1 current-list)
        (case incoming-character
          \) 3
          \] 57
          \} 1197
          \> 25137
          )
        )
      )
    )
  )

(defn process-navigation-line-part-one
  [navigation-line]

  (let [conculsion (reduce
                     (fn [current-list incoming-character]
                       (let [responce (process-next-character-part-one current-list incoming-character)]
                         (if (int? responce)
                           (reduced responce)
                           responce
                           ))
                       )
                     '()
                     navigation-line
                     )]
    (if (int? conculsion)
      conculsion
      0
      )
    )
  )

(defn day-10 [file-name]
  (apply + (for [line (utils/read-file-line-by-line file-name)]
     (process-navigation-line-part-one (seq line))
     ))
  )

;---------------------------------------------------------------------------------

(defn process-next-character-part-two
  [current-list incoming-character]
  (if (utils/in? '(\( \[ \{ \<) incoming-character)
    (conj current-list incoming-character)
    (let [latest-character-of-list (first current-list)]
      (if (= latest-character-of-list (get bracket-pair incoming-character))
        (drop 1 current-list)
        0
      )
    )
    )
  )


(defn process-navigation-line-part-two
  [navigation-line]

  (let [conculsion (reduce
                     (fn [current-list incoming-character]
                       (let [responce (process-next-character-part-two current-list incoming-character)]
                         (if (int? responce)
                           (reduced responce)
                           responce
                           ))
                       )
                     '()
                     navigation-line
                     )]
    (if (int? conculsion)
      conculsion
      (reduce
        (fn
          [current-value incoming-character]
          (+ (* 5 current-value)
             (case incoming-character
               \( 1
               \[ 2
               \{ 3
               \< 4
               )
             )

          )
        0
        conculsion)
      )
    )
  )

(defn day-10-part-two [file-name]
  (let [scores (sort (remove #(= 0 %) (for [line (utils/read-file-line-by-line file-name)]
                                 (process-navigation-line-part-two (seq line))
                                 )))]
    scores
    (nth scores (/ (- (count scores) 1) 2))
    )
  )