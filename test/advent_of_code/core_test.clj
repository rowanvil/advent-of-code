(ns advent-of-code.core-test
  (:require [clojure.test :refer :all]
            [advent-of-code.2021.day-1 :as one]
            [advent-of-code.2021.day-2 :as two]
            [advent-of-code.2021.day-3 :as three]
            [advent-of-code.2021.day-4 :as four]
            [advent-of-code.2021.day-5 :as five]
            [advent-of-code.2021.day-6 :as six]
            [advent-of-code.2021.day-7 :as seven]
            [advent-of-code.2021.day-8 :as eight]
            [advent-of-code.2021.day-9 :as nine]
            [advent-of-code.2021.day-10 :as ten]
            [advent-of-code.2021.day-11 :as eleven]
            [advent-of-code.2021.day-12 :as twelve]
            [advent-of-code.2021.day-13 :as thirteen]
            [advent-of-code.2021.day-14 :as fourteen]
            [advent-of-code.2021.day-15 :as fifteen]
            ))

(defn test-file [n]
  (str "resources/2021/day-" n "/test.txt"))

(deftest day-one
  (is (= 7 (one/day-one (test-file 1))))
  (is (= 5 (one/day-one-part-two (test-file 1)))))

(deftest day-two
  (is (= 150 (two/day-two (test-file 2))))
  (is (= 900 (two/day-two-part-two (test-file 2)))))

(deftest day-three
  (is (= 198 (three/day-three (test-file 3))))
  (is (= 230 (three/day-three-part-two (test-file 3)))))

(deftest day-four
  (is (= 4512
         (four/day-four "resources/2021/day-4/test-boards.txt" "resources/2021/day-4/test-draws.txt")))
  (is (= 1924
         (four/day-four-part-two "resources/2021/day-4/test-boards.txt" "resources/2021/day-4/test-draws.txt"))))

(deftest day-five
  (is (= 5 (five/day-five (test-file 5))))
  (is (= 12 (five/day-five-part-two (test-file 5)))))

(deftest day-six
  (is (= 5934 (six/day-six-part-two (test-file 6) 80)))
  (is (= 26984457539 (six/day-six-part-two (test-file 6) 256))))

(deftest day-seven
  (is (= 168 (seven/day-seven (test-file 7)))))

(deftest day-eight
  (is (= 26 (eight/day-eight (test-file 8))))
  (is (= 61229 (eight/day-eight-part-two (test-file 8)))))

(deftest day-nine
  (is (= 15 (nine/day-9 (test-file 9))))
  (is (= 1134 (nine/day-9-part-2 (test-file 9)))))

(deftest day-ten
  (is (= 26397 (ten/day-10 (test-file 10))))
  (is (= 288957 (ten/day-10-part-two (test-file 10)))))

(deftest day-eleven
  (is (= 1656 (eleven/day-11 (test-file 11) 100)))
  (is (= 195 (eleven/day-11-part-two (test-file 11)))))

(deftest day-twelve
  (is (= 10 (twelve/day-12 "resources/2021/day-12/test-1.txt")))
  (is (= 19 (twelve/day-12 "resources/2021/day-12/test-2.txt")))
  (is (= 226 (twelve/day-12 "resources/2021/day-12/test-3.txt")))
  (is (= 36 (twelve/day-12-part-two "resources/2021/day-12/test-1.txt")))
  (is (= 103 (twelve/day-12-part-two "resources/2021/day-12/test-2.txt")))
  (is (= 3509 (twelve/day-12-part-two "resources/2021/day-12/test-3.txt"))))

(deftest day-thirteen
  (is (= 17 (thirteen/day-13 "resources/2021/day-13/test-dots.txt" "resources/2021/day-13/test-folds.txt")))
  (is (= '("0 0 0 0 0" "0       0" "0       0" "0       0" "0 0 0 0 0")
         (thirteen/day-13-part-two "resources/2021/day-13/test-dots.txt" "resources/2021/day-13/test-folds.txt"))))

(deftest day-fourteen
  (is (= 1588
         (fourteen/day-14 "resources/2021/day-14/test-template.txt" "resources/2021/day-14/test-rules.txt" 10)))
  (is (= 2188189693529
         (fourteen/day-14 "resources/2021/day-14/test-template.txt" "resources/2021/day-14/test-rules.txt" 40))))

(deftest day-fifteen
  (is (= 40 (val (last(fifteen/day-15 (test-file 15))))))
  (is (= 315 (val (fifteen/day-15-part-two (test-file 15))))))
