(ns advent-of-code.2021.day-16
  (:require [advent-of-code.utils :as utils]
            [clojure.string :as str]))

(def get-binary
  {\0 "0000" \1 "0001" \2 "0010" \3 "0011" \4 "0100" \5 "0101" \6 "0110" \7 "0111"
   \8 "1000" \9 "1001" \A "1010" \B "1011" \C "1100" \D "1101" \E "1110" \F "1111"})

(def get-type {0 + 1 * 2 min 3 max 4 identity 5 > 6 < 7 =})

(defn binary-string-to-dec-value [binary-string]
  (if (nil? binary-string) nil (utils/binary-value (utils/string-to-list-of-ints binary-string))))

(defn hexdec-to-binary [hex-string]
  (apply str (for [c hex-string] (get get-binary c))))

(defn return-value-packet [version type package-string]
  (let [body (subs package-string 6)
        bits (partition 5 body)
        value (binary-string-to-dec-value
                (reduce (fn [s b]
                          (if (= \0 (first b))
                            (reduced (str/join "" [s (apply str (drop 1 b))]))
                            (str/join "" [s (apply str (drop 1 b))])))
                        ""
                        bits))
        used-bits (inc (count (take-while  #(not= \0 (first %)) bits)))
        remaining-string (drop (* 5 used-bits) body)]
    {:version      version
     :type         type
     :value        value
     :next-package (apply str remaining-string)}))

(defn parse-packet [package-string]
  (let [version (binary-string-to-dec-value (subs package-string 0 3))
        type (binary-string-to-dec-value (subs package-string 3 6))]
    (if (= type 4)
      (return-value-packet version  (get get-type type) package-string)
      (let [type-length-id (nth package-string 6)]
        (case type-length-id
          \0 (let [length-of-sub-packages (binary-string-to-dec-value (subs package-string 7 22))]
               {:version          version
                :type              (get get-type type)
                :sub-packets-body (apply str (take length-of-sub-packages (subs package-string 22)))
                :next-package     (apply str (drop (+ 22 length-of-sub-packages) package-string))})
          \1 {:version           version
              :type               (get get-type type)
              :no-of-sub-packets (binary-string-to-dec-value (subs package-string 7 18))
              :body              (apply str (drop 18 package-string))})))))

(defn process-packet [p]
  (if (contains? p :value)
    (let [output (dissoc p :next-package)]
      (if (empty? (str/replace (:next-package p) "0" ""))
        [output]
        (cons output (process-packet (parse-packet (:next-package p))))))
    (if (contains? p :sub-packets-body)
      (let [output {:version     (:version p)
                    :type        (:type p)
                    :sub-packets (process-packet (parse-packet (:sub-packets-body p)))}]
        (if (empty? (str/replace (:next-package p) "0" ""))
          [output]
          (cons output (process-packet (parse-packet (:next-package p))))))
      (if (contains? p :no-of-sub-packets)
        (let [packets (process-packet (parse-packet (:body p)))
              sub-packets  (take (:no-of-sub-packets p) packets)
              next-packets (drop (:no-of-sub-packets p) packets)
              output {:version     (:version p)
                      :type        (:type p)
                      :sub-packets sub-packets}]
          (if (empty? next-packets)
            [output]
            (cons output next-packets)))
        nil))))

(defn boolean-or-int [v]
  (if (= java.lang.Long (type v))
    v
    (if v 1 0)))

(defn get-version-sum [packets]
  (apply + (remove nil? (for [p packets]
                          (if (contains? p :sub-packets)
                            (+ (:version p) (get-version-sum (:sub-packets p)))
                            (:version p))))))

(defn calculate-value [packets]
  (for [packet packets]
    (if (= identity (:type packet))
      (:value packet)
      (boolean-or-int (apply (:type packet) (calculate-value (:sub-packets packet)))))))

(defn day-16 [file-name]
  (get-version-sum (process-packet (parse-packet (hexdec-to-binary (slurp file-name))))))

(defn day-16-part-two [file-name]
  (first (calculate-value (process-packet (parse-packet (hexdec-to-binary (slurp file-name)))))))
