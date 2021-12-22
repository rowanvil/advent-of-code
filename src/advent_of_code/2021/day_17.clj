(ns advent-of-code.2021.day-17)

(defn find-x-vel-step-pairs [x-min x-max]
  (flatten (remove empty? (for [x (range 1 (inc x-max))]
                    (drop-while #(< (:x %) x-min)
                                (take-while #(and
                                               (>= (- (:x-vel %) (:n %)) 0)
                                               (<= (:x %) x-max))
                                            (for [n (range)]
                                              {:x-vel x
                                               :n     n
                                               :x     (- (* x n) (apply + (range n)))})))))))

(defn find-possible-x-vel [x-min x-max]
  (distinct (map #(hash-map :x (:x-vel %)) (find-x-vel-step-pairs x-min x-max))))

(defn find-drop-shot-x-data [x-data]
  (remove nil? (for [x x-data]
     (if (= (:n x) (:x-vel x)) x))))

(defn apply-step [m]
  {:x          (if (= 0 (:x m)) 0 (dec (:x m)))
   :y          (dec (:y m))
   :x-position (+ (:x-position m) (:x m))
   :y-position (+ (:y-position m) (:y m))})

(defn all-shots [x-data x-min x-max y-min y-max]
  (remove nil? (for [x x-data
         y (range y-min (inc (- (+ y-min 1))))]
     (let [starting-point (merge x {:y y :x-position 0 :y-position 0})]
       (let [end (last (take-while #(and (>= (:y-position %) y-min)
                                         (<= (:x-position %) x-max))
                                   (iterate apply-step starting-point)))]
         (if (and (>= (:x-position end) x-min)
                  (<= (:y-position end) y-max))
           {:x (:x starting-point) :y (:y starting-point) :shot [(:x-position end) (:y-position end)]}
           nil))))))

(defn day-17-part-two [x-min x-max y-min y-max]
  (count (all-shots (find-possible-x-vel x-min x-max) x-min x-max y-min y-max)))

(defn day-17 [x-min x-max y-min y-max]
  (let [y-vel (let [x-data (find-x-vel-step-pairs x-min x-max)
                    drop-shots (find-drop-shot-x-data x-data)]
                (if (not (empty? drop-shots))
                  (- (+ y-min 1))
                  nil)
                )]
    (apply + y-vel (range y-vel))))
