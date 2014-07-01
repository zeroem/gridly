(ns gridly.core)

(defn ordered-set-of-points [points]
  (->> points
       set
       (sort-by #(first %))))

(defn delta-fn [f]
  (fn [p1 p2]
    (->> [p2 p1]
         (map f)
         (apply -))))

(def dx (delta-fn first))
(def dy (delta-fn second))

(defn slope [p1 p2]
  (let [dx (dx p1 p2)
        dy (dy p1 p2)]
    (if-not (= dx 0)
      (/ dy dx))))

(defn next-slope [points]
  (apply slope (take 2 points)))

(defn in-range [x {min-r :min max-r :max}]
  (cond
    (nil? min-r) (< x max-r)
    (nil? max-r) (> x min-r)
    :else (and (>= x min-r)
               (<= x max-r))))

(defn first-y [points]
  (second (first points)))

(def first-x ffirst)
(defn next-x [points] (ffirst (next points)))

(defn ranges-with-slope [points]
  (loop [acc [{:max (first-x points)
               :m (next-slope points)
               :base (first-y points)}]
         remaining points]
    (if (= (count remaining) 1)
      (conj acc {:min (first-x remaining)
                 :m (:m (peek acc))
                 :base (first-y remaining)})
      (recur (conj acc
                   {:min (first-x remaining)
                    :max (next-x remaining)
                    :m (next-slope remaining)
                    :base (first-y remaining)})
             (rest remaining)))))

(defn delta-range [x {min-r :min max-r :max}]
  (let [min-r (if min-r min-r max-r)]
    (- x min-r)))

(defn ranges-containing [x rs]
  (filterv (partial in-range x) rs))

(defn range-containing [x rs]
  (let [matching (ranges-containing x rs)]
    ; Multiple or zero matching ranges is an "undefined" result
    (if (= (count (take 2 matching)) 1)
      (first matching))))

(defn linear 
  "Generate a linear interpolation function for the given set of two or more points"

  [points]

  {:pre [(>= (count points) 2)]}

  (let [ranges (-> (ordered-set-of-points points)
                   ranges-with-slope)]
    (fn [x]
      (if-let [r (range-containing x ranges)]
        (-> x
            (delta-range r)
            (* (:m r))
            (+ (:base r)))))))
