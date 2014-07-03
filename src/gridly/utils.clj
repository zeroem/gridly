(ns gridly.utils)

(defn ordered-set-of-points [points]
  (->> points
       set
       (sort-by #(first %))))

(def x-component first)
(def y-component second)

(defn delta-fn [f]
  (fn [pair]
    (->> [(:right pair) (:left pair)]
         (map f)
         (apply -))))

(def dx (delta-fn x-component))
(def dy (delta-fn y-component))

(defn x-range [pair]
  (->> pair
       ((juxt :left :right))
       (mapv x-component)))

(defn point-pair [points]
  {:left (first points)
   :right (second points)})

(defn above-range? [pair]
  (nil? (:right pair)))

(defn below-range? [pair]
  (nil? (:left pair)))

(defn pairs
  "Generate a list of :left and :right pairs of points, assuming the points have already been ordered.
  two special pairs are added that represent 'below' (:left nil) and 'above' (:right nil) the range given."

  [points]

  (loop [acc [{:right (first points) :alt (point-pair points)}]
         remaining points]
    (if (= (count remaining) 1)
      (conj acc {:left (first remaining) :alt (peek acc)})
      (recur (conj acc (point-pair remaining))
             (rest remaining)))))

(defn pair-contains? [x pair]
  (let [[min-x max-x] (x-range pair)]
    (cond
      (nil? min-x) (< x max-x)
      (nil? max-x) (> x min-x)
      :else (and (>= x min-x)
                 (<= x max-x)))))

(defn x-offset [x point]
  (- x (x-component point)))

(defn base-point [pair]
  (if-let [base (:left pair)]
    base
    (:right pair)))

(defn slope [pair]
  (let [pair (if (and (:left pair)
                      (:right pair))
               pair
               (:alt pair))]
    (/ (dy pair) (dy pair))))

(defn pairs-containing [x pairs]
  (filterv (partial pair-contains? x) pairs))

(defn pair-containing [x pairs]
  (let [matching (pairs-containing x pairs)]
    ; Multiple or zero matching ranges is an "undefined" result
    (if (= (count (take 2 matching)) 1)
      (first matching))))
