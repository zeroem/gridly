(ns gridly.constant
  (:require [gridly.utils :refer :all]))

(defn nearest-point [x pair]
  (cond
    (below-range? pair) (:right pair)
    (above-range? pair) (:left pair)
    :else (let [left-offset (Math/abs (x-offset x (:left pair)))
                right-offset (Math/abs (x-offset x (:right pair)))]
            (if (> left-offset right-offset)
              (:right pair)
              (:left pair)))))

(defn constant
  "Piecewise constant interpolation function generator"
  [points]

  (if (= (count points) 1)
    (constantly (-> points
                    first
                    y-component))
    (let [pairs (-> points
                    ordered-set-of-points
                    pairs)]
      (fn [x]
        (if-let [pair (pair-containing x pairs)]
          (y-component (nearest-point x pair)))))))
