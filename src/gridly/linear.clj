(ns gridly.linear
  (:require [gridly.utils :refer :all]))


(defn linear 
  "Generate a linear interpolation function for the given set of two or more points"

  [points]

  {:pre [(>= (count points) 2)]}

  (let [pairs (-> points
                  ordered-set-of-points
                  pairs)]
    (fn [x]
      (if-let [pair (pair-containing x pairs)]
        (let [base (base-point pair)]
          (-> x
              (x-offset base)
              (* (slope pair))
              (+ (y-component base))))))))
