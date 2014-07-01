(ns gridly.core-test
  (:require [clojure.test :refer :all]
            [gridly.core :refer :all]))

(deftest deltas
  (testing "dx"
    (let [d (dx [1 nil] [2 nil])]
      (is (= d 1))))

  (testing "dy"
    (let [d (dy [nil 1] [nil 2])]
      (is (= d 1)))))

(deftest ranges
  (testing "bottom-most range"
    (is (in-range 1 {:max 2}))
    (is (not (in-range 1 {:max 0}))))
  
  (testing "top-most range"
    (is (in-range 1 {:min 0}))
    (is (not (in-range 1 {:min 2}))))
  
  (testing "generic range"
    (is (in-range 1 {:min 0 :max 2}))
    (is (not (in-range 3 {:min 0 :max 2}))))

  (testing "precalculate ranges"
    (let [points [[1 2] [3 4]]
          ranges (ranges-with-slope points)]
      (is (= (count ranges) 3))
      (is (= (first ranges) {:max 1 :base 2 :m 1}))
      (is (= (second ranges) {:min 1 :max 3 :base 2 :m 1}))
      (is (= (last ranges) {:min 3 :base 4 :m 1})))))

(deftest linear-inerpolation-fn
  (testing "two points"
    (let [lfn (linear [[1 2] [3 4]])]
      (is (= (lfn 2) 3))
      (is (= (lfn 0) 1))
      (is (= (lfn 4) 5)))))
