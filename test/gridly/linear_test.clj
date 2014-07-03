(ns gridly.linear-test
  (:require [clojure.test :refer :all]
            [gridly.linear :refer :all]))

(deftest linear-inerpolation-fn
  (testing "two points"
    (let [lfn (linear [[1 2] [3 4]])]
      (is (= (lfn 2) 3))
      (is (= (lfn 0) 1))
      (is (= (lfn 4) 5)))))
