(ns gridly.constant-test
  (:require [clojure.test :refer :all]
            [gridly.constant :refer :all]))

(deftest constant-test
  (let [points [[1 1] [3 3] [6 6]]
        cfn (constant points)]
    (is (= 1 (cfn 1)))
    (is (= 6 (cfn 5)))
    ; If equidistant, pick the left side.
    ; completely arbitrary, no guide for what to do in this case
    (is (= 1 (cfn 2)))))
