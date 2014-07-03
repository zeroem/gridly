(ns gridly.utils-test
  (:require [clojure.test :refer :all]
            [gridly.utils :refer :all]))

(deftest deltas-test
  (testing "dx"
    (let [d (dx {:left [1] :right [2]})]
      (is (= 1 d))))

  (testing "dy"
    (let [d (dy {:left [nil 1] :right [nil 2]})]
      (is (= 1 d)))))

(deftest pairs-test
  (testing "Points are ordered properly"
    (is (= [[1 1] [2 1] [3 1]] (ordered-set-of-points [[3 1] [2 1] [1 1]]))))

  (testing "Points are paired properly"
    (is (= [{:right [1 1]
             :alt {:left [1 1] :right [2 2]}}
            {:left [1 1]
             :right [2 2]}
            {:left [2 2]
             :alt {:left [1 1] :right [2 2]}}]
          (pairs [[1 1] [2 2]]))))

  (testing "bottom-most range"
    (is (pair-contains? 1 {:right [2 nil]}))
    (is (not (pair-contains? 1 {:right [0 nil]}))))

  (testing "top-most range"
    (is (pair-contains? 1 {:left [0 nil]}))
    (is (not (pair-contains? 1 {:left [2 nil]}))))

  (testing "generic range"
    (is (pair-contains? 1 {:left [0 nil] :right [2 nil]}))
    (is (not (pair-contains? 3 {:left [0 nil] :right [2 nil]})))))
