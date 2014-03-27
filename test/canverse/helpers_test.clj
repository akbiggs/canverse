(ns canverse.test.helpers-test
  (:use [canverse.helpers]
        [clojure.test]))

(deftest test-find-where
  (is (= 1 (find-where #(= -4 (- % 5)) [3 2 1]))))

(deftest test-in-range
  (is (in-range? 3 1 5))
  (is (not (in-range? 0 1 5))))

(run-all-tests #"canverse.test.helpers-test")