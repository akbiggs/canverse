(ns canverse.test.helpers-test
  (:use [canverse.helpers]
        [clojure.test]))

(deftest test-lerp
  (is (= 20.0 (lerp 0 40 0.5)))
  (is (= 0 (clamped-lerp 0 40 -1)))
  (is (= 40 (clamped-lerp 0 40 2))))

(deftest test-find-where
  (is (= 1 (find-where #(= -4 (- % 5)) [3 2 1]))))

(deftest test-in-range
  (is (in-range? 3 1 5))
  (is (not (in-range? 0 1 5))))

(deftest test-midpoint
  (is (= (midpoint 0 50) 25))
  (is (= (midpoint 100 200)) 150))

(run-all-tests #"canverse.test.helpers-test")
