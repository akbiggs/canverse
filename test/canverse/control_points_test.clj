(ns canverse.test.control-points-test
  (:require [canverse.inst.control-points :as control-points]
            [overtone.live :as o])
  (:use clojure.test))

(def test-control-points (control-points/create))
test-control-points
(def sustain-point (control-points/find-point :sustain test-control-points))

(deftest find-by-params-test
  (is (= (:param sustain-point) :sustain)))

(deftest map-tests
  (are [x y] (= (map x test-control-points) y)
       control-points/get-value-on-control-axis [25.0 125.0 300.0 325.0]

       (partial control-points/get-value-on-opposite-axis test-control-points)
       [0 300.0 125.0 400]))

(deftest position-test
  (is (= (control-points/get-positions test-control-points)
         [{:x 25.0 :y 0}
          {:x 125.0 :y 300.0}
          {:x 125.0 :y 300.0}
          {:x 325.0 :y 400}])))

(run-all-tests #"canverse.test.control-points-test")
