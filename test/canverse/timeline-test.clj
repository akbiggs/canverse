(ns canverse.test.timeline-test
  (:require [canverse.point :as point]
            [canverse.synths :as synths]
            [canverse.timeline :as timeline]
            [overtone.live :as o])
  (:use clojure.test))

(def test-timeline (timeline/create (point/create 50 50)
                                    (point/create 300 50)
                                    30000))
(def test-node (synths/oksaw :freq 60 :amp 0.5))
(def with-note-added (timeline/add-note-from-node test-node test-timeline))

(deftest adding-note
  (let [new-note (first (:history with-note-added))]
    (are [x y] (= (x new-note) y)
         :freq 60.0
         :amp 0.5
         :relative-time 30000
         :node-id (o/to-sc-id test-node))))

(deftest movement
  (is (= (timeline/get-movement 5000 test-timeline) 50)))

(deftest history-progression
  (let [after-history-progressed (timeline/progress-history 5000 with-note-added)
        with-another-note (timeline/add-note-from-node (synths/oksaw :freq 65 :amp 0.5) after-history-progressed)
        history (:history with-another-note)]
    (are [x y z] (= (y x) z)
         (first history) :relative-time 30000
         (second history) :relative-time 25000)))

(run-all-tests #"canverse.test.timeline-test")
