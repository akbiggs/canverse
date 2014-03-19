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
(def after-history-progressed (timeline/progress-history 5000 with-note-added))
(def with-another-note (timeline/add-note-from-node (synths/oksaw :freq 65 :amp 0.5) after-history-progressed))
(def with-note-selected (timeline/select-node-at-position (point/create 300 100) with-another-note))

(deftest adding-note
  (let [new-note (first (:history with-note-added))]
    (are [key y] (= (key new-note) y)
         :freq 60.0
         :amp 0.5
         :relative-time 30000
         :node-id (o/to-sc-id test-node))))

(deftest movement
  (is (= (timeline/get-movement 5000 test-timeline) 50)))

(deftest history-progression
  (let [history (:history with-another-note)]
    (are [x y z] (= (y x) z)
         (first history) :relative-time 30000
         (second history) :relative-time 25000)))

(deftest get-node
  (let [selected-note (timeline/get-node-at 300 with-another-note)]
    (is (= (:x selected-note) 300)))
  (let [selected-note (timeline/get-node-at 250 with-another-note)]
    (is (nil? selected-note))))

(deftest get-history
  (let [history-to-loop (timeline/get-history-to-loop with-note-selected)]
    (are [x y] (= (x history-to-loop) y)
         :start-time 25000
         :notes (:loop-notes with-note-selected)
         :end-time 25000)))

(run-all-tests #"canverse.test.timeline-test")
