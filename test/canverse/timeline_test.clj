(ns canverse.test.timeline-test
  (:require [canverse.point :as point]
            [canverse.synths :as synths]
            [canverse.timeline :as timeline]
            [canverse.loop :as loop]
            [overtone.live :as o])
  (:use clojure.test))

(o/stop)
(def test-timeline (timeline/create (point/create 50 50)
                                    (point/create 300 50)
                                    30000))
(def test-node (synths/oksaw :freq 60 :amp 0.5))
(def with-note-added (timeline/add-note-from-node test-node test-timeline))
(def after-history-progressed (timeline/progress-history 5000 with-note-added))
(def with-another-note (timeline/add-note-from-node (synths/oksaw :freq 65 :amp 0.5) after-history-progressed))
(def with-note-selected (timeline/select-node-at-position (point/create 300 100) with-another-note))
(def with-bunch-of-notes-selected (->> test-timeline
                                       (timeline/progress-history 5000)
                                       (timeline/add-note 50 0.5 55)
                                       (timeline/progress-history 2500)
                                       (timeline/add-note 55 0.25 55)
                                       (timeline/select-node 55)))
(def test-loop (loop/create (timeline/get-history-to-loop nil with-bunch-of-notes-selected)))
(def with-another-note-selected (->> with-bunch-of-notes-selected
                                     (timeline/clear)
                                     (timeline/progress-history 1250)
                                     (timeline/add-note 50 0.5 56)
                                     (timeline/progress-history 50)
                                     (timeline/add-note 52 0.6 56)
                                     (timeline/select-node 56)))
(def test-loop-progressed (loop/progress 1250 test-loop))

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
         (first history) :relative-time 25000
         (second history) :relative-time 30000)))

(deftest get-node
  (let [selected-note (timeline/get-node-at 300 with-another-note)]
    (is (= (:x selected-note) 300)))
  (let [selected-note (timeline/get-node-at 250 with-another-note)]
    (is (nil? selected-note))))

(deftest get-history
  (let [history-to-loop (timeline/get-history-to-loop nil with-note-selected)]
    (are [x y] (= (x history-to-loop) y)
         :start-time 25000
         :notes (:loop-notes with-note-selected)
         :end-time 25000)))

(deftest create-loop
  (are [x y] (= (x test-loop) y)
       loop/get-length 2500
       loop/next-end-time 32500))

(deftest create-loop-relative-to-another
  (let [relative-loop (loop/create (timeline/get-history-to-loop [test-loop-progressed] with-another-note-selected))
        after-some-progress (loop/update! 500 relative-loop)]
    (is (= (:time-before-start relative-loop) 1250))
    (is (= (:time-before-start after-some-progress) 750))
    (is (= (:last-start-time relative-loop) 31250))))

(run-all-tests #"canverse.test.timeline-test")

(o/stop)
