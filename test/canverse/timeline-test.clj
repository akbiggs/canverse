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

(deftest adding-note
  (let [with-note-added (timeline/add-note-from-node test-node test-timeline)
        new-note (first (:history with-note-added))]
    (are [x y] (= (x new-note) y)
         :freq 60.0
         :amp 0.5
         :node-id (o/to-sc-id test-node))))

(run-all-tests #"canverse.test.timeline-test")
