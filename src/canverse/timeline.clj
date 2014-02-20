(ns canverse.timeline
  (:require [quil.core :as q]))

(defn create [history-length width]
  "Creates a new timeline retaining history-length
  milliseconds worth of information."
  {:history []
   :history-length history-length
   :width width})

(defn update [timeline]
  (assoc timeline
    :history
    (let [delta @(q/state :time-delta)
          delta-ratio (/ delta (:history-length timeline))
          width (:width timeline)
          movement (* delta-ratio width)
          history (:history timeline)]
      (for [note history]
        (assoc note :x (- (:x note) movement))))))

(defn add-note [relative-pitch timeline]
  (assoc timeline :history
    (let [new-x (:width timeline)
          new-y (- 400 (* 10 relative-pitch))
          new-note {:x new-x :y new-y :size [1 10]}
          history (:history timeline)]
      (concat history new-note))))

(defn draw [timeline]
  (q/fill 255)
  (doseq [note timeline]
    (let [[w h] (:size note)]
      (q/rect (:x note) (:y note) w h))))
