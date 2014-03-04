(ns canverse.timeline
  (:require [canverse.helpers :as helpers]
            [canverse.square :as square]
            [canverse.synths :as synths]
            [overtone.core :as o]
            [quil.core :as q]))

(defn create [history-length width]
  "Creates a new timeline retaining history-length
  milliseconds worth of information."
  {:history []
   :history-length history-length
   :width width})

(defn add-note-from-node [node timeline]
  (assoc timeline :history
    (let [{freq :freq amp :amp} (o/node-get-controls node [:freq :amp])
          new-x (:width timeline)
          relative-frequency (helpers/relative-in-scale freq square/scale)
          new-y (- 395 (* 45 relative-frequency))
          new-note {:x new-x :y new-y :size [1 2] :amp amp :relative-freq relative-frequency}
          history (:history timeline)]
      (conj history new-note))))

(defn add-notes-from-nodes [nodes timeline]
  (reduce #(add-note-from-node %2 %1) timeline
          (filter o/node-live? nodes)))

(defn update [elapsed-time timeline]
  (assoc timeline
    :history
    (let [movement-ratio (/ elapsed-time (:history-length timeline))
          width (:width timeline)
          movement (* movement-ratio width)
          history (:history timeline)]
      (vec (for [note history]
        (assoc note :x (- (:x note) movement)))))))

(defn draw [timeline]
  (q/push-style)

  (doseq [note (:history timeline)]
    (let [x (:x note)
          y (:y note)
          alpha (* 255 (:amp note))
          relative-freq (:relative-freq note)
          color (q/lerp-color (apply q/color (second square/COLUMN_COLORS))
                              (apply q/color (last square/COLUMN_COLORS))
                              relative-freq)
          [w h] (:size note)]

      (q/no-stroke)
      (q/fill color alpha)
      (q/rect x y w h)))

  (q/pop-style))
