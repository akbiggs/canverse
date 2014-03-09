(ns canverse.timeline
  (:require [canverse.helpers :as helpers]
            [canverse.square :as square]
            [canverse.synths :as synths]
            [canverse.point :as point]
            [overtone.core :as o]
            [quil.core :as q]))

(defn create [position size history-length]
  "Creates a new timeline retaining history-length
  milliseconds worth of information."
  {:position position
   :size size
   :history nil
   :history-length history-length})

(defn get-width [timeline]
  (get-in timeline [:size :x]))

(defn get-height [timeline]
  (get-in timeline [:size :y]))

(defn get-bottom [timeline]
  (let [{:keys [position size]} timeline]
    (+ (:y position) (:y size))))

(defn add-note-from-node [node timeline]
  (assoc timeline :history
    (let [{:keys [freq amp]} (o/node-get-controls node [:freq :amp])
          width (get-width timeline)
          new-x (+ (get-in timeline [:position :x]) width)

          ; place the line for the note so higher frequencies go higher,
          ; lower frequencies go lower
          relative-frequency (helpers/relative-in-scale freq square/scale)
          y-offset (* (get-height timeline) relative-frequency)
          new-y (- (get-bottom timeline) y-offset)

          new-note {:x new-x :y new-y :size [1 2] :amp amp :relative-freq relative-frequency}
          history (:history timeline)]
      (conj history new-note))))

(defn add-notes-from-nodes [nodes timeline]
  (reduce #(add-note-from-node %2 %1) timeline
          (filter o/node-live? nodes)))

(defn progress-history [elapsed-time timeline]
  (assoc timeline
    :history
    (let [movement-ratio (/ elapsed-time (:history-length timeline))
          movement (* movement-ratio (get-width timeline))
          history (:history timeline)]
      (for [note history]
        (assoc note :x (- (:x note) movement))))))

(defn update [elapsed-time nodes timeline]
  (->> timeline
       (add-notes-from-nodes nodes)
       (progress-history elapsed-time)))

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

(def test-timeline (create (point/create 0 350) (point/create 352 45) 30000))
