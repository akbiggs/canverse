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
   :history-length history-length
   :loop-marquees nil})

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

(defn in-bounds? [position timeline]
  (helpers/is-point-in-rect? position (:position timeline) (:size timeline)))

(defn add-loop-marquee-at-position [position timeline]
  (update-in timeline [:loop-marquees]
             #(conj % {:x (:x position)})))

(defn add-loop-marquees-on-click [user-input timeline]
  (if (and (:mouse-tapped? user-input)
           (in-bounds? (:mouse-pos user-input) timeline))
    (add-loop-marquee-at-position (:mouse-pos user-input) timeline)
    timeline))

(defn update [user-input elapsed-time nodes timeline]
  (->> timeline
       (add-notes-from-nodes nodes)
       (progress-history elapsed-time)
       (add-loop-marquees-on-click user-input)))

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

  (doseq [marquee (:loop-marquees timeline)]
    (prn "Marquee: " marquee)
    (let [x (:x marquee)
          top (get-in timeline [:position :y])
          bottom (get-bottom timeline)
          alpha 0.5]
      (q/stroke 255)
      (q/stroke-weight 5)
      (q/line x top x bottom)))

  (q/pop-style))

