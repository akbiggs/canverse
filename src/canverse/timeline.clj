(ns canverse.timeline
  (:require [canverse.helpers :as helpers]
            [canverse.square :as square]
            [canverse.synths :as synths]
            [canverse.point :as point]
            [canverse.loop :as loop]

            [overtone.core :as o]
            [quil.core :as q]))

(defn create [position size history-length]
  "Creates a new timeline retaining history-length
  milliseconds worth of information."
  {:position position
   :size size
   :history nil
   :history-length history-length
   :loop-marquees nil
   :loop-selected? false})

(defn get-width [timeline]
  (get-in timeline [:size :x]))

(defn get-height [timeline]
  (get-in timeline [:size :y]))

(defn get-bottom [timeline]
  (let [{:keys [position size]} timeline]
    (+ (:y position) (:y size))))

(defn clear [timeline]
  (assoc timeline :history nil :loop-marquees nil :loop-selected? false))

(defn clear-after-loop-selected [timeline]
  (if (:loop-selected? timeline)
    (clear timeline)
    timeline))

(defn add-note-from-node [node timeline]
  (assoc timeline
    :history
    (let [{:keys [freq amp]} (o/node-get-controls node [:freq :amp])
          width (get-width timeline)
          new-x (+ (get-in timeline [:position :x]) width)

          ; place the line for the note so higher frequencies go higher,
          ; lower frequencies go lower
          relative-frequency (helpers/relative-in-scale freq square/scale)
          y-offset (* (get-height timeline) relative-frequency)
          new-y (- (get-bottom timeline) y-offset)

          new-note {:x new-x :y new-y :relative-time (:history-length timeline)
                    :size [1 (* 5 amp)] :amp amp :freq relative-frequency}
          history (:history timeline)]
      (conj history new-note))

    ; clear loop selection on note added
    :loop-marquees nil))

(defn add-notes-from-nodes [nodes timeline]
  (reduce #(add-note-from-node %2 %1) timeline
          (filter o/node-live? nodes)))

(defn should-be-frozen? [timeline]
  (seq? (:loop-marquees timeline)))

(defn progress-history [elapsed-time timeline]
  (if-not (should-be-frozen? timeline)
    (assoc timeline
      :history
      (let [movement-ratio (/ elapsed-time (:history-length timeline))
            movement (* movement-ratio (get-width timeline))
            history (:history timeline)]
        (for [note history]
          (let [new-x (- (:x note) movement)
                relative-time (get-relative-time-of new-x)]
            (assoc note :x new-x :relative-time relative-time)))))
    timeline))

(defn in-bounds? [position timeline]
  (helpers/is-point-in-rect? position (:position timeline) (:size timeline)))

(defn add-loop-marquee-at-position [position timeline]
  (update-in timeline [:loop-marquees]
             #(sort-by :x (conj % {:x (:x position)}))))

(defn add-loop-marquees-on-click [user-input timeline]
  (if (and (:mouse-tapped? user-input)
           (in-bounds? (:mouse-pos user-input) timeline))
    (add-loop-marquee-at-position (:mouse-pos user-input) timeline)
    timeline))

(defn is-loop-selected? [timeline]
  (>= (count (:loop-marquees timeline)) 2))

(defn get-relative-time-of [x-pos timeline]
  (let [x-ratio (/ x-pos (get-width timeline))]
    (Math/floor (* x-ratio (:history-length timeline)))))

(defn get-history-to-loop [timeline]
  (if (is-loop-selected? timeline)
    (let [marquees (:loop-marquees timeline)

          start-loop-time (get-relative-time-of (:x (first marquees)) timeline)
          end-loop-time (get-relative-time-of (:x (last marquees)) timeline)]
      {:start-time start-loop-time
       :notes (filter #(and (>= (:relative-time %) start-loop-time)
                            (<= (:relative-time %) end-loop-time))
                      (:history timeline))
       :end-time end-loop-time

       ; TODO: Don't hardcode instrument
       :instrument synths/oksaw})
    nil))

(defn update-loop-selected [timeline]
  (assoc timeline :loop-selected? (is-loop-selected? timeline)))

(defn update [user-input elapsed-time nodes timeline]
  (->> timeline
       (clear-after-loop-selected)
       (add-notes-from-nodes nodes)
       (progress-history elapsed-time)
       (add-loop-marquees-on-click user-input)
       (update-loop-selected)))

(defn draw [timeline]
  (q/push-style)

  (doseq [note (:history timeline)]
    (let [x (:x note)
          y (:y note)
          alpha (* 255 (:amp note))
          relative-freq (helpers/relative-in-scale (:freq note) square/scale)
          color (q/lerp-color (apply q/color (second square/COLUMN_COLORS))
                              (apply q/color (last square/COLUMN_COLORS))
                              relative-freq)
          [w h] (:size note)]

      (q/no-stroke)
      (q/fill color alpha)
      (q/rect x y w h)))

  (doseq [marquee (:loop-marquees timeline)]
    (let [x (:x marquee)
          top (get-in timeline [:position :y])
          bottom (get-bottom timeline)
          alpha 0.5]
      (q/stroke 255)
      (q/stroke-weight 5)
      (q/line x top x bottom)))

  (q/pop-style))

