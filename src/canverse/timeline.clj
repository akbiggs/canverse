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
   :loop-notes nil
   :loop-selected? false})

(defn get-width [timeline]
  (get-in timeline [:size :x]))

(defn get-height [timeline]
  (get-in timeline [:size :y]))

(defn get-bottom [timeline]
  (let [{:keys [position size]} timeline]
    (+ (:y position) (:y size))))

(defn clear [timeline]
  (assoc timeline :history nil :loop-notes nil :loop-selected? false))

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
                    :size [1 (* 5 amp)] :amp amp :freq freq :node-id (o/to-sc-id node)}
          history (:history timeline)]
      (conj history new-note))

    ; clear loop selection on note added
    :loop-notes nil))

(defn add-notes-from-nodes [nodes timeline]
  (reduce #(add-note-from-node %2 %1) timeline
          (filter o/node-live? nodes)))

(defn should-be-frozen? [timeline]
  (seq? (:loop-notes timeline)))

(defn get-relative-time-of [x-pos timeline]
  (let [x-ratio (/ x-pos (get-width timeline))]
    (Math/floor (* x-ratio (:history-length timeline)))))

(defn get-movement [elapsed-time timeline]
  (* (/ elapsed-time (:history-length timeline)) (get-width timeline)))

(defn progress-history [elapsed-time timeline]
  (if-not (should-be-frozen? timeline)
    (assoc timeline
      :history
      (let [movement (get-movement elapsed-time timeline)
            history (:history timeline)]
        (for [note history]
          (let [new-x (- (:x note) movement)
                relative-time (- (:relative-time note) elapsed-time)]
            (assoc note :x new-x :relative-time relative-time)))))
    timeline))

(defn in-bounds? [position timeline]
  (helpers/is-point-in-rect? position (:position timeline) (:size timeline)))

(defn get-note-distance-from [x-pos note timeline]
  (Math/abs (- (int (:x note)) x-pos)))

(defn get-node-at [x-pos timeline]
  (let [is-note-close? #(<= (get-note-distance-from x-pos % timeline) 0.5)
        notes (:history timeline)
        closest-note (helpers/find-where is-note-close? notes)]
    closest-note))

(defn select-node [id timeline]
  (assoc timeline :loop-notes
    (let [old-loop-notes (:loop-notes timeline)
          all-node-notes (filter #(= (:node-id %) id) (:history timeline))]
      (sort-by :x (concat old-loop-notes all-node-notes)))))

(defn select-node-at-position [position timeline]
  (let [node-to-select (get-node-at (:x position) timeline)]
    (select-node (:node-id node-to-select) timeline)))

(defn select-node-on-click [user-input timeline]
  (if (and (:mouse-tapped? user-input)
           (in-bounds? (:mouse-pos user-input) timeline))
    (select-node-at-position (:mouse-pos user-input) timeline)
    timeline))

(defn is-loop-selected? [timeline]
  (not (nil? (:loop-notes timeline))))

(defn is-note-selected? [note timeline]
  (some #(= (:node-id note) (:node-id %)) (:loop-notes timeline)))

(defn get-history-to-loop [timeline]
  (if (is-loop-selected? timeline)
    (let [loop-notes (:loop-notes timeline)
          start-loop-time (:relative-time (first loop-notes))
          end-loop-time (:relative-time (last loop-notes))]
      {:start-time start-loop-time
       :notes (:loop-notes timeline)
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
       (select-node-on-click user-input)
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

      (if (is-note-selected? note timeline)
        (q/stroke 200)
        (q/no-stroke))

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
