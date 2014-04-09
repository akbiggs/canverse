(ns canverse.loop-toggles
  (:require [canverse.point :as point]
            [canverse.timeline :as timeline]
            [canverse.loop :as loop]
            [canverse.input :as input]
            [canverse.helpers :as helpers]

            [quil.core :as q]))

(def colors [
             [128 0 128]
             [0 0 255]
             [0 255 255]
             [0 255 0]
             [255 255 0]
             [255 165 0]
             [255 0 0]
             ])

(defn create [position size]
  {
   :loops nil
   :position position
   :size size

   :selected nil
   })

(defn update-loops [loops toggles]
  (assoc toggles :loops (take-last (count colors) loops)))

(defn get-loop-rect [index toggles]
  {:position (point/create (:x (:position toggles)) (* index 50))
   :size (point/create 50 50)})

(defn loop-just-selected? [user-input index toggles]
  (let [{:keys [position size]} (get-loop-rect index toggles)]
    (input/just-selected? position size user-input)))

(defn selected-loop-index [user-input toggles]
  (let [selected-loop-list (for [i (range (count (:loops toggles)))]
                             (loop-just-selected? user-input i toggles))]
    (helpers/find-thing true selected-loop-list)))

(defn mouse-ratio-in-loop [user-input index toggles]
  (let [{:keys [position size]} (get-loop-rect index toggles)
        start-y (:y position)
        end-y (+ start-y (:y size))]
    (- 1 (helpers/relative (:y (:mouse-pos user-input)) start-y end-y))))

(defn change-volume-on-click [user-input toggles]
  (let [loop-index (selected-loop-index user-input toggles)]
    (if-not (nil? loop-index)
      (assoc toggles :selected loop-index)
      toggles)))

(defn slide-volume-with-mouse [user-input toggles]
  (let [loop-index (:selected toggles)]
    (if-not (nil? loop-index)
      (let [mouse-ratio (mouse-ratio-in-loop user-input loop-index toggles)]
        (assoc toggles :loops
          (helpers/update-at loop-index #(assoc % :amp mouse-ratio) (:loops toggles))))
      toggles)))

(defn toggle-on-double-click [user-input toggles]
  (update-in toggles [:loops]
   #(for [i (range (count %))]
      (let [{:keys [position size]} (get-loop-rect i toggles)
            loop (nth % i)]
        (if (input/just-double-clicked? position size user-input)
          (loop/toggle loop)
          loop)))))

(defn stop-selecting-on-mouse-release [user-input toggles]
  (if (:mouse-just-released? user-input)
    (assoc toggles :selected nil)
    toggles))

(defn update [user-input loops toggles]
  (->> toggles
       (update-loops loops)
       (change-volume-on-click user-input)
       (slide-volume-with-mouse user-input)
       (stop-selecting-on-mouse-release user-input)
       (toggle-on-double-click user-input)))

(defn draw [toggles]
  (doseq [i (range (count (:loops toggles)))]
    (let [loop (nth (:loops toggles) i)
          toggle-start (point/create (:x (:position toggles)) (* i 50))
          toggle-size (point/create 50 50)

          loop-current-note (loop/get-current-note loop)
          alpha (if (nil? loop-current-note)
                  0
                  (* (:amp loop-current-note) 255))
          fill-color (if (:active? loop)
                       (nth colors i)
                       [125])
          fill (conj fill-color alpha)

          grayed-out-height (* (:y toggle-size) (- 1 (:amp loop)))
          colored-height (- (:y toggle-size) grayed-out-height)]
      (q/fill 125 alpha)
      (q/rect (:x toggle-start) (:y toggle-start) (:x toggle-size) grayed-out-height)

      (apply q/fill fill)
      (q/rect (:x toggle-start) (+ (:y toggle-start) grayed-out-height) (:x toggle-size) colored-height))))
