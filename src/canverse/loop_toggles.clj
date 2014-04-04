(ns canverse.loop-toggles
  (:require [canverse.point :as point]
            [canverse.timeline :as timeline]
            [canverse.loop :as loop]
            [canverse.input :as input]

            [quil.core :as q]))

(defn create [position size]
  {
   :loops nil
   :position position
   :size size
   })

(defn update-loops [loops toggles]
  (assoc toggles :loops loops))

(defn toggle-on-click [user-input toggles]
  (update-in toggles [:loops]
   #(for [i (range (count %))]
      (let [toggle-start (point/create (:x (:position toggles)) (* i 50))
            toggle-size (point/create 50 50)
            loop (nth % i)]
        (if (input/just-selected? toggle-start toggle-size user-input)
          (loop/toggle loop)
          loop)))))

(defn update [user-input loops toggles]
  (->> toggles
       (update-loops loops)
       (toggle-on-click user-input)))

(defn draw [toggles]
  (doseq [i (range (count (:loops toggles)))]
    (let [loop (nth (:loops toggles) i)
          toggle-start (point/create (:x (:position toggles)) (* i 50))
          toggle-size (point/create 50 50)

          loop-current-note (loop/get-current-note loop)
          alpha (if (nil? loop-current-note)
                  0
                  (* (:amp loop-current-note) 255))]
      (q/fill 125 alpha)
      (q/rect (:x toggle-start) (:y toggle-start) (:x toggle-size) (:y toggle-size)))))