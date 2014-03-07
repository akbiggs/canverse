(ns canverse.input
  (:require [quil.core :as q]))

(defn create []
  {:mouse-pos {:x 0 :y 0}
   :mouse-down? false
   :mouse-down-duration 0
   :mouse-tapped? false
   :mouse-just-released? false})

(defn update [elapsed-time previous-input]
  (let [mouse-down? (q/mouse-state)
        previous-mouse-down? (:mouse-down? previous-input)
        old-down-duration (:mouse-down-duration previous-input)]
    {:mouse-pos {:x (q/mouse-x) :y (q/mouse-y)}
     :mouse-down? mouse-down?

     :mouse-tapped?
     (and mouse-down? (not previous-mouse-down?))

     :mouse-just-released?
     (and (not mouse-down?) previous-mouse-down?)

     :mouse-down-duration
     (if mouse-down?
       (+ old-down-duration elapsed-time)
       0)}))
