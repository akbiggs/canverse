(ns canverse.input
  (:require [quil.core :as q]
            [canverse.helpers :as helpers]))

(defn create []
  {:mouse-pos {:x 0 :y 0}
   :mouse-down? false
   :mouse-up? true
   :mouse-down-duration 0
   :mouse-tapped? false
   :mouse-just-released? false
   :last-key-pressed nil
   :last-key-tapped nil})

(defn update [elapsed-time previous-input]
  (let [mouse-down? (q/mouse-state)
        previous-mouse-down? (:mouse-down? previous-input)
        old-down-duration (:mouse-down-duration previous-input)]
    {:mouse-pos {:x (q/mouse-x) :y (q/mouse-y)}
     :mouse-down? mouse-down?
     :mouse-up? (not mouse-down?)

     :mouse-tapped?
     (and mouse-down? (not previous-mouse-down?))

     :mouse-just-released?
     (and (not mouse-down?) previous-mouse-down?)

     :mouse-down-duration
     (if mouse-down?
       (+ old-down-duration elapsed-time)
       0)

     :last-key-pressed
     (if (q/key-pressed?) (q/raw-key) (:last-key-pressed previous-input))

     :last-key-tapped
     (if (q/key-pressed?) (q/raw-key) nil)}))

(defn just-selected? [hitbox-start hitbox-size input]
  (and (:mouse-tapped? input)
       (helpers/is-point-in-rect? (:mouse-pos input) hitbox-start hitbox-size)))
