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
   :time-since-last-click 0

   :last-key-pressed nil
   :last-keycode-pressed nil
   :last-key-tapped nil
   :last-keycode-tapped nil
   })

(defn update [elapsed-time previous-input]
  (let [mouse-down? (q/mouse-state)
        previous-mouse-down? (:mouse-down? previous-input)
        old-down-duration (:mouse-down-duration previous-input)

        mouse-tapped? (and mouse-down? (not previous-mouse-down?))
        mouse-just-released? (and (not mouse-down?) previous-mouse-down?)
        time-since-last-click (if mouse-just-released?
                                0
                                (+ (:time-since-last-click previous-input)
                                   elapsed-time))]
    {:mouse-pos {:x (q/mouse-x) :y (q/mouse-y)}
     :mouse-down? mouse-down?
     :mouse-up? (not mouse-down?)

     :mouse-tapped?
     mouse-tapped?

     :mouse-double-clicked?
     (and mouse-tapped? (<= time-since-last-click 200))

     :mouse-just-released?
     mouse-just-released?

     :mouse-down-duration
     (if mouse-down?
       (+ old-down-duration elapsed-time)
       0)

     :time-since-last-click
     time-since-last-click

     :last-key-pressed
     (if (q/key-pressed?) (q/raw-key) nil)

     :last-keycode-pressed
     (if (q/key-pressed?) (q/key-code) nil)

     :last-key-tapped
     (if (and (q/key-pressed?) (nil? (:last-key-pressed previous-input)))
       (q/raw-key)
       nil)

     :last-keycode-tapped
     (if (and (q/key-pressed?) (nil? (:last-keycode-pressed previous-input)))
       (q/key-code)
       nil)}))

(defn just-evented-in-rect? [event hitbox-start hitbox-size input]
  (and event (helpers/is-point-in-rect? (:mouse-pos input) hitbox-start hitbox-size)))

(defn just-double-clicked? [hitbox-start hitbox-size input]
  (just-evented-in-rect? (:mouse-double-clicked? input) hitbox-start hitbox-size input))

(defn just-selected? [hitbox-start hitbox-size input]
  (just-evented-in-rect? (:mouse-tapped? input) hitbox-start hitbox-size input))

(defn left-mouse-click? []
  (and (q/mouse-state) (= (q/mouse-button) :left)))

(defn right-mouse-click? []
  (and (q/mouse-state) (= (q/mouse-button) :right)))
