(ns canverse.core
  (:require [canverse.grid :as grid]
            [canverse.timeline :as timeline]
            [quil.core :as q]
            [overtone.live :as o]))

(def WINDOW_WIDTH 352)

(def my-kick (o/freesound 777))
(def my-hat (o/freesound 802))
(def my-clap (o/freesound 48310))

(defn drum-loop []
  (let [time (o/now)]
    (doseq [interval [0 1000 2000 3000 3600]]
      (o/apply-at (+ time interval) my-kick))
    (doseq [interval [500 1500 2500 3350 3500]]
      (o/apply-at (+ time interval) my-hat))
    (doseq [interval [1000 3000 3750]]
      (o/apply-at (+ time interval) my-clap))
    (o/apply-at (+ time 4000) drum-loop)))

(drum-loop)
(o/stop)

(defn setup []
  (q/smooth)
  (q/frame-rate 30)

  (q/set-state! :grid (atom (grid/create 7 7))
                :timeline (atom (timeline/create 60000 WINDOW_WIDTH))
                :time-delta (atom 0)
                :last-update-time (atom (o/now))
                :active (atom nil)

                :is-mouse-down? (atom false)
                :mouse-down-duration (atom 0)))

(defn on-mouse-pressed []
  (reset! (q/state :is-mouse-down?) true)
  (reset! (q/state :mouse-down-duration) 0))

(defn on-mouse-released []
  (reset! (q/state :is-mouse-down?) false))

(defn update []
  (let [current-time (o/now)
        last-update-time @(q/state :last-update-time)
        mouse-down-duration @(q/state :mouse-down-duration)
        elapsed-time (- current-time last-update-time)]
    (reset! (q/state :time-delta) elapsed-time)
    (reset! (q/state :last-update-time) current-time)
    (when @(q/state :is-mouse-down?)
      (reset! (q/state :mouse-down-duration) (+ mouse-down-duration elapsed-time))
      (println (str "Mouse has been held down for: " @(q/state :mouse-down-duration)))))

  (swap! (q/state :grid) grid/update)
  (swap! (q/state :timeline) timeline/update))

(defn draw []
  ; Quil has no update function that we can pass into
  ; the sketch, so we have to do it at the top of the
  ; draw call.
  (update)

  (q/background 125)
  (grid/draw @(q/state :grid))
  (timeline/draw @(q/state :timeline)))

(q/defsketch groovy
  :title "Groovy"
  :setup setup
  :draw draw
  :mouse-pressed on-mouse-pressed
  :mouse-released on-mouse-released
  :size [WINDOW_WIDTH 400])
