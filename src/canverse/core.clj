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
                :last-update-time (atom (o/now))))

(defn update []
  (let [current-time (o/now)
        last-update-time @(q/state :last-update-time)]
    (reset! (q/state :time-delta) (- current-time last-update-time))
    (reset! (q/state :last-update-time) current-time))

  (swap! (q/state :grid) grid/update)
  (swap! (q/state :timeline) timeline/update))

(defn draw []
  ; Quil has no update function that we can pass into
  ; the sketch, so we have to do it at the top of the
  ; draw call.
  (update)

  (q/background 125)
  (grid/draw @(q/state :grid)))

(q/defsketch test
  :title "Groovy"
  :setup setup
  :draw draw
  :size [WINDOW_WIDTH 400])
