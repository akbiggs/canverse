(ns groovyclojure.core
  (:require [groovyclojure.grid :as grid]
            [quil.core :as q]
            [overtone.live :as o]))

(def my-grid (atom (grid/create 7 7)))

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

(defn setup []
  (q/smooth)
  (q/frame-rate 30))

(defn draw []
  (q/background 125)
  (swap! my-grid grid/update)
  (grid/draw @my-grid))

(q/defsketch test
  :title "Groovy"
  :setup setup
  :draw draw
  :size [(/ (q/screen-width) 2) (q/screen-height)])
