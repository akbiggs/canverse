(ns groovyclojure.core
  (:require [groovyclojure.grid :as grid]))

(def my-grid (atom (grid/create-grid 7 7)))

(defn setup []
  (q/smooth)
  (q/frame-rate 30))

(defn draw []
  (q/background 125)
  (swap! my-grid update-grid)
  (draw-grid @my-grid))

(q/defsketch test
  :title "Groovy"
  :setup setup
  :draw draw
  :size [(/ (q/screen-width) 2) (q/screen-height)])
