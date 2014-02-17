(ns groovyclojure.grid
  (:require [groovyclojure.square :as square]))

(defn create-grid [w h]
  (for [row (range h)
        col (range w)]
    (square/create row col)))

(defn update-grid [grid]
  (for [square grid]
    (square/update square)))

(defn draw-grid [grid]
  (doseq [square grid] (square/draw square)))