(ns canverse.grid
  (:require [canverse.square :as square]))

(defn create [w h]
  (for [row (range h)
        col (range w)]
    (square/create row col)))

(defn update [grid]
  (for [square grid]
    (square/update square)))

(defn size [grid]
  [(* 7 50) (* 7 50)])

(defn draw [grid]
  (doseq [square grid] (square/draw square)))
