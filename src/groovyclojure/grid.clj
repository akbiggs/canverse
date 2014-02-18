(ns groovyclojure.grid
  (:require [groovyclojure.square :as square]))

(defn create [w h]
  (for [row (range h)
        col (range w)]
    (square/create row col)))

(defn update [grid]
  (for [square grid]
    (square/update square)))

(defn draw [grid]
  (doseq [square grid] (square/draw square)))
