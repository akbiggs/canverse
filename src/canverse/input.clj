(ns canverse.input
  (:require [quil.core :as q]))

(defn mouse-pos []
  {:x (q/mouse-x) :y (q/mouse-y)})