(ns canverse.grid
  (:require [canverse.square :as square]
            [canverse.point :as point]))

(defn create [w h]
  {:width w
   :height h
   :squares (for [row (range h)
                  col (range w)]
              (square/create row col))})

(defn update [active-nodes grid]
  (assoc grid
    :squares
    (map #(square/update active-nodes %) (:squares grid))))

(defn pixel-size [grid]
  (let [square-size (square/SQUARE_SIZE)
        width (* square-size (:width grid))
        height (* square-size (:height grid))]
    [width height]))

(defn square-size [grid]
  [(:width grid) (:height grid)])

(defn get-col-for [pos grid]
  (quot (:x pos) square/SQUARE_SIZE))

(defn get-row-for [pos grid]
  (quot (:y pos) square/SQUARE_SIZE))

(defn get-square-position-for [pos grid]
  {:x (get-col-for pos grid)
   :y (get-row-for pos grid)})

(defn get-index-for [pos grid]
  (let [{col :x row :y} (get-square-position-for pos grid)
        width-in-squares (:width grid)]
    (+ col (* width-in-squares row))))

(defn get-square-for [pos grid]
  (nth (:squares grid) (get-index-for pos grid) nil))

(defn in-bounds? [pos grid]
  (not (nil? (get-square-for (point/quotient pos (point/create square/SQUARE_SIZE square/SQUARE_SIZE))
                             grid))))

(defn play-at [pos grid]
  (if (in-bounds? pos grid)
    (square/play (get-square-for pos grid))
    nil))

(defn draw [grid]
  (doseq [square (:squares grid)]
    (square/draw square)))

(def test-grid (create 5 5))
