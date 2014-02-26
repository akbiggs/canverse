(ns canverse.square
  (:require [canverse.synths :as synths]
            [canverse.timeline :as timeline]
            [overtone.live :as o]
            [quil.core :as q]))

(def SQUARE_SIZE 50)
(def COLUMN_COLORS [
                    [128 0 128]
                    [0 0 255]
                    [0 255 255]
                    [0 255 0]
                    [255 255 0]
                    [255 165 0]
                    [255 0 0]
                    ])
(def pitches (o/scale :c4 :major))

(defn create [row col]
  {:row row :col col :synth synths/oksaw})

(defn get-x [square]
  (* SQUARE_SIZE (:col square)))

(defn get-y [square]
  (* SQUARE_SIZE (:row square)))

(defn fill-color [square]
  (get COLUMN_COLORS (:col square)))

(defn inside-bounds? [square position]
  (let [start-x (get-x square)
        end-x (+ start-x SQUARE_SIZE)
        start-y (get-y square)
        end-y (+ start-y SQUARE_SIZE)
        pos-x (:x position)
        pos-y (:y position)]
    (and
     (>= pos-x start-x) (>= pos-y start-y)
     (<= pos-x end-x) (<= pos-y end-y))))

(defn is-selected? [square]
  (let [mouse-pos {:x (q/mouse-x) :y (q/mouse-y)}]
    (and
     (q/mouse-state)
     (inside-bounds? square mouse-pos))))

(defn play [square]
  (let [synth (:synth square)
        col (:col square)
        row (:row square)
        freq (nth pitches col)
        climb (+ 20 (* 200 row))
        node (synth :freq freq :amp 0.01 :climb climb)
        add-note (partial timeline/add-note col)]
    ;(swap! (q/state :timeline) add-note)
    node))

(defn update [square]
  square)

(defn draw [square]
  (q/stroke 255)
  (q/stroke-weight 2)
  (if (is-selected? square)
    (apply q/fill (fill-color square))
    (q/no-fill))
  (let [x (get-x square)
        y (get-y square)]
    (q/rect x y SQUARE_SIZE SQUARE_SIZE)))
