(ns canverse.square
  (:require [canverse.synths :as synths]
            [canverse.helpers :as helpers]
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
(def scale (o/scale :c4 :major))

(defn create [row col]
  {:row row :col col :synth synths/oksaw :alpha 0})

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

(defn update-alpha [active-node square]
  (assoc square
    :alpha
    (if (and (not (nil? active-node)) (is-selected? square))
      (* 100 (o/node-get-control active-node :amp))
      (helpers/push-towards (:alpha square) 0 2))))

(defn play [square]
  (let [synth (:synth square)
        col (:col square)
        row (:row square)
        freq (nth scale col)
        node (synth :freq freq :amp 0.01)]
    ;(swap! (q/state :timeline) add-note)
    node))

(defn update [active-node square]
  (update-alpha active-node square))

(defn draw [square]
  (q/push-style)
  (q/stroke 125 25)
  (q/stroke-weight 2)
  (apply q/fill (conj (fill-color square) (:alpha square)))
  (let [x (get-x square)
        y (get-y square)]
    (q/rect x y SQUARE_SIZE SQUARE_SIZE))
  (q/pop-style))
