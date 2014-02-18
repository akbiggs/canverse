(ns groovyclojure.square
  (:require [groovyclojure.synths :as synths]
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

(defn create [row col]
  {:row row :col col :synth synths/oksaw :synthnode nil})

(defn get-x [square]
  (* SQUARE_SIZE (:col square)))

(defn get-y [square]
  (* SQUARE_SIZE (:row square)))

(defn fill-color [square]
  (get COLUMN_COLORS (:col square)))

(defn is-selected? [square]
  (let [mouse-pos {:x (q/mouse-x) :y (q/mouse-y)}]
    (and
     (q/mouse-state)
     (inside-bounds? square mouse-pos))))

(defn inside-bounds? [square position]
  (let [x (get-x square)
        end-x (+ x SQUARE_SIZE)
        y (get-y square)
        end-y (+ y SQUARE_SIZE)
        pos-x (:x position)
        pos-y (:y position)]
    (and
     (>= pos-x x) (>= pos-y y)
     (<= pos-x end-x) (<= pos-y end-y))))

(defn update [square]
  (if (is-selected? square)
    (select square)
    square))

(defn play [square]
  (if (is-playing? square)
    square
    (let [synth (:synth square)
          freq (+ 50 (* 2 (:col square)))
          node (synth freq)]
      (assoc square :synthnode node))))

(defn select [square]
  (assoc (play square) :selected true))

(defn is-playing? [square]
  (o/node-live? (:synthnode square)))

(defn draw [square]
  (q/stroke 255)
  (q/stroke-weight 2)
  (if (is-selected? square)
    (apply q/fill (fill-color square))
    (q/no-fill))
  (let [x (get-x square)
        y (get-y square)]
    (q/rect x y SQUARE_SIZE SQUARE_SIZE)))
