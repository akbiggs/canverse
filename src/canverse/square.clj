(ns canverse.square
  (:require [canverse.synths :as synths]
            [canverse.helpers :as helpers]
            [canverse.point :as point]
            [canverse.input :as input]
            [canverse.inst.envelopeinput :as envelope-input]
            [canverse.drums :as drums]

            [overtone.core :as o]
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

(def scale (concat (o/scale :Cb4 :minor) (o/scale :Ab4 :minor)))

(defn create [row col]
  {:row row
   :col col
   :synth @synths/current-instrument
   :drums @drums/current-drum-loop
   :alpha 0})

(defn get-x [square]
  (* SQUARE_SIZE (:col square)))

(defn get-y [square]
  (* SQUARE_SIZE (:row square)))

(defn get-position [square]
  (point/create (get-x square) (get-y square)))

(defn fill-color [square]
  (get COLUMN_COLORS (:col square)))

(defn inside-bounds? [square point]
  (let [square-pos (get-position square)
        square-size (point/create SQUARE_SIZE SQUARE_SIZE)]
    (helpers/is-point-in-rect? point square-pos square-size)))

(defn is-selected? [square]
  (let [mouse-pos {:x (q/mouse-x) :y (q/mouse-y)}]
    (and
     (q/mouse-state)
     (inside-bounds? square mouse-pos))))

(defn update-alpha [actives square]
  (assoc square
    :alpha
    (if (and (seq? actives) (is-selected? square))
      (* 100 (o/node-get-control (:node (first actives)) :amp))
      (helpers/push-towards (:alpha square) 0 2))))

(defn play [envelope square]
  ;(helpers/dbg (:synth square))
  ; if left-mouse-clicked:
  ;    play current synth << envelope-input
  ; else:
  ;    play current drum track at current col/rows
  (if (input/right-mouse-click?)
    (let [drums (:drums square)
          row (:row square)
          col (:col square)
          drum-key (keyword (str row))]
      ;kill previous drum on this row before replaying new drum loop
      ((drum-key @drums/drums-hash))
      (drums/metro :bpm  col)))
    (let [synth (:synth square)
          col (:col square)
          row (:row square)
          freq (nth scale col)
          synth-args (conj {:freq freq :amp 0.01} (envelope-input/get-params envelope))
          node (helpers/apply-hash synth synth-args)]
      node))

(defn update [actives square]
  (update-alpha actives square))

(defn update-square-synth [square]
  (assoc square :synth @synths/current-instrument))

(defn draw [square]
  (q/push-style)
  (q/stroke 125 25)
  (q/stroke-weight 2)
  (apply q/fill (conj (fill-color square) (:alpha square)))
  (let [x (get-x square)
        y (get-y square)]
    (q/rect x y SQUARE_SIZE SQUARE_SIZE))
  (q/pop-style))
