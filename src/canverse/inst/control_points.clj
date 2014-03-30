(ns canverse.inst.control-points
  (:require [canverse.helpers :as helpers]
            [quil.core :as q]))

(def WINDOW_WIDTH 400)
(def QUARTER_WINDOW_WIDTH (/ WINDOW_WIDTH 4))
(def WINDOW_HEIGHT 400)

(def coordinate-ranges
  {:attack [0 QUARTER_WINDOW_WIDTH]
   :decay [QUARTER_WINDOW_WIDTH (* QUARTER_WINDOW_WIDTH 2)]
   :sustain [WINDOW_HEIGHT 0]
   :release [(* QUARTER_WINDOW_WIDTH 3) (* QUARTER_WINDOW_WIDTH 4)]})

(def opposite-axis-offsets
  {:attack 50
   :release 380
   ; decay and sustain node offsets are reliant on
   ; each other's values
   })

(defn create-point [param]
  {:param param

   ; ADR measure timespan, S measures amplitude,
   ; so axes along which they are controlled are different
   :control-axis
   (if (some #{param} [:attack :decay :release]) :x :y)

   :level 0.5})

(defn create []
  (map create-point [:attack :decay :sustain :release]))

(defn find-point [param control-points]
  (helpers/find-where #(= (:param %) param) control-points))

(defn get-control-and-opposite-axes [control-point]
  (if (nil? control-point)
    nil
    (let [control-axis (:control-axis control-point)]
      [control-axis (if (= control-axis :x) :y :x)])))

(defn get-value-on-control-axis [control-point]
  (let [control-value-range ((:param control-point) coordinate-ranges)
        [min max] [(first control-value-range) (last control-value-range)]]
    (q/lerp min max (:level control-point))))

(defn get-value-on-opposite-axis [all-points control-point]
  (let [param (:param control-point)]
    (cond (contains? opposite-axis-offsets param)
          (param opposite-axis-offsets)

          (= param :decay)
          (get-value-on-control-axis (find-point :sustain all-points))

          (= param :sustain)
          (helpers/midpoint
           (get-value-on-control-axis (find-point :decay all-points))
           (* QUARTER_WINDOW_WIDTH 3)))))

(defn get-position [all-points control-point]
  (let [[control-axis opposite-axis] (get-control-and-opposite-axes control-point)
        value-on-control-axis (get-value-on-control-axis control-point)
        value-on-opposite-axis (get-value-on-opposite-axis all-points control-point)]
    {control-axis value-on-control-axis
     opposite-axis value-on-opposite-axis}))

(defn get-positions [control-points]
  (map (partial get-position control-points) control-points))

(defn update [user-input elapsed-time control-points]
  (for [point control-points]
    point))

(defn draw [control-points]
  (let [point-positions (get-positions control-points)
        [attack-pos decay-pos sustain-pos release-pos] point-positions]
    (doseq [position (get-positions control-points)]
      (q/ellipse (:x position) (:y position) 25 25))

    (q/stroke-weight 5)
    (q/stroke 255)
    (q/line 0 (:release opposite-axis-offsets) (:x attack-pos) (:y attack-pos))
    (q/line (:x attack-pos) (:y attack-pos) (:x decay-pos) (:y decay-pos))
    (q/line (:x decay-pos) (:y decay-pos) (* QUARTER_WINDOW_WIDTH 3) (:y sustain-pos))
    (q/line (* QUARTER_WINDOW_WIDTH 3) (:y sustain-pos) (:x release-pos) (:y release-pos))))
