(ns canverse.inst.control-points
  (:require [canverse.helpers :as helpers]
            [canverse.input :as input]
            [canverse.point :as point]
            [quil.core :as q]))

(def WINDOW_WIDTH 400)
(def QUARTER_WINDOW_WIDTH (/ WINDOW_WIDTH 4))
(def WINDOW_HEIGHT 400)

(def opposite-axis-offsets
  {:attack 50
   :release 380
   ; decay and sustain node offsets are reliant on
   ; each other's values
   })

(def coordinate-ranges
  {:attack [0 QUARTER_WINDOW_WIDTH]
   :decay [QUARTER_WINDOW_WIDTH (* QUARTER_WINDOW_WIDTH 2)]
   :sustain [(:release opposite-axis-offsets) (:attack opposite-axis-offsets)]
   :release [(* QUARTER_WINDOW_WIDTH 3) (* QUARTER_WINDOW_WIDTH 4)]})

(defn create-point [level param]
  {:param param

   ; ADR measure timespan, S measures amplitude,
   ; so axes along which they are controlled are different
   :control-axis
   (if (some #{param} [:attack :decay :release]) :x :y)

   :level level
   :size (point/create 25 25)

   :selected? false
   :selection-point nil})

(defn create [default-level]
  (map (partial create-point default-level)
       [:attack :decay :sustain :release]))

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

          ; place sustain between decay and start of release slope
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

(defn select [at-position control-point]
  (assoc control-point
    :selected? true
    :selection-point at-position))

(defn select-on-click [user-input all-points control-point]
  (let [position (get-position all-points control-point)
        ; circle is centered, hitbox starts at top-left corner,
        ; so need to offset by half of circle's size
        position-with-offset
        (point/minus position (point/scalar-divide (:size control-point) 2))]

    (if (input/just-selected? position-with-offset (:size control-point) user-input)
      (select (:mouse-pos user-input) control-point)
      control-point)))

(defn drag-if-selected [user-input control-point]
  (if-let [selection-point (:selection-point control-point)]
    (let [[level-min level-max] ((:param control-point) coordinate-ranges)
          control-axis (:control-axis control-point)
          mouse-value-on-axis (control-axis (:mouse-pos user-input))
          mouse-ratio-in-range (helpers/relative mouse-value-on-axis level-min level-max)
          new-level (helpers/clamp mouse-ratio-in-range 0 1)]
      (assoc control-point :level new-level))
    control-point))

(defn deselect-on-mouse-release [user-input control-point]
  (if (and (:selected? control-point) (:mouse-up? user-input))
    (assoc control-point :selected? false :selection-point nil)
    control-point))

(defn update [user-input elapsed-time control-points]
  (for [control-point control-points]
    (->> control-point
         (select-on-click user-input control-points)
         (drag-if-selected user-input)
         (deselect-on-mouse-release user-input))))

(defn draw [control-points]
  (let [[attack-pos decay-pos sustain-pos release-pos] (get-positions control-points)]
    (doseq [control-point control-points]
      (let [position (get-position control-points control-point)
            size (:size control-point)]
        (if (:selected? control-point)
          (q/fill 125)
          (q/fill 255))
        (q/ellipse (:x position) (:y position) (:x size) (:y size))))

    (q/stroke-weight 5)
    (q/stroke 255)

    (q/line 0 (:release opposite-axis-offsets) (:x attack-pos) (:y attack-pos))
    (q/line (:x attack-pos) (:y attack-pos) (:x decay-pos) (:y decay-pos))
    (q/line (:x decay-pos) (:y decay-pos) (* QUARTER_WINDOW_WIDTH 3) (:y sustain-pos))
    (q/line (* QUARTER_WINDOW_WIDTH 3) (:y sustain-pos) (:x release-pos) (:y release-pos))))
