(ns canverse.helpers)

(defn clamp [n min-value max-value]
  (min max-value (max min-value n)))

(defn push-towards [n target amount]
  (if (< n target)
    (min target (+ n amount))
    (max target (- n amount))))

(push-towards 100 0 0.5)

(defn relative [n min max]
  "Get a value from 0 to 1 indicating the position of
  n relative to the min and max."
  (/ (- n min) (- max min)))

(defn relative-in-scale [note scale]
  (relative note (first scale) (last scale)))

(defn is-point-in-rect? [point rect-start rect-size]
  (let [start-x (:x rect-start)
        end-x (+ start-x (:x rect-size))
        start-y (:y rect-start)
        end-y (+ start-y (:x rect-size))
        pos-x (:x point)
        pos-y (:y point)]
    (and
     (>= pos-x start-x) (>= pos-y start-y)
     (<= pos-x end-x) (<= pos-y end-y))))
