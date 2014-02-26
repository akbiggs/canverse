(ns canverse.helpers)

(defn clamp [n min-value max-value]
  (min max-value (max min-value n)))

(defn push-towards [n target amount]
  (if (< n target)
    (min target (+ n amount))
    (max target (- n amount))))

(defn relative [n min max]
  "Get a value from 0 to 1 indicating the position of
  n relative to the min and max."
  (/ (- n min) (- max min)))

(defn relative-in-scale [note scale]
  (relative note (first scale) (last scale)))
