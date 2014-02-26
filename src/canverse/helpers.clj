(ns canverse.helpers)

(defn clamp [n min-value max-value]
  (min max-value (max min-value n)))

(defn push-towards [n target amount]
  (if (< n target)
    (min target (+ n amount))
    (max target (- n amount))))
