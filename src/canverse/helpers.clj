(ns canverse.helpers)

(defn lerp [a b t]
  (+ a (* t (- b a))))

(defn clamp [n min-value max-value]
  (min max-value (max min-value n)))

(defn clamped-lerp [a b t]
  (clamp (lerp a b t) a b))

(defn push-towards [n target amount]
  (if (< n target)
    (min target (+ n amount))
    (max target (- n amount))))

(defn midpoint [min max]
  (+ min (/ (- max min) 2)))

(defn relative [n min max]
  "Get a value from 0 to 1 indicating the position of
  n relative to the min and max."
  (clamp (/ (- n min) (- max min)) 0 1))

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

(defn update-at [index function coll]
  (for [i (range (count coll))]
    (if (= i index)
      (function (nth coll i))
      (nth coll i))))

(defn replace-at [index value coll]
  (update-at index (fn [_] value) coll))

(defn find-where [pred coll]
  (first (filter pred coll)))

(defn in-range? [n min max]
  (and (<= min n) (>= max n)))

(defn apply-hash [fn hash]
  (apply fn (interleave (keys hash) (vals hash))))

(defn indices-of [f coll]
  (keep-indexed #(if (f %2) %1 nil) coll))

(defn first-index-of [f coll]
  (first (indices-of f coll)))

(defn find-thing [value coll]
  (first-index-of #(= % value) coll))
;;debugging parts of expressions
(defmacro dbg [x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defn update-print [x]
  (if (= (rand-int 3) 0) (dbg x)))
