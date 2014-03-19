(ns canverse.instrumentwindow.keycapture
  (:require [canverse.point :as point]
            [canverse.helpers :as helpers])
   (:use quil.core))

(def INPUT_TYPE [
                 "attack"
                 "decay"
                 "sustain"
                 "release"
                 "level"
                 "curve"
                 "bias"
                 ])

(def current-text-size (atom 12))

(defn create [input]
  {:input input
   :value 0})

(defn get-input [input]
  (atom (str "Enter " (get INPUT_TYPE input))))


(def params {:big-text-size 12
	:background-color 25
	:foreground-color 200})

;(defn setup []
 ; (smooth)
;  (no-stroke)
 ; (set-state! :message (atom "Click on screen and type a key")))

(defn change-value-of-envelope-input [index key-entered envelope-inputs]
  (assoc envelope-inputs :params
    (for [input (:params envelope-inputs)]
      (if (= (:input input) index) (assoc input :value (- key-entered 48)) input))))

;(for [x [1 2 3]]
;  (+ x 5))

;(assoc {:input 0 :value 0} :value 5)

;(def test-params [{:input 0 :value 0} {:input 1 :value 0} {:input 2 :value 0}])
;(def _raw-key 1)
;(def index 2)
;(for [input test-params]
;  (if (= (:input input) index) (assoc input :value raw-key) input))

(defn draw [input user-input]
  ;(background-float (params :background-color))
  (stroke-weight 20)
  (stroke-float 10)
  (fill (params :foreground-color))
  (text-size @current-text-size)
  (text (str (:value input)) 200 (+ 60 (* 20 (:input input))))
  (text @(get-input (:input input)) 20 (+ 60 (* 20 (:input input))))
  (def mousepointer (point/create (mouse-x) (mouse-y)))
  (text (str (:x mousepointer) (:y mousepointer)) 10 10)
  (def last-key-pressed (:last-key-pressed user-input))
  (if (and (> (:x mousepointer) 190)
           (< (:x mousepointer) 210)
           (> (:y mousepointer) 50)
           (< (:y mousepointer) 70)
           (not (nil? last-key-pressed))
           (> (int last-key-pressed) 47)
           (< (int last-key-pressed) 58))
    (swap! (state :envelope-input) #(change-value-of-envelope-input 0 (int last-key-pressed) %))
    (text "false" 200 200)))

(defn key-press []
	(let [big-text-size (params :big-text-size)]
	(if (< @current-text-size big-text-size) (reset! current-text-size big-text-size))
	(reset! (state :message) (raw-key))))





;(defsketch key-listener
 ; :title "Keyboard listener example"
  ;:size [400 100]
 ; :setup setup
  ;:draw draw
  ;:key-typed key-press)
