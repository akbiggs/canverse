(ns canverse.core
  (:require [canverse.grid :as grid]
            [canverse.timeline :as timeline]
            [canverse.input :as input]
            [canverse.square :as square]
            [canverse.point :as point]
            [canverse.nodes :as nodes]
            [canverse.time :as time]
            [canverse.synths :as synths]
            [canverse.helpers :as helpers]
            [canverse.instrumentwindow.keycapture :as key-capture]
            [canverse.instrumentwindow.envelopeinput :as envelope-input]

            [quil.core :as q]
            [overtone.core :as o])
  (:gen-class :main true))

(def WINDOW_WIDTH 352)

(def synth-definition (atom nil))
(def frame-counter (atom 0))

(defn swap-state! [state function]
  (swap! (q/state state) function))

(defn reset-state! [state value]
  (reset! (q/state state) value))

(defn update-state! [state & args]
  (let [namespace-symbol (symbol (str "canverse." (name state)))
        update-fn (intern `~namespace-symbol 'update)
        partial-args (concat [update-fn] args)]
    (swap-state! state (apply partial partial-args))
    @(q/state state)))

(defn setup []
  (q/smooth)
  (q/frame-rate 30)

  (q/set-state! :grid (atom (grid/create 7 7))
                :timeline (atom (timeline/create (point/create 0 350)
                                                 (point/create WINDOW_WIDTH 45)
                                                 30000))
                :nodes (atom (nodes/create))
                :time (atom (time/create (o/now)))
                :input (atom (input/create))))

(defn setup-instrument-window []
  (q/smooth)
  (q/no-stroke)
  (q/frame-rate 30)


  (q/set-state! :message (atom "Value")
                :envelope-input (atom (envelope-input/create 7))
                :time (atom (time/create (o/now)))
                :input (atom (input/create))))

(defn draw-instrument []
  ; Quil has no update function that we can pass into
  ; the sketch, so we have to do it at the top of the
  ; draw call.
  (update-state! :input 20)

  (q/background 0)
  (envelope-input/draw @(q/state :envelope-input) @(q/state :input))
  (reset! synth-definition (map #(:value %) (:params @(q/state :envelope-input)))))

(defn update! []
  (update-state! :time (o/now))
  (def elapsed-time (:elapsed-time @(q/state :time)))

  (update-state! :input elapsed-time)
  (def user-input @(q/state :input))

  (update-state! :nodes elapsed-time user-input @(q/state :grid) @(q/state :timeline))
  (def current-nodes @(q/state :nodes))

  (update-state! :grid (:active current-nodes))
  (update-state! :timeline user-input elapsed-time (nodes/get-all current-nodes))
  (if (= 30 @frame-counter)
    (do (reset! frame-counter 0) (synths/update @synth-definition))
    (swap! frame-counter inc)))

(defn draw []
  ; Quil has no update function that we can pass into
  ; the sketch, so we have to do it at the top of the
  ; draw call.
  (update!)

  (q/background 0)
  (grid/draw @(q/state :grid))
  (timeline/draw @(q/state :timeline)))

(defn -main [& args]
  (q/defsketch groovy
               :title "Canverse"
               :setup setup
               :draw draw
               :size [WINDOW_WIDTH 400])
  (q/defsketch instrgroovy
               :title "Canverse Instrument"
               :setup setup-instrument-window
               :draw draw-instrument
               :size [WINDOW_WIDTH 300]))

(-main)

(o/stop)
