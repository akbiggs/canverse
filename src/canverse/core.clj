(ns canverse.core
  (:require [canverse.grid :as grid]
            [canverse.timeline :as timeline]
            [canverse.input :as input]
            [canverse.square :as square]
            [canverse.point :as point]
            [canverse.nodes :as nodes]
            [canverse.helpers :as helpers]

            [quil.core :as q]
            [overtone.core :as o])
  (:gen-class :main true))

(def WINDOW_WIDTH 352)

(defmacro swap-state! [state function]
  `(swap! (q/state ~state) ~function))

(defmacro reset-state! [state value]
  `(reset! (q/state ~state) ~value))

(defn setup []
  (q/smooth)
  (q/frame-rate 30)

  (q/set-state! :grid (atom (grid/create 7 7))
                :timeline (atom (timeline/create 30000 WINDOW_WIDTH))
                :nodes (atom (nodes/create))

                ; keep track of the amount of time that passes between
                ; frames for properly handling animations/tweens
                :time-delta (atom 0)
                :last-update-time (atom (o/now))

                :input (atom (input/initialize))))

(defn update-time! [current-time elapsed-time]
  (reset-state! :time-delta elapsed-time)
  (reset-state! :last-update-time current-time))

(defn update-input! [elapsed-time]
  (swap-state! :input (partial input/update elapsed-time)))

(defn update-nodes! [elapsed-time user-input grid]
  (swap-state! :nodes (partial nodes/update elapsed-time user-input grid)))

(defn update-grid! [active-nodes]
  (swap-state! :grid (partial grid/update active-nodes)))

(defn update-timeline! [elapsed-time nodes]
  (swap-state! :timeline
               #(->> %
                     (timeline/add-notes-from-nodes nodes)
                     (timeline/update elapsed-time))))

(defn update! []
  (let [current-time (o/now)
        last-update-time @(q/state :last-update-time)
        elapsed-time (- current-time last-update-time)]

    (update-time! current-time elapsed-time)

    (update-input! elapsed-time)
    (def user-input @(q/state :input))

    (update-nodes! elapsed-time user-input @(q/state :grid))
    (def current-nodes @(q/state :nodes))

    (update-grid! (:active current-nodes))
    (update-timeline! elapsed-time (nodes/get-all current-nodes))))

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
               :size [WINDOW_WIDTH 400]))
(-main)
