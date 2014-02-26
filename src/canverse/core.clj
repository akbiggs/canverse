(ns canverse.core
  (:require [canverse.grid :as grid]
            [canverse.timeline :as timeline]
            [canverse.input :as input]
            [canverse.square :as square]
            [canverse.point :as point]
            [canverse.helpers :as helpers]
            [quil.core :as q]
            [overtone.live :as o]))

(def WINDOW_WIDTH 352)

(def my-kick (o/freesound 777))
(def my-hat (o/freesound 802))
(def my-clap (o/freesound 48310))

(defn drum-loop []
  (let [time (o/now)]
    (doseq [interval [0 1000 2000 3000 3600]]
      (o/apply-at (+ time interval) my-kick))
    (doseq [interval [500 1500 2500 3350 3500]]
      (o/apply-at (+ time interval) my-hat))
    (doseq [interval [1000 3000 3750]]
      (o/apply-at (+ time interval) my-clap))
    (o/apply-at (+ time 4000) drum-loop)))

(drum-loop)
(o/stop)

(defn setup []
  (q/smooth)
  (q/frame-rate 30)

  (q/set-state! :grid (atom (grid/create 7 7))
                :timeline (atom (timeline/create 60000 WINDOW_WIDTH))

                ; keep track of the amount of time that passes between
                ; frames for properly handling animations/tweens
                :time-delta (atom 0)
                :last-update-time (atom (o/now))

                ; active node is the node currently being controlled by
                ; the user's mouse
                :active (atom nil)

                ; releasing nodes are all nodes that are no longer controlled
                ; by the user (e.g. nodes that are fading out)
                :releasing (atom nil)

                :input (atom (input/initialize))))

(defn update-time! [current-time elapsed-time]
  (reset! (q/state :time-delta) elapsed-time)
  (reset! (q/state :last-update-time) current-time))

(defn update-input! [elapsed-time]
  (swap! (q/state :input) (partial input/update elapsed-time))
  @(q/state :input))

(defn activate-node-at! [position grid]
  (let [square (grid/get-square-for position grid)
        node (square/play square)
        initial-freq (o/node-get-control node :freq)
        max-amp (- 1 (* 0.1 (:row square)))
        base-props {:position position :freq initial-freq :max-amp max-amp}]
    (reset! (q/state :active) {:node node :base base-props})))

(defn get-active-node []
  (:node @(q/state :active)))

(defn update-active-node! [elapsed-time user-input]
  (let [mouse-down-duration (:mouse-down-duration user-input)
        mouse-pos (:mouse-pos user-input)
        active @(q/state :active)
        node (:node active)

        ; deviation is the amount of distance between the point at which
        ; the node started and the current position of the mouse
        deviation (point/minus mouse-pos (get-in active [:base :position]))

        ; to keep the transition smooth but the note still sounding nice,
        ; as the user's mouse moves push the current frequency towards the nearest
        ; note of the scale we're working on
        target-freq (nth square/scale (/ (:x mouse-pos) square/SQUARE_SIZE))
        freq-climb 0.2
        current-freq (o/node-get-control node :freq)
        next-freq (helpers/push-towards current-freq target-freq freq-climb)

        amp-climb 0.05
        next-amp (+ (o/node-get-control node :amp) amp-climb)

        ; max amplitude a note can reach is determined by the y position
        ; of the mouse, need to divide by a sufficiently large number to get
        ; small increments as mouse moves around
        intended-max-amp (- (get-in active [:base :max-amp]) (/ (:y deviation) 325))
        max-amp (helpers/clamp intended-max-amp 0 1)]
    (o/ctl node :amp (min max-amp next-amp) :freq next-freq)))

(defn release-active-node! []
  (swap! (q/state :releasing) #(conj % (get-active-node)))
  (reset! (q/state :active) nil))

(defn update-releasing-nodes! [elapsed-time]
  (swap! (q/state :releasing) #(filter o/node-live? %))

  (doseq [node @(q/state :releasing)]
    (let [current-amp (o/node-get-control node :amp)
          amp-decrease (/ elapsed-time 1000)
          new-amp (max 0 (- current-amp amp-decrease))]
      (if (<= new-amp 0)
        (o/kill node)
        (o/ctl node :amp new-amp)))))

(defn update! []
  (let [current-time (o/now)
        last-update-time @(q/state :last-update-time)
        elapsed-time (- current-time last-update-time)
        user-input (update-input! elapsed-time)]

    (update-time! current-time elapsed-time)

    (cond (:mouse-tapped? user-input)
          (activate-node-at! (:mouse-pos user-input) @(q/state :grid))

          (:mouse-just-released? user-input)
          (release-active-node!))

    (when-not (nil? @(q/state :active))
      (update-active-node! elapsed-time user-input))

    (update-releasing-nodes! elapsed-time)))

(defn draw []
  ; Quil has no update function that we can pass into
  ; the sketch, so we have to do it at the top of the
  ; draw call.
  (update!)

  (q/background 125)
  (grid/draw @(q/state :grid))
  (timeline/draw @(q/state :timeline)))

(q/defsketch groovy
  :title "Groovy"
  :setup setup
  :draw draw
  :size [WINDOW_WIDTH 400])
