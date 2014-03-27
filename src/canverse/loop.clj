(ns canverse.loop
  (:require [canverse.synths :as synths]
            [canverse.helpers :as helpers]
            [overtone.core :as o]))

(defn initialize-node [instrument]
  (instrument :amp 0))

(defn create [history]
  (let [{:keys [notes start-time end-time instrument time-before-start]} history]
    (prn "Relative note times "(map :relative-time notes))
    (prn "Start time" start-time)
    (prn "time before start " time-before-start)
    {
     ; since we no longer care about the time when the loop started, just
     ; the relative offsets of the notes from the start, subtract
     ; the start time from each note's time
     :notes (map #(assoc % :relative-time (- (+ (:relative-time %) time-before-start) start-time)) notes)
     :end-time (- end-time start-time)
     :last-start-time end-time
     :node (initialize-node instrument)
     :current-time 0
     :time-before-start time-before-start

     :instrument instrument}))

(defn get-length [loop]
  (:end-time loop))

(defn next-end-time [loop]
  (+ (:last-start-time loop) (get-length loop)))

(defn started? [loop]
  (<= (:time-before-start loop) 0))

(defn restart [time-past-end loop]
  (when-not (nil? (:node loop))
    (o/kill (:node loop)))

  (assoc loop
    :node (initialize-node (:instrument loop))
    :current-time time-past-end
    :last-start-time (+ (:last-start-time loop) (get-length loop))))

(defn progress [elapsed-time loop]
  (let [{:keys [current-time end-time time-before-start last-start-time]} loop
        new-time (+ current-time elapsed-time)]
    (cond (not (started? loop))
          (assoc loop :time-before-start (- time-before-start elapsed-time))

          (> new-time end-time)
          (restart (- new-time end-time) loop)

          :else
          (assoc loop
            :current-time new-time
            :last-start-time (- last-start-time elapsed-time)))))

(defn get-notes-before-current-time [loop]
  (filter #(<= (:relative-time %) (:current-time loop))
          (:notes loop)))

(defn get-current-note [loop]
  (last (get-notes-before-current-time loop)))

(defn play-current-note! [loop]
  (let [current-note (get-current-note loop)
        time-before-start (:time-before-start loop)]
    (when-not (or (> time-before-start 0) (nil? current-note))
      (o/ctl (:node loop) :amp (:amp current-note) :freq (:freq current-note))))
  loop)

(defn update! [elapsed-time loop]
  (->> loop
       (progress elapsed-time)
       (play-current-note!)))

; TESTING
(def test-loop (create {:start-time 500 :notes [{:relative-time 800 :amp 0.5 :freq 60} {:relative-time 1000} {:relative-time 1200}]
                        :end-time 1500
                        :time-before-start 20
                        :instrument synths/oksaw}))
(def after-time-update (progress 400 test-loop))
(:time-before-start after-time-update)
(get-notes-before-current-time after-time-update)
(get-current-note after-time-update)
