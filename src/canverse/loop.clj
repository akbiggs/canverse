(ns canverse.loop
  (:require [canverse.synths :as synths]
            [overtone.core :as o]))

(defn initialize-node [instrument]
  (instrument :amp 0))

(defn create-from-history [history]
  (let [{:keys [notes start-time end-time instrument]} history]
    {:notes (map #(assoc % :relative-time (- (:relative-time %) start-time)) notes)
     :end-time (- end-time start-time)
     :node (initialize-node instrument)
     :current-time 0

     ; TODO: Don't hardcode instrument.
     :instrument instrument}))

(defn restart [time-past-end loop]
  (when-not (nil? (:node loop))
    (o/kill (:node loop)))

  (assoc loop
    :node (initialize-node (:instrument loop))
    :current-time time-past-end))

(defn update-current-time [elapsed-time loop]
  (let [{:keys [current-time end-time]} loop
        new-time (+ current-time elapsed-time)]
    (if (> new-time end-time)
      (restart (- new-time end-time) loop)
      (assoc loop :current-time new-time))))

(defn get-notes-before-current-time [loop]
  (filter #(<= (:relative-time %) (:current-time loop))
          (:notes loop)))

(defn get-current-note [loop]
  (let [notes-before-current-time (filter )]))
(defn play-current-note [loop]
  (let [current-note (get-current-note loop)]
    (if )))

(defn update [elapsed-time loop]
  (->> loop
       (update-current-time elapsed-time)
       (play-current-note)))

(def test-loop (create-from-history {:start-time 500 :notes [{:relative-time 800} {:relative-time 1000} {:relative-time 1200}]
                                      :end-time 1500
                                     :instrument synths/oksaw}))
(def after-time-update (update-current-time 400 test-loop))
after-time-update
(get-notes-before-current-time after-time-update)
