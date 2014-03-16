(ns canverse.time)

(defn create [current-time]
  {:last-update-time current-time :elapsed-time 0})

(defn update [current-time time-object]
  {:last-update-time current-time
   :elapsed-time (- current-time (:last-update-time time-object))})
