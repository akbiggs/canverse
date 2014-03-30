(ns canverse.inst.envelopeinput
  (:require [canverse.inst.control-points :as control-points]))

(defn create []
  {:control-points (control-points/create 0.5)})

(def instance (atom (create)))

(defn update [user-input elapsed-time envelope]
  (update-in envelope [:control-points]
             #(control-points/update user-input elapsed-time %)))

(defn get-params [envelope]
  (let [param-names (map :param (:control-points envelope))
        param-levels (map :level (:control-points envelope))]
    (zipmap param-names param-levels)))

(defn draw [envelope]
  (control-points/draw (:control-points envelope)))
