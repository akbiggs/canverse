(ns canverse.inst.envelopeinput
  (:require [canverse.inst.keycapture :as key-capture]
            [canverse.inst.control-points :as control-points]))

(defn create []
  {:control-points (control-points/create)})

(defn update [user-input elapsed-time envelope]
  (update-in envelope [:control-points]
             #(control-points/update user-input elapsed-time %)))

(defn draw [envelope]
  (control-points/draw (:control-points envelope)))
