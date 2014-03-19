(ns canverse.instrumentwindow.envelopeinput
  (:require [canverse.instrumentwindow.keycapture :as key-capture]))

(defn create [inputs]
  {:synth-spec inputs
   :params (for [input (range inputs)]
            (key-capture/create input))
   })

(defn draw [envelope-input user-input]
  (doseq [input (:params envelope-input)]
    (key-capture/draw input user-input)))
