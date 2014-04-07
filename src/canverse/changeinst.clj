(ns canverse.changeinst
  (:require [canverse.input :as input]
            [canverse.synths :as synths]
            [canverse.helpers :as helpers])
  (:use quil.core))

(def index (atom 1))

(def insts
  [synths/awesome
   synths/resonant
   synths/deus
   synths/vector
   synths/chase
   synths/fight
   synths/fight-downtime
   synths/climax
   synths/chase
   synths/weird])

(defn draw [user-input]
  (def last-key-pressed (:last-keycode-tapped user-input))
  (if (and (key-pressed?)
           (not (nil? last-key-pressed))
           (or (= last-key-pressed 40)
               (= last-key-pressed 38)))
    (cond (= last-key-pressed 40)
          (if (< @index (count insts))
              (do
                (reset! index (inc @index))
                (helpers/dbg @index))
              (do
                (reset! index 1)
                (helpers/dbg @index)))
          (= last-key-pressed 38)
          (if (> @index 1)
              (reset! index (dec @index))
              (reset! index (count insts)))))

  (doseq [i (range 1 (+ (count insts) 1))]
    (when (= @index i)
      (synths/update-instrument (nth insts (dec i))))))

