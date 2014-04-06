(ns canverse.changeinst
  (:require [canverse.input :as input]
            [canverse.synths :as synths]
            [canverse.helpers :as helpers])
  (:use quil.core))

(def index (atom 1))

(defn draw [user-input]
  (def last-key-pressed (:last-keycode-tapped user-input))
  (if (and (key-pressed?)
           (not (nil? last-key-pressed))
           (or (= last-key-pressed 40)
               (= last-key-pressed 38)))
    (cond (= last-key-pressed 40)
          (if (< @index 5)
              (do
                (reset! index (inc @index))
                (helpers/dbg @index))
              (do
                (reset! index 1)
                (helpers/dbg @index)))
          (= last-key-pressed 38)
          (if (> @index 1)
              (reset! index (dec @index))
              (reset! index 5))))
  (cond (= @index 1)
        (synths/update-instrument synths/oksaw)
        (= @index 2)
        (synths/update-instrument synths/dark-sea-horns)
        (= @index 3)
        (synths/update-instrument synths/plucked-string)
        (= @index 4)
        (synths/update-instrument synths/sampled-piano)
        (= @index 5)
        (synths/update-instrument synths/sampled-flute-vibrato)))

