(ns canverse.changeinst
  (:require [canverse.input :as input]
            [canverse.synths :as synths]
            [canverse.helpers :as helpers]
            [canverse.drums :as drums])
  (:use quil.core))

(def index (atom 1))
(def text-alpha (atom 0))
(def fading-in? (atom false))

(def inst-names
  [
   "80s Saw"
   "Flute"
   "Piano"
   "Dark Sea Horns"
   "Awe"
   "Resonant"
   "Deus"
   "Vector"
   "Endgame"
   "Fight"
   "Downtime"
   "Finish Them!"
   "Chase"
   "Descent"
   "Armistice"
   "Moon"
   "Sagan"
   "Woods"
   "Street"
   ])

(def insts
  [synths/oksaw
   synths/sampled-flute-vibrato
   synths/sampled-piano
   synths/dark-sea-horns
   synths/awesome
   synths/resonant
   synths/deus
   synths/vectorman
   synths/poseidon
   synths/fight
   synths/fight-downtime
   synths/climax
   synths/chase
   synths/sad-fi
   synths/tristesse
   synths/moon
   synths/sagan
   synths/woods
   synths/streets])

(defn change-index! [new-index]
  (reset! index new-index)
  (reset! fading-in? true))

(defn draw [elapsed-time user-input]
  (def last-key-pressed (:last-keycode-tapped user-input))

  (if (and (key-pressed?)
           (not (nil? last-key-pressed))
           (or (= last-key-pressed 40)
               (= last-key-pressed 38)
               (= last-key-pressed 37)
               (= last-key-pressed 39)))
    (cond (= last-key-pressed 40)
          (if (< @index (count insts))
            (change-index! (inc @index))
            (change-index! 1))
          (= last-key-pressed 38)
          (if (> @index 1)
              (change-index! (dec @index))
              (change-index! (count insts)))
          (= last-key-pressed 39)
          (do
            (comment helpers/dbg (drums/metro :bpm))
            (drums/metro :bpm
                       (helpers/clamp
                        (mod (+ 10 (drums/metro :bpm)) 80) 10 70)))
          (= last-key-pressed 37)
          (do
            (comment helpers/dbg (drums/metro :bpm))
            (drums/metro :bpm
                       (helpers/clamp
                        (mod (- (drums/metro :bpm) 10) 80) 10 70)))))

  (if @fading-in?
    (reset! text-alpha (+ @text-alpha (/ elapsed-time 3)))
    (reset! text-alpha (- @text-alpha (/ elapsed-time 3))))

  (reset! text-alpha (helpers/clamp @text-alpha 0 255))

  (when (= @text-alpha 255)
    (reset! fading-in? false))

  (text-size 36)
  (let [inst-name (nth inst-names (- @index 1))
        width (text-width inst-name)
        half-screen-height 180
        text-start (- 200 (/ width 2))]
    (fill 255 @text-alpha)
    (text inst-name text-start half-screen-height))

  (doseq [i (range 1 (+ (count insts) 1))]
    (when (= @index i)
      (synths/update-instrument (nth insts (dec i))))))
