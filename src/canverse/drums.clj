(ns canverse.drums
  (:require [canverse.screen :as screen]
            [overtone.at-at :as a])
  (:use [overtone.core]))

(boot-external-server)

(def hat( sample (freesound-path 802)))
;; (definst hat [volume 1.0]
;;   (let [src (white-noise)
;;         env (env-gen (perc 0.001 0.3) :action FREE)]
;;     (* volume 1 src env)))

(comment (render-hat))

;; (definst hat2 [volume 1.0]
;;   (let [src (white-noise)
;;         env (env-gen (perc 0.002 0.5) :action FREE)]
;;     (* volume 1 src env)))

(comment (render-hat))

;; sampled kick drum
;; from http://www.freesound.org/people/opm/sounds/2086/
;; the overtone freesound API allows you to download freesounds samples
;; by id (2086 in this case)

(def kick (sample (freesound-path 2086)))
(def snare (sample (freesound-path 26903)))

(defn render-kick []
  (kick)
  (reset! screen/instance (screen/activate-alpha-fade 0 @screen/instance)))

(defn render-hat []
  (hat :amp (rand-nth [0.2 0.4 0.75 1])))
  ;(reset! screen/instance (screen/activate-alpha-fade 1 @screen/instance)))

(defn render-snare []
  (snare :amp (rand-nth [0.65 0.75 0.85 1]))
  (reset! screen/instance (screen/activate-alpha-fade 2 @screen/instance)))

(defn render-hat-and-kick []
  (render-hat)
  (render-kick))

(comment (render-kick))

;; we can schedule beats for the future with the at macro:

(comment (at (+ 1000 (now)) (render-kick)))

;; ...and chain multiple beats together with a do form:

(comment
  (let
    [time (now)]
    (at (+    0 time) (render-kick) )
    (at (+  400 time) (render-hat)  )
    (at (+  800 time) (render-kick) )
    (at (+ 1200 time) (render-hat)  )))

;; to repeat, we use the apply-at macro to schedule a recursive call
;; for the future

(defn loop-beats [time]
  (apply-at (+    0 time) #'render-kick )
  (apply-at (+  400 time) #'render-hat)
  (apply-at (+  800 time) #'render-kick)
  (apply-at (+ 1200 time) #'render-hat)
  (apply-at (+ 1600 time) loop-beats (+ 1600 time) []))

(comment (loop-beats (now)))
(stop)
;; rather than thinking in terms of milliseconds, it's useful to think
;; in terms of beats. We can create a metronome to help with this. A
;; metronome counts beats over time. Here's a metronome at 180 beats
;; per minute (bpm):

(defonce metro (metronome 240))

;; we use it as follows:

(metro) ; current beat number
(metro 10) ; timestamp of beat number 3

;; if we rewrite loop-beats using a metronome, it would look like
;; this:

(defn kick-beat [m beat-num]
  (apply-at (m (+ 0 beat-num)) #'render-kick)
  (apply-at (m (+ 0.5 beat-num)) #'render-kick)
  (apply-at (m (+ 1.0 beat-num)) #'render-kick)
  (apply-at (m (+ 2 beat-num)) kick-beat m (+ 2 beat-num) []))

(defn hat-beat [m beat-num]
  (apply-at (m (+ 0 beat-num)) #'render-hat)
  (apply-at (m (+ 0.25 beat-num)) #'render-hat)
  (apply-at (m (+ 0.5 beat-num)) #'render-hat)
  (apply-at (m (+ 1 beat-num)) hat-beat m (+ 1 beat-num) []))

(comment (hat-beat metro (metro)))
(stop)

(defn metro-beats [m beat-num]
  (apply-at (m (+ 0 beat-num)) #'render-kick)
  (apply-at (m (+ 1 beat-num)) #'render-hat)
  (apply-at (m (+ 2.5 beat-num)) #'render-kick)
  (apply-at (m (+ 3 beat-num)) #'render-hat)
  (apply-at (m (+ 4 beat-num)) metro-beats m (+ 4 beat-num) []))

(comment (metro-beats metro (metro)))
(stop)

;; because we're using a metronome, we can change the speed:

;; a more complex rhythm

(defn weak-hat []
  (hat 0.3))

(defn phat-beats [m beat-num]
  (apply-at (m (+ 0 beat-num))    #'render-hat-and-kick)
  (apply-at (m (+ 0.5 beat-num))  #'render-kick)
  (apply-at (m (+ 1 beat-num))    #'render-hat)
  (apply-at (m (+ 1.5 beat-num))  #'render-hat-and-kick)
  (apply-at (m (+ 2 beat-num))    #'render-hat-and-kick)
  (apply-at (m (+ 2.25 beat-num)) #'render-hat-and-kick)
  (apply-at (m (+ 2.5 beat-num))  #'render-kick)
  (apply-at (m (+ 3 beat-num))    #'render-kick)
  (apply-at (m (+ 3.5 beat-num))  #'render-hat)
  (apply-at (m (+ 4 beat-num)) #'phat-beats [m (+ 4 beat-num)]))

(defn disco [m beat-num]
  (apply-at (m (+ 0 beat-num)) #'render-kick)
  (apply-at (m (+ 0 beat-num)) #'render-hat)
  (apply-at (m (+ 0.125 beat-num)) #'render-hat)
  (apply-at (m (+ 0.25 beat-num)) #'render-hat)
  (apply-at (m (+ 0.375 beat-num)) #'render-hat)
  (apply-at (m (+ 0.5 beat-num)) #'render-hat)
  (apply-at (m (+ 0.5 beat-num)) #'render-snare)
  (apply-at (m (+ 0.625 beat-num)) #'render-hat)
  (apply-at (m (+ 0.75 beat-num)) #'render-hat)
  ;(apply-at (m (+ 0.75 beat-num)) #'render-kick)
  (apply-at (m (+ 0.875 beat-num)) #'render-hat)
  (apply-at (m (+ 1 beat-num)) #'render-hat)
  (apply-at (m (+ 1 beat-num)) #'render-kick)

  (apply-at (m (+ 1.125 beat-num)) #'render-hat)
  (apply-at (m (+ 1.25 beat-num)) #'render-hat)
  (apply-at (m (+ 1.25 beat-num)) #'render-kick)
  (apply-at (m (+ 1.375 beat-num)) #'render-hat)
  (apply-at (m (+ 1.5 beat-num)) #'render-hat)
  (apply-at (m (+ 1.5 beat-num)) #'render-snare)
  (apply-at (m (+ 1.625 beat-num)) #'render-hat)
  (apply-at (m (+ 1.75 beat-num)) #'render-hat)
  (apply-at (m (+ 1.75 beat-num)) #'render-snare)
  (apply-at (m (+ 1.875 beat-num)) #'render-hat)
  (apply-at (m (+ 2 beat-num)) #'render-hat)
  (apply-at (m (+ 2 beat-num)) #'render-kick)

  (apply-at (m (+ 2.125 beat-num)) #'render-hat)
  (apply-at (m (+ 2.25 beat-num)) #'render-hat)
  (apply-at (m (+ 2.375 beat-num)) #'render-hat)
  (apply-at (m (+ 2.5 beat-num)) #'render-hat)
  ;(apply-at (m (+ 2.5625 beat-num)) #'render-snare)
  (apply-at (m (+ 2.625 beat-num)) #'render-hat)
  ;(apply-at (m (+ 2.6875 beat-num)) #'render-snare)
  (apply-at (m (+ 2.75 beat-num)) #'render-hat)
  (apply-at (m (+ 2.75 beat-num)) #'render-kick)
  ;(apply-at (m (+ 2.8125 beat-num)) #'render-snare)
  (apply-at (m (+ 2.875 beat-num)) #'render-hat)
  (apply-at (m (+ 2.875 beat-num)) #'render-kick)
  ;(apply-at (m (+ 2.9375 beat-num)) #'render-snare)
  (apply-at (m (+ 3 beat-num)) #'render-hat)
  (apply-at (m (+ 3 beat-num)) #'render-snare)

  (apply-at (m (+ 3.125 beat-num)) #'render-hat)
  (apply-at (m (+ 3.125 beat-num)) #'render-kick)
  (apply-at (m (+ 3.25 beat-num)) #'render-hat)
  (apply-at (m (+ 3.375 beat-num)) #'render-hat)
  (apply-at (m (+ 3.375 beat-num)) #'render-kick)
  (apply-at (m (+ 3.5 beat-num)) #'render-hat)
  (apply-at (m (+ 3.625 beat-num)) #'render-hat)
  (apply-at (m (+ 3.625 beat-num)) #'render-kick)
  (apply-at (m (+ 3.75 beat-num)) #'render-hat)
  (apply-at (m (+ 3.875 beat-num)) #'render-hat)
  (apply-at (m (+ 3.875 beat-num)) #'render-kick)

  (apply-at (m (+ 4 beat-num)) #'disco [m (+ 4 beat-num)]))

(defn phat-beats2 [] (phat-beats metro (metro)))

(defn metro-beats2 [] (metro-beats metro (metro)))

(defn hat-beat2 [] (hat-beat metro (metro)))

(defn kick-beat2 [] (kick-beat metro (metro)))

(defn disco2 [] (disco metro (metro)))

;; and combining ideas from sounds.clj with the rhythm ideas here:

;; first we bring back the dubstep inst

(definst dubstep [freq 100 wobble-freq 5]
  (let [sweep (lin-exp (lf-saw wobble-freq) -1 1 40 5000)
        son   (mix (saw (* freq [0.99 1 1.01])))]
    (lpf son sweep)))

;; define a vector of frequencies from a tune
;; later, we use (cycle notes) to repeat the tune indefinitely

(def notes (vec (map (comp midi->hz note) [:g1 :g2 :d2 :f2 :c2 :c3 :bb1 :bb2
                                           :a1 :a2 :e2 :g2 :d2 :d3 :c2 :c3])))

;; bass is a function which will play the first note in a sequence,
;; then schedule itself to play the rest of the notes on the next beat

(defn bass [m num notes]
  (at (m num)
      (ctl dubstep :freq (first notes)))
  (apply-at (m (inc num)) bass m (inc num) (next notes) []))

(bass metro (metro) (cycle notes))

;; wobble changes the wobble factor randomly every 4th beat

(defn wobble [m num]
  (at (m num)
      (ctl dubstep :wobble-freq
           (choose [4 6 8 16])))
  (apply-at (m (+ 4 num)) wobble m (+ 4 num) []))

;; put it all together
(comment do
  (metro :bpm 40)
  (dubstep) ;; start the synth, so that bass and wobble can change it
  (bass metro (metro) (cycle notes))
  (wobble metro (metro)))

(metro :bpm 40)
;TODO: Build this hash programatically
(def drums-hash (atom {:0 {:drum-loop kick-beat2 :job nil}
                       :1 {:drum-loop hat-beat2 :job nil}
                       :2 {:drum-loop metro-beats2 :job nil}
                       :3 {:drum-loop phat-beats2 :job nil}
                       :4 {:drum-loop disco2 :job nil}
                       :5 {:drum-loop phat-beats2 :job nil}
                       :6 {:drum-loop phat-beats2 :job nil}}))

(defn play-drum-at [index]
  (let [drums @drums-hash
        current-drum (drums index)
        drum-loop (current-drum :drum-loop)
        playing? (not (nil? (current-drum :job)))]
    (if playing?
      (do (a/kill (current-drum :job))
          (update-drums! index nil))
      (update-drums! index (drum-loop)))))

(defn update-drums! [index job]
  (let [drums @drums-hash
        current-drums (drums index)]
    (reset! drums-hash
            (assoc drums index
              (assoc current-drums :job job)))))

(def current-drum-loop (atom phat-beats2))

(defn update! [drum-loop]
  (reset! current-drum-loop drum-loop))

(stop)

