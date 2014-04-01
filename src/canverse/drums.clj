(ns canverse.drums
  (:use [overtone.core]))

(boot-external-server)

(definst hat [volume 1.0]
  (let [src (white-noise)
        env (env-gen (perc 0.001 0.3) :action FREE)]
    (* volume 1 src env)))

(comment (hat))

(definst hat2 [volume 1.0]
  (let [src (white-noise)
        env (env-gen (perc 0.002 0.5) :action FREE)]
    (* volume 1 src env)))

(comment (hat))

;; sampled kick drum
;; from http://www.freesound.org/people/opm/sounds/2086/
;; the overtone freesound API allows you to download freesounds samples
;; by id (2086 in this case)

(def kick (sample (freesound-path 2086)))

(comment (kick))

;; we can schedule beats for the future with the at macro:

(comment (at (+ 1000 (now)) (kick)))

;; ...and chain multiple beats together with a do form:

(comment
  (let
    [time (now)]
    (at (+    0 time) (kick) )
    (at (+  400 time) (hat)  )
    (at (+  800 time) (kick) )
    (at (+ 1200 time) (hat)  )))

;; to repeat, we use the apply-at macro to schedule a recursive call
;; for the future

(defn loop-beats [time]
  (at (+    0 time) (kick) )
  (at (+  400 time) (hat)  )
  (at (+  800 time) (kick) )
  (at (+ 1200 time) (hat)  )
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

(defn metro-beats [m beat-num]
  (at (m (+ 0 beat-num)) (kick))
  (at (m (+ 1 beat-num)) (hat))
  (at (m (+ 2.5 beat-num)) (kick))
  (at (m (+ 2.5 beat-num)) (kick))
  (at (m (+ 3 beat-num)) (hat))
  (apply-at (m (+ 4 beat-num)) metro-beats m (+ 4 beat-num) []))

(metro-beats metro (metro))
(stop)

;; because we're using a metronome, we can change the speed:

(metro :bpm 180) ;slower
(metro :bpm 500) ;faster
(stop)

;; a more complex rhythm

(defn weak-hat []
  (hat 0.3))


(defn phat-beats [m beat-num]
  (at (m (+ 0 beat-num)) (kick) (weak-hat))
  (at (m (+ 0.5 beat-num)) (kick))
  (at (m (+ 1 beat-num))        (hat))
  (at (m (+ 1.5 beat-num)) (kick) (weak-hat))
  (at (m (+ 2 beat-num)) (kick) (weak-hat))
  (at (m (+ 2.25 beat-num)) (kick) (weak-hat))
  (at (m (+ 2.5 beat-num)) (kick))
  (at (m (+ 3 beat-num)) (kick) (hat2) )
  (at (m (+ 3.5 beat-num))        (weak-hat) )
  (apply-at (m (+ 4 beat-num)) phat-beats m (+ 4 beat-num) []))


(phat-beats metro (metro))
(metro :bpm  180)
(stop)

(defn phat-beats2 [] (phat-beats metro (metro)))

(defn metro-beats2 [] (metro-beats metro (metro)))
(phat-beats2)
;(kill 1)
(stop)


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
(do
  (metro :bpm 40)
  (dubstep) ;; start the synth, so that bass and wobble can change it
  (bass metro (metro) (cycle notes))
  (wobble metro (metro)))

(stop)

(def current-drum-loop (atom phat-beats2))

(defn update! [drum-loop]
  (reset! current-drum-loop drum-loop))
