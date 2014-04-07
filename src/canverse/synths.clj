(ns canverse.synths
  (:use [overtone.core]
        [canverse.helpers]
        [canverse.piano :only [index-piano-buffer]])
  (:require [quil.core :as q]
            [clojure.string :as str]))

(boot-external-server)

(definst plucked-string [freq 60 amp 0.8 dur 2 attack 0.5 decay 30 sustain 0.5 release 0.5 coef 0.3 gate 1]
  (let [freq (midicps freq)
        noize (* 0.8 ( white-noise))
        dly (/ 1.0 freq)
        ;plk (pluck noize gate dly dly decay coef)
        ;dist (distort plk)
        ;filt (rlpf dist (* 12 freq) 0.6)
        ;clp (clip2 filt 0.8)
        reverb (free-verb noize 0.4 0.8 0.2)]
    (* amp (env-gen (adsr attack decay sustain release) :action FREE) reverb)))

(demo (saw (midicps (* 400 (distort (pink-noise))))))
(odoc rlpf)
(demo
 (let [freq 60 attack 0.5 decay 0.5 sustain 0.8 release 1 gate 0.5
       snd (pulse (midicps freq) 0.5)
       ;snd (rlpf snd 2000)

       env (env-gen (adsr attack decay sustain release) gate :action FREE)
       ]
   (* snd env)))

(definst test [freq 60]
  (let [snd (saw (midicps freq))
        noise (* 0.5 (pink-noise))
        env (env-gen (perc) :action FREE)]
    (* env snd)))

(test 40)
(stop)

;piano instrument
(definst piano [note 60
                gate 1
                vel 100
                decay 0.8
                release 0.8
                hard 0.8
                velhard 0.8
                muffle 0.8
                velmuff 0.8
                velcurve 0.8
                stereo 0.2
                tune 0.5
                random 0.1
                stretch 0.1
                sustain 0.1]
  (let [snd (mda-piano {:freq (midicps note)
                        :gate gate
                        :vel vel
                        :decay decay
                        :release release
                        :hard hard
                        :velhard velhard
                        :muffle muffle
                        :velmuff velmuff
                        :velcurve velcurve
                        :stereo stereo
                        :tune tune
                        :random random
                        :stretch stretch
                        :sustain sustain})]
    (detect-silence snd 0.005 :action FREE)
    (* 1 snd)))

(definst sampled-piano
  [freq 60 amp 1 level 1 rate 1 loop? 0
   attack 0 decay 1 sustain 1 release 0.1 curve -4 gate 1]
  (let [buf (index:kr (:id index-piano-buffer) freq)
        env (env-gen (adsr attack decay sustain release level curve)
                     :gate gate
                     :action FREE)]
    (* env amp (scaled-play-buf 2 buf :rate rate :level level :loop loop? :action FREE))))


; climb is a hack parameter that isn't used here, but is instead
; controlled in the main update loop based on the mouse movement
(definst oksaw [freq 60 velocity 100 gate 1 amp 1 climb 100]
  (let [
        ; Set up a normal amplitude and amp envelope.
        amp-env (env-gen (adsr 0 0.6 0.75 0.9) gate :action FREE)

        ; Get three frequencies (primary freq and detunes at 40 and 22 cents higher)
        freq (midicps freq)
        dfreq1 (* freq (Math/pow 2 0.040))
        dfreq2 (* freq (Math/pow 2 0.022))

        ; Set up delay oscillator from zero to a quarter period away
        quarter-period (/ 1 (* 4 freq))
        delay-osc (* quarter-period (+ 0.5 (* 0.5 (sin-osc 1))))

        ; Make three saws from the frequencies and offset them a bit
        ; against the delay oscillator
        saw1 (* (/ amp 3) (saw freq))
        saw2 (delay-l (* (/ amp 3) (saw dfreq1)) 1 (/ delay-osc 2))
        saw3 (delay-l (* (/ amp 3) (saw dfreq2)) 1 (/ delay-osc 3))

        ; Set a cut-off envelope.  We'll use this with an LPF to give the
        ; sound a bit of a stabby attack
        cutoff-env (env-gen (adsr 0.25 1 1 9999) gate)

        ; Combine the saws
        snd (+ saw1 saw2 saw3)

        ; Add the stabby attack part
        snd (lpf snd (* 16000 cutoff-env))

        ; Apply some EQ (Up the bass, drop the mids)

        snd (b-low-shelf snd 80 1 10)
        snd (b-peak-eq snd 800 1 -10)

        snd (b-hi-shelf snd 2000 1 5)

        ; Apply the amp envelope
        snd (* amp-env snd)]
      snd))

(def current-instrument (atom oksaw))

(@current-instrument :freq 40)
(stop)

(defn generic-player [attack decay sustain release level curve bias]
  (definst generic-synth [freq 60 amp 1]
    (let [

          ; Default synth values
          velocity 100
          gate 1
          climb 100
          level (max level 0.5)


          ; Update envelope values

          ; Set up a normal amplitude and amp envelope.
          amp-env (env-gen
                   (apply adsr (list attack decay sustain release level curve bias))
                   gate :action FREE)

          ; Get three frequencies (primary freq and detunes at 40 and 22 cents higher)
          freq (midicps freq)
          dfreq1 (* freq (Math/pow curve (* 0.040 decay 9)))
          dfreq2 (* freq (Math/pow curve (* 0.022 (/ decay 9))))

          ; Set up delay oscillator from zero to a quarter period away
          quarter-period (/ 1 (* 4 freq))
          delay-osc (* quarter-period (+ 0.5 (* 0.5 (sin-osc 1))))

          ; Make three saws from the frequencies and offset them a bit
          ; against the delay oscillator
          saw1 (* (/ amp 3) (saw freq))
          saw2 (delay-l (* (/ amp 3) (saw dfreq1)) 1 (/ delay-osc 2))
          saw3 (delay-l (* (/ amp 3) (saw dfreq2)) 1 (/ delay-osc 3))

          ; Set a cut-off envelope.  We'll use this with an LPF to give the
          ; sound a bit of a stabby attack
          cutoff-env (env-gen (adsr 0.25 1 1 9999) gate)

          ; Combine the saws
          snd (+ saw1 saw2 saw3)

          ; Add the stabby attack part
          snd (lpf snd (* (* attack 1000) cutoff-env))

          ; Apply some EQ (Up the bass, drop the mids)

          snd (b-low-shelf snd 800 1 attack)
          snd (b-peak-eq snd (* 800  (/ attack 9)) 1 (- attack))

          snd (b-hi-shelf snd (* 2000 (/ attack 9)) 1 5)

          ; Apply the amp envelope
          snd (* amp-env snd)]
        snd)))

(defn update-instrument [instrument]
;  (dbg instrument)
  (reset! current-instrument instrument))

(defn create-synth [input]
  (if (not (nil? input))
    (apply generic-player input)
    @current-instrument))

(defn update [input]
  (if (not (nil? input))
    (update-instrument (create-synth input))
    (update-instrument (create-synth nil))))

(definst plucked-string [note 60 amp 0.8 dur 2 decay 30 coef 0.3 gate 1]
  (let [freq (midicps note)
        noize (* 0.8 ( white-noise))
        dly (/ 1.0 freq)
        plk (pluck noize gate dly dly decay coef)
        dist (distort plk)
        filt (rlpf dist (* 12 freq) 0.6)
        clp (clip2 filt 0.8)
        reverb (free-verb clp 0.4 0.8 0.2)]
    (* amp (env-gen (perc 0.0001 dur) FREE) reverb)))


(definst monotron
  "Korg Monotron from website diagram:
   http://korg.com/services/products/monotron/monotron_Block_diagram.jpg."
  [note     60            ; midi note value
   volume   0.7           ; gain of the output
   mod_pitch_not_cutoff 1 ; use 0 or 1 only to select LFO pitch or cutoff modification
   pitch    0.0           ; frequency of the VCO
   rate     4.0           ; frequency of the LFO
   int      1.0           ; intensity of the LFO
   cutoff   1000.0        ; cutoff frequency of the VCF
   peak     0.5           ; VCF peak control (resonance)
   pan      0             ; stereo panning
   ]
  (let [note_freq       (midicps note)
        pitch_mod_coef  mod_pitch_not_cutoff
        cutoff_mod_coef (- 1 mod_pitch_not_cutoff)
        LFO             (* int (saw rate))
        VCO             (saw (+ note_freq pitch (* pitch_mod_coef LFO)))
        vcf_freq        (+ cutoff (* cutoff_mod_coef LFO) note_freq)
        VCF             (moog-ff VCO vcf_freq peak)
        ]
    ;(tap "monotron-volume" 30 VCF)
    (out 0 (pan2 (* volume VCF) pan))))

(defsynth space-reverb
  [out-bus 0
   in-bus 0
   gate 1
   threshold 0.1
   amp 0.1]
  (let [env             (linen gate 0.1 1 0.1 FREE)
        num-combs       6
        num-allpass     4
        src             (in in-bus 2)
        reverb-predelay (delay-n src 0.048 0.048)
        y               (mix (repeat num-combs (comb-l reverb-predelay 0.1 (ranged-rand 0.01 0.1) 5)))
        y               (loop [cnt num-allpass
                               res y]
                          (if (<= cnt 0)
                            res
                            (recur (dec cnt)
                                   (allpass-n res
                                              0.051
                                              [(ranged-rand 0.01 0.05)
                                               (ranged-rand 0.01 0.05)]
                                              1))))]
    (out out-bus (* env amp (pan2 y)))))

(defsynth sing [freq 380
                freq0 400  freq1 750 freq2 2400 freq3 2600 freq4 2900
                amps0 1 amps1 0.28 amps2 0.08 amps3 0.1 amps4 0.01
                qs0 0.1 qs1 0.1 qs2 0.04 qs3 0.04 qs4 0.04
                out-bus 0 vibrato-speed 6 vibrato-depth 4 lag-val 0.5 gate 1]
  (let [freqs-list (map #(lag:kr % lag-val) [freq0 freq1 freq2 freq3 freq4])
        amps-list  (map #(lag:kr (dbamp %) lag-val) [amps0 amps1 amps2 amps3 amps4] )
        qs-list    (map #(lag:kr % lag-val) [qs0 qs1 qs2 qs3 qs4])

        vibrato (* vibrato-depth (sin-osc:kr vibrato-speed))
        in (saw:ar (lag:kr (+ freq vibrato) 0.2))
        env (env-gen:kr (env-asr 1) gate FREE)
        snd (mix (* amps-list (bpf:ar in freqs-list qs-list)))]
    (out out-bus (* snd env))))

;;http://sccode.org/1-j
(defsynth dark-sea-horns
  "Dark, rough and sharp sea horn.
   Without any attempt to use recursion (recursive example lacked the feedback feel)"
  [out-bus 0 amp 1 freq 65]
  (let [a (tanh (* 6 (lf-noise1:ar 3) (sin-osc:ar freq (* (lf-noise1:ar 0.1) 3))))

        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (tanh a)

        a (tanh (* 6 (lf-noise1:ar 3) (sin-osc:ar freq (* a (lf-noise1:ar 0.1) 3))))
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (tanh a)

        a (tanh (* 6 (lf-noise1:ar 3) (sin-osc:ar freq (* a (lf-noise1:ar 0.1) 3))))
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (tanh a)

        a (tanh (* 6 (lf-noise1:ar 3) (sin-osc:ar freq (* a (lf-noise1:ar 0.1) 3))))
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (tanh a)

        a (tanh (* 6 (lf-noise1:ar 3) (sin-osc:ar freq (* a (lf-noise1:ar 0.1) 3))))
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (tanh a)

        a (tanh (* 6 (lf-noise1:ar 3) (sin-osc:ar freq (* a (lf-noise1:ar 0.1) 3))))
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (tanh a)

        a (tanh (* 6 (lf-noise1:ar 3) (sin-osc:ar freq (* a (lf-noise1:ar 0.1) 3))))
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (tanh a)

        a (tanh (* 6 (lf-noise1:ar 3) (sin-osc:ar freq (* a (lf-noise1:ar 0.1) 3))))
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (tanh a)]
    (out out-bus (* amp [ a a]))))

; DEFINING FLUTE


(def FREESOUND-VIBRATO-FLUTE-SAMPLES
  "Freesound ids for all samples in the Vibrato Transverse Flute pack"
  {154274 :C7  154273 :B6 154272 :A#6 154271 :A6 154270 :G#6 154269 :G6  154268 :F#6
   154267 :F6  154266 :E6 154265 :D#6 154264 :D6 154263 :C#6 154262 :C6  154261 :B5
   154260 :A#5 154259 :A5 154258 :G#5 154257 :G5 154256 :F#5 154255 :F5  154254 :E5
   154253 :D#5 154252 :D5 154251 :C#5 154250 :C5 154249 :B4  154248 :A#4 154247 :A4
   154246 :G#4 154245 :G4 154244 :F#4 154243 :E4 154242 :F4  154241 :D#4 154240 :D4
   154239 :C#4 154238 :C4})

(def FLUTE-SAMPLE-IDS (keys FREESOUND-VIBRATO-FLUTE-SAMPLES))

(def flute-samples
  (doall (map freesound-sample FLUTE-SAMPLE-IDS)))

(defn- buffer->midi-note [buf]
  (-> buf :freesound-id FREESOUND-VIBRATO-FLUTE-SAMPLES name note))

(defn- note-index
  "Returns a map of midi-note values [0-127] to buffer ids."
  [buffers]
  (reduce (fn [index buf]
            (let [id (-> buf :id)
                  note (buffer->midi-note buf)]
              (assoc index note id)))
          {}
          buffers))

;; Silent buffer used to fill in the gaps.
(defonce ^:private silent-buffer (buffer 0))

(defonce index-flute-buffer
  (let [tab (note-index flute-samples)
        buf (buffer 128)]
    (buffer-fill! buf (:id silent-buffer))
    (doseq [[idx val] tab]
      (buffer-set! buf idx val))
    buf))

(definst sampled-flute-vibrato
  [freq 60 amp 1 rate 1
   attack 0 decay 1 sustain 10 release 0.1 curve -4 gate 1]
  (let [buf (index:kr (:id index-flute-buffer) freq)
        env (env-gen (adsr attack decay sustain release :amp amp :curve curve)
                     :gate gate
                     :action FREE)]
    (* 1.2 env (scaled-play-buf 2 buf :level amp :loop 0 :action FREE))))

(defsynth pulse-synth [freq 60 amp 0.8]
  (let [snd (pulse (midicps freq))]
    (out 0 (* amp snd))))

(defmacro create-from-sample [synth-name sample-name]
  `(let [buffer# (load-sample ~(str "samples/" sample-name))]
     (defsynth ~(symbol synth-name) [~'freq 60 ~'amp 0.8]
       (let [samp# (play-buf 2 buffer# :loop 1)
             snd# (~'* ~'amp samp#)
             snd# (lpf snd# (midicps
                             (~'* 5 (~'+ 8 (~'- ~'freq 65)))))
             snd# (~'* 1.5 snd#)]
         (out 0 snd#)))))

(create-from-sample "awesome" "massive3.wav")
(create-from-sample "deus" "deus2.wav")
(create-from-sample "weird" "garage2.wav")
(create-from-sample "vector" "nirvana1b.wav")
(create-from-sample "chase" "nirvana2b.wav")
(create-from-sample "fight" "nirvana3b.wav")
(create-from-sample "fight-downtime" "nirvana3c.wav")
(create-from-sample "climax" "climax2.wav")
(create-from-sample "resonant" "resonant2.wav")

(stop)
