(ns groovyclojure.synths
  (:use [overtone.live]))

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

(definst oksaw [note 60 velocity 100 gate 1 bass-presence 0]
  (let [freq (midicps note)
        amp 0.33
        snd (sin-osc freq)
        env (env-gen (perc 0.0001 gate) gate :action FREE)
        del (* (/ 1 (* 2 freq)) (+ 0.25 (* 0.25 (sin-osc 1))))
        dfreq1 (* freq (Math/pow 2 (/ 40 1000)))
        dfreq2 (* freq (Math/pow 2 (/ 22 1000)))
        cutoff-env (env-gen (adsr 0.25 1 1 2) gate)
        snd (lpf
              (+
                (* bass-presence amp (sin-osc (/ freq 2)))
                (* amp (saw freq))
                (delay-l (* amp (saw dfreq1)) 1 (/ del 2))
                (delay-l (* amp (saw dfreq2)) 1 (/ del 3)))
              (* 16000 cutoff-env))
        snd (b-low-shelf snd 80 1 10)
        snd (b-peak-eq snd 800 1 0)
        snd (b-hi-shelf snd 2000 1 5)]
    (* env snd)))

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
