(ns canverse.piano
  (:use [overtone.core]))


(defn- registered-samples
  "Fetch piano samples from the asset store if they have been manually
  registered"
  []
  (filter #(.contains % "LOUD")
          (registered-assets ::MISStereoPiano)))

;;(freesound-searchm [:id] "LOUD" :f "pack:MISStereoPiano")
(def FREESOUND-PIANO-SAMPLES
  "Freesound ids and matching notes for all the loud samples in the MISStereoPiano pack"
  {148401 :BB5})

(def PIANO-SAMPLE-IDS (keys FREESOUND-PIANO-SAMPLES))

(defonce piano-samples
  (doall (map freesound-sample PIANO-SAMPLE-IDS)))

(defn- buffer->midi-note [buf]
  (-> buf :freesound-id FREESOUND-PIANO-SAMPLES name match-note :midi-note))

(defn- note-index
  "Returns a map of midi-note values [0-127] to buffer ids."
  [buffers]
  (reduce (fn [index buf]
            (let [note (buffer->midi-note buf)
                  id   (-> buf :id)]
              (assoc index note id)))
          {}
          buffers))

;; Silent buffer used to fill in the gaps.
(defonce ^:private silent-buffer (buffer 0))

(defonce index-piano-buffer
  (let [tab (note-index piano-samples)
        buf (buffer 128)]
    (buffer-fill! buf (:id silent-buffer))
    (doseq [[idx val] tab]
      (buffer-set! buf idx val))
    buf))
