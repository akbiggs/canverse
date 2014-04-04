(ns canverse.screen
  (:require [canverse.point :as point]
            [quil.core :as q]))

(def instance (atom nil))

(defn create [width height]
  {
   :graphics (q/create-graphics width height)
   :size (point/create width height)
   :alpha-levels '(0 0 0)
   :activating-alphas '(false false false)})

; needs to be invoked during sketch setup due to reliance on q
(defn initialize [width height]
  (reset! instance (create width height)))

(defn fade-active-alphas [elapsed-time screen]
  (assoc screen :alpha-levels
    (for [i (range (count (:alpha-levels screen)))]
      (let [current-alpha-level (nth (:alpha-levels screen) i)]
        (if (nth (:activating-alphas screen) i)
          (min (+ current-alpha-level (* 3 elapsed-time)) 255)
          current-alpha-level)))))

(defn release-fully-alphas [screen]
  (assoc screen :activating-alphas
    (for [i (range (count (:alpha-levels screen)))]
      (and (nth (:activating-alphas screen) i)
           (not= (nth (:alpha-levels screen) i) 255)))))

(defn fade-releasing-alphas [elapsed-time screen]
  (assoc screen :alpha-levels
    (for [i (range (count (:alpha-levels screen)))]
      (let [current-alpha-level (nth (:alpha-levels screen) i)]
        (if (nth (:activating-alphas screen) i)
          current-alpha-level
          (max (- current-alpha-level (* 1 elapsed-time)) 0))))))

(defn update [elapsed-time screen]
  (->> screen
       (fade-active-alphas elapsed-time)
       (fade-releasing-alphas elapsed-time)
       (release-fully-alphas)))

(defn activate-alpha-fade [index screen]
  (update-in screen [:activating-alphas]
    #(for [i (range (count %))]
      (= i index))))

(defn draw [screen]
  (let [gr (:graphics screen)
        half-screen-size (point/scalar-divide (:size screen) 2)
        neg-half-screen-size (point/neg half-screen-size)]
    (q/no-tint)
    (q/image gr 0 0)

    (q/tint 255 175 180 (nth (:alpha-levels screen) 2))
    (q/image gr -5 -5)

    (q/with-translation
     [(:x half-screen-size) (:y half-screen-size)]
     (q/with-rotation
      [0.1]
      (q/tint 255 (nth (:alpha-levels screen) 0))
      (q/image gr (:x neg-half-screen-size) (:y neg-half-screen-size))))))