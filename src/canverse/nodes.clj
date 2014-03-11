(ns canverse.nodes
  (:require [canverse.grid :as grid]
            [canverse.square :as square]
            [canverse.point :as point]
            [canverse.helpers :as helpers]
            [overtone.core :as o]
            [quil.core :as q]))

(defn create []
  {
   ; active nodes are the nodes currently being controlled by
   ; the user's mouse; active is a sequence of hashes {:node, :base}
   ; where :node is the node being played and :base is the base properties
   ; of when and where it started playing, used to calculate changes in
   ; properties as the mouse moves around
   :active nil

   ; releasing nodes are all nodes that are no longer controlled
   ; by the user (e.g. nodes that are fading out)
   :releasing nil})

(defn get-active-nodes [nodes]
  (map #(:node %) (:active nodes)))

(defn get-all [nodes]
  (concat (get-active-nodes nodes) (:releasing nodes)))

(defn activate-at [position grid nodes]
  (if-not (grid/in-bounds? position grid)
    nodes
    (let [square (grid/get-square-for position grid)
          node (square/play square)

          initial-freq (o/node-get-control node :freq)

          ; control the max amplitude using the y-position
          ; of the mouse
          max-amp (- 1 (* 0.1 (:row square)))

          ; base properties are the initial properties from which new
          ; properties will be derived as the mouse moves around
          base-props {:position position :freq initial-freq :max-amp max-amp}
          new-active-node {:node node :base base-props}]
      (update-in nodes [:active] #(conj % new-active-node)))))

(defn release-active [nodes]
  (assoc nodes
    :active nil
    :releasing (concat (:releasing nodes) (get-active-nodes nodes))))

(defn update-with-input [user-input grid nodes]
  (if (grid/in-bounds? (:mouse-pos user-input) grid)
    (cond->> nodes
             (:mouse-tapped? user-input)
             (activate-at (:mouse-pos user-input) grid)

             (:mouse-just-released? user-input)
             (release-active)

             :else (identity))
    (release-active nodes)))

(defn update-single-active [elapsed-time user-input active]
  (let [{:keys [mouse-down-duration mouse-pos]} user-input
        {:keys [node base]} active

        ; deviation is the amount of distance between the point at which
        ; the node started and the current position of the mouse
        deviation (point/minus mouse-pos (:position base))

        ; to keep the transition smooth but the note still sounding nice,
        ; as the user's mouse moves push the current frequency towards the nearest
        ; note of the scale we're working on
        target-freq (nth square/scale (/ (:x mouse-pos) square/SQUARE_SIZE))
        freq-climb 0.5
        current-freq (o/node-get-control node :freq)
        next-freq (helpers/push-towards current-freq target-freq freq-climb)

        amp-climb 0.05
        next-amp (+ (o/node-get-control node :amp) amp-climb)

        ; divide y-deviation by a sufficiently large number to get
        ; small increments in max amplitude as mouse moves around
        intended-max-amp (- (:max-amp base) (/ (:y deviation) 325))
        max-amp (helpers/clamp intended-max-amp 0 1)]
    (o/ctl node :amp (min max-amp next-amp) :freq next-freq)
    active))

(defn update-all-active [elapsed-time user-input nodes]
  (assoc nodes :active
    (map #(update-single-active elapsed-time user-input %)
         (:active nodes))))

(defn is-node-alive? [node]
  (and (o/node-live? node) (> (o/node-get-control node :amp) 0.05)))

(defn get-next-amplitude [elapsed-time node]
  (let [current-amp (o/node-get-control node :amp)
        amp-decrease (/ elapsed-time 2000)]
    (max 0 (- current-amp amp-decrease))))

(defn update-releasing [elapsed-time nodes]
  ; filter out nodes that have finished playing
  (let [live-nodes (filter is-node-alive? (:releasing nodes))
        next-amplitudes (map (partial get-next-amplitude elapsed-time) live-nodes)
        nodes-and-amplitudes (map vector live-nodes next-amplitudes)
        kill-threshold 0.05
        surviving-nodes (map first (filter #(> (second %) kill-threshold) nodes-and-amplitudes))]

    (doseq [[node new-amp] nodes-and-amplitudes]
      ; kill the node once it falls below a small amplitude
      ; to free up memory
      (if (<= new-amp kill-threshold)
        (o/kill node)
        (o/ctl node :amp new-amp)))

    (assoc nodes :releasing surviving-nodes)))

(defn update [elapsed-time user-input grid nodes]
  (->> nodes
       (update-with-input user-input grid)
       (update-all-active elapsed-time user-input)
       (update-releasing elapsed-time)))
