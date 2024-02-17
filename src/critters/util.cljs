(ns critters.util
  (:require-macros [quil.core :as q])
  (:require [quil.core :as q]))

(defn h
  "Returns a given percentage of the height Quil-specific."
  ([] (h 1.0))
  ([percentage] (* (q/height) percentage)))

(defn w
  "Returns a given percentage of the width. Quil-specific."
  ([] (w 1.0))
  ([percentage] (* (q/width) percentage)))

(defn between?
  "Returns true if value is between end1 and end2"
  [value end1 end2]
  (and (>= value (min end1 end2))
       (<= value (max end1 end2))))

;; TODO make args not need to be ordered
(defn rand-between [min max]
  (+ min (rand (- max min))))

(defn gauss
  "Samples a single value from a Gaussian distribution with the given mean
   and variance"
  [mean variance]
  (+ mean (* variance (q/random-gaussian))))

(defn angle
  "Returns the angle between two points in radians. Values are between
   0 and 2 * PI."
  [x1 y1 x2 y2]
  (let [a (q/atan2 (- y2 y1) (- x2 x1))]
    (if (neg? a)
      (+ a (* q/PI 2.0))
      a)))

(defn clamp [low high x]
  (max low (min high x)))

(comment
  (angle l)
)
