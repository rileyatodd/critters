(ns critters.dynamic
  (:require [quil.core :as q])
  (:require [genartlib.util :as u])
  (:require [genartlib.random :refer :all])
  (:require [genartlib.algebra :refer :all])
  (:require [clojure.math.numeric-tower :refer :all]))

(defonce play-state (atom {:physics false
                           :color false
                           :all false}))

(defn play-pause [k] 
  (swap! play-state #(update-in % [k] not)))

(defn plot [f xs]
  (doseq [x xs]
    (q/point x (f x))))

(defn plot-points [ps]
  (doseq [[x y] ps]
    (q/point x y)))

(defn random-critter []
  (let [length (gauss 40 10)]
    {:cx (q/random (u/w)) 
     :cy (q/random (u/h)) 
     :l length 
     :w (* length (gauss 0.5 0.1)) 
     :num-legs (gauss 20 5)
     :period (gauss 1 0.1)}))

(defn initial-state []
  {:critters (map (fn [_] (random-critter)) (range 10))})

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :hsb)
  (q/smooth)

  (initial-state))

(defonce reset-state? (atom false))

(defn reset-state []
  (swap! reset-state? (fn [_] true)))

(defn update-state [state]
  (if @reset-state?
    (do
      (swap! reset-state? (fn [_] false))
      (initial-state))
    state))

(defn critter [{:keys [cx cy l w num-legs period]}]
  (let [rad-per-leg (/ q/TWO-PI num-legs)]
    (q/no-fill)
    (q/ellipse cx cy l w)
    (q/with-translation [cx cy]
      (doseq [rad (range 0 q/TWO-PI rad-per-leg)]
        (let [torso-x (* l 0.5 (q/cos rad))
              torso-y (* w 0.5 (q/sin rad))
              foot-r 10
              leg-length-coef 3
              motion-coef 0.5
              length (+ leg-length-coef 
                        (* motion-coef (q/sin (+ (* (q/millis) (/ q/TWO-PI (* 1000 period))) 
                                                 (Math/abs (- rad q/PI))))))
              foot-x (* length torso-x)
              foot-y (* length torso-y)]
          (q/line torso-x torso-y foot-x foot-y)
          (q/ellipse foot-x foot-y foot-r foot-r))))))

(defn draw-state [state]
  (q/background 60)
  (q/stroke 255)
  (doseq [c (:critters state)]
    (critter c)))
  
  
