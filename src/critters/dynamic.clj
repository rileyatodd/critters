(ns critters.dynamic
  (:require [quil.core :as q])
  (:require [genartlib.util :as u])
  (:require [genartlib.random :as r :refer :all])
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

(def length-u "" 40)
(def length-v "" 10)
(def leg-length-coef-u "" 3)
(def leg-length-coef-v "" 0.5)
(def speed-u "" 3)
(def speed-v "" 0.5)
(def period-u "" 1.5)
(def period-v "" 0.12)
(def num-legs-u "" 44)
(def num-legs-v "" 3)
(def foot-r-u "" 2.5)
(def foot-r-v "" 0.03)
(def width-coef-u "" 0.5)
(def width-coef-v "" 0.1)
(def wavelength-u "" (/ q/PI 16)); in radians?
(def wavelength-v "" 0.01)

(defn random-critter []
  (let [length (r/gauss length-u length-v)
        leg-length-coef (r/gauss leg-length-coef-u leg-length-coef-v)
        speed (max 0.1 (r/gauss (+ speed-u (* 0.067 (- length-u length))) speed-v))
        period (r/gauss (+ period-u (* 0.6 (- speed-u speed))) period-v)
        num-legs (max 24 (r/gauss (+ num-legs-u (* 0.8 (- length length-u))) num-legs-v))
        foot-r (max 1.4 (r/gauss (+ foot-r-u (* 0.07 (- num-legs-u num-legs))
                                           (* 0.1 (- length length-u))) foot-r-v))
        wavelength (r/gauss (+ wavelength-u (* -0.004 (- num-legs num-legs-u))) wavelength-v)]
    {:cx (q/random (u/w))
     :cy (q/random (u/h))
     :l length
     :w (* length (r/gauss width-coef-u width-coef-v))
     :num-legs num-legs
     :foot-r foot-r
     :period period
     :wavelength wavelength
     :leg-length-coef leg-length-coef
     :motion-amp (r/gauss (* leg-length-coef length 0.06) 0.4)
     :speed speed
     :hue (rand 255)}))

(defn random-point-on-side [i]
  (cond
    (= i 0) [0 (rand (u/h))]
    (= i 1) [(rand (u/w)) 0]
    (= i 2) [(u/w) (rand (u/h))]
    (= i 3) [(rand (u/w)) (u/h)]))

(defn random-line-thu-sketch []
  (mapcat #(random-point-on-side %) (take 2 (shuffle (range 4)))))

(defn scale-line-about-midpoint [[x0 y0 x1 y1] scale]
  (let [rise (- y1 y0)
        run (- x1 x0)
        mid-x (+ x0 (/ run 2))
        mid-y (+ y0 (/ rise 2))
        new-half-rise (* scale 0.5 rise)
        new-half-run (* scale 0.5 run)]
    [(- mid-x new-half-run) (- mid-y new-half-rise)
     (+ mid-x new-half-run) (+ mid-y new-half-rise)]))

(defn random-critter-on-line []
  (let [line (scale-line-about-midpoint (random-line-thu-sketch) 2)]
    (merge (random-critter) {:cx (nth line 0) :cy (nth line 1) :line line})))

(defn initial-state []
  {:critters (map (fn [_] (random-critter-on-line))
                  (range 10))})

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :hsb)
  (q/smooth)

  (initial-state))

(defonce reset-state? (atom false))

(defn reset-state []
  (swap! reset-state? (fn [_] true)))

(defonce pause-state (atom false))

(defn update-state [state]
	 (if @pause-state
	 	 (q/no-loop)
	 	 (q/start-loop))
  (if @reset-state?
    (do
      (swap! reset-state? (fn [_] false))
      (initial-state))
    {:critters (as-> (:critters state) cs
         (filter #(and (u/between? (:cx %) (nth (:line %) 0) (nth (:line %) 2))
                       (u/between? (:cy %) (nth (:line %) 1) (nth (:line %) 3))) cs)
         (if (< (rand) 0.025) (conj cs (random-critter-on-line)) cs)
         (map (fn [c]
                (let [speed (:speed c)
                      [x0 y0 x1 y1] (:line c)
                      slope (/ (- y1 y0) (- x1 x0))
                      angle (apply angle (:line c))
                      dx (* speed (q/cos angle))
                      dy (* speed (q/sin angle))]
                  (merge c {:cx (+ dx (:cx c))
                            :cy (+ dy (:cy c))})))
              cs))}))

(defn critter [{:keys [l w num-legs period wavelength leg-length-coef motion-amp period foot-r]}]
  (let [rad-per-leg (/ q/TWO-PI num-legs)]
    (q/no-fill)
    (q/stroke-weight 2)
    (q/ellipse 0 0 l w)
    (q/stroke-weight 1)
    (doseq [rad (range q/PI 0 (- rad-per-leg))]
      (let [torso-x (* l 0.5 (q/cos rad))
            torso-y (* w 0.5 (q/sin rad))
            foot-d (* 2 foot-r)
            motion-t (* (q/millis) (/ q/TWO-PI (* 1000 period)))
            motion-coef (* motion-amp (q/sin (- (+ motion-t
                                                   (Math/abs (- (/ rad wavelength) q/PI))))))
            motion-dx 0 #_(* -1 motion-coef (q/cos rad)) ;non-zero motion-dx makes critters seem to be crawling rather than swimming
            motion-dy (* motion-coef (q/sin rad))
            length (+ leg-length-coef motion-coef)
            ankle-x (+ motion-dx (* leg-length-coef torso-x))
            ankle-y (+ motion-dy (* leg-length-coef torso-y))
            foot-x (+ ankle-x (* (q/cos rad) foot-r))
            foot-y (+ ankle-y (* (q/sin rad) foot-r))]
        (q/line torso-x torso-y ankle-x ankle-y)
        (q/line torso-x (- torso-y) ankle-x (- ankle-y))
        (q/ellipse foot-x foot-y foot-d foot-d)
        (q/ellipse foot-x (- foot-y) foot-d foot-d)))))

(defn draw-state [state]
  (q/background 160)
  (q/stroke 255)
  (doseq [c (:critters state)]
    (q/with-translation [(:cx c) (:cy c)]
      (q/with-rotation [(+ q/PI (apply angle (:line c)))]
        (q/stroke (:hue c) 195 105)
        (critter c)))))

(comment
  (reset-state)

)
