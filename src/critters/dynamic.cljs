(ns critters.dynamic
  (:require-macros [quil.core :as q])
  (:require [quil.core :as q]
            [critters.util :as u]
            [cljs.pprint :refer [pprint]]))

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
  (let [length (u/gauss length-u length-v)
        leg-length-coef (u/gauss leg-length-coef-u leg-length-coef-v)
        speed (max 0.1 (u/gauss (+ speed-u (* 0.067 (- length-u length))) speed-v))
        period (u/gauss (+ period-u (* 0.6 (- speed-u speed))) period-v)
        num-legs (max 24 (u/gauss (+ num-legs-u (* 0.8 (- length length-u))) num-legs-v))
        foot-r (max 1.4 (u/gauss (+ foot-r-u (* 0.07 (- num-legs-u num-legs))
                                           (* 0.1 (- length length-u))) foot-r-v))
        wavelength (u/gauss (+ wavelength-u (* -0.004 (- num-legs num-legs-u))) wavelength-v)]
    {:cx (q/random (u/w))
     :cy (q/random (u/h))
     :l length
     :w (* length (u/gauss width-coef-u width-coef-v))
     :num-legs num-legs
     :foot-r foot-r
     :period period
     :wavelength wavelength
     :leg-length-coef leg-length-coef
     :max-r (* 1.2 length leg-length-coef)
     :motion-amp (u/gauss (* leg-length-coef length 0.06) 0.4)
     :speed speed
     :hue (rand 255)}))

(defn random-point-on-rect-side [i min-x max-x min-y max-y]
  ;; Don't let the point be too close to a corn
  (cond
    (= i 0) [min-x (u/rand-between min-y max-y)]
    (= i 1) [(u/rand-between min-x max-x) min-y]
    (= i 2) [max-x (u/rand-between min-y max-y)]
    (= i 3) [(u/rand-between min-x max-x) max-y]))

(defn random-line-thu-sketch []
  (let [max-x (* .85 (u/w))
        max-y (* .85 (u/h))
        min-x (* .15 (u/w))
        min-y (* .15 (u/h))]
    (mapcat #(random-point-on-rect-side % min-x max-x min-y max-y)
            (take 2 (shuffle (range 4))))))

(defn flip-line [[x0 y0 x1 y1]]
  [x1 y1 x0 y0])

(defn extend-line-to-boundary [min-x min-y max-x max-y [x0 y0 x1 y1]]
  (let [dy (- y1 y0)
        dx (- x1 x0)
        ybound (if (neg? dy) min-y max-y)
        xbound (if (neg? dx) min-x max-x)
        xdist (- xbound x1)
        ydist (- ybound y1)]
    (if (< (/ xdist dx)
           (/ ydist dy))
      [x0 y0 xbound (+ y1 (* xdist (/ dy dx)))]
      [x0 y0 (+ x1 (* ydist (/ dx dy))) ybound])))

(defn extend-line-to-both-boundaries [min-x min-y max-x max-y line]
  (->> line
       flip-line
       (extend-line-to-boundary min-x min-y max-x max-y)
       flip-line
       (extend-line-to-boundary min-x min-y max-x max-y)))

(defn scale-line-about-midpoint [[x0 y0 x1 y1] scale]
  (let [rise (- y1 y0)
        run (- x1 x0)
        factor (/ (- scale 1) 2)
        extra-rise (* factor rise)
        extra-run (* factor run)]
    [(- x0 extra-run) (- y0 extra-rise)
     (+ x1 extra-run) (+ y1 extra-rise)]))

(defn random-critter-on-line []
  (let [small-line (random-line-thu-sketch)
        bigger-line (scale-line-about-midpoint small-line 2)
        critter (random-critter)
        max-r (:max-r critter)
        line (extend-line-to-both-boundaries (- max-r) (- max-r) (+ max-r (u/w)) (+ max-r (u/h)) small-line)
        speed (:speed critter)
        angle (apply u/angle line)
        heading [(q/cos angle) (q/sin angle)]]
    (merge critter
           {:cx (nth line 0)
            :cy (nth line 1)
            :line line
            :angle angle
            :heading heading
            :small-line small-line
            :velocity (into [] (map #(* speed %) heading))})))

(defn critter-on-screen? [critter]
  (let [screen-w (u/w)
        screen-h (u/h)
        max-r (:max-r critter)
        min-x (- max-r)
        max-x (+ screen-w max-r)
        min-y (- max-r)
        max-y (+ screen-h max-r)]
    #_(pprint [critter [min-x max-x min-y max-y]])
    (and (u/between? (:cx critter) min-x max-x)
         (u/between? (:cy critter) min-y max-y))))

(defn critter-on-line? [critter]
  (let [[x0 y0 x1 y1] (:line critter)]
    (and (u/between? (:cx critter) x0 x1)
         (u/between? (:cy critter) y0 y1))))

(defn initial-state []
  {:critters (map (fn [_] (random-critter-on-line))
                  (range 10))
   :time-coef 1
   :brightness 128})

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :hsb)
  (q/smooth)

  (initial-state))

(defonce reset-state? (atom false))

(defn reset-state []
  (swap! reset-state? (fn [_] true)))

(defonce pause-state (atom false))

(defn state-step [state]
  (let [millis (q/millis)]
    (merge state
           {:critters
            (as-> (:critters state) cs
              (filter (fn [c]
                        (if (critter-on-line? c)
                          (conj acc c)
                          (do #_(pprint c)
                              acc)))
                      cs)
              (if (< (rand) (* 0.025 (:time-coef state)))
                (conj cs (random-critter-on-line))
                cs)
              (map (fn [c]
                     (let [[dx dy] (map #(* (:time-coef state) %) (:velocity c))]
                       (merge c {:cx (+ dx (:cx c))
                                 :cy (+ dy (:cy c))})))
                   cs))
            :t (+ (:t state)
                  (* (:time-coef state)
                     (- millis (:last-millis state))))
            :last-millis millis})))

(defn update-state [state]
	 (if @pause-state
	 	 (q/no-loop)
	 	 (q/start-loop))
  (if @reset-state?
    (do
      (swap! reset-state? (fn [_] false))
      (initial-state))
    (state-step state)))

(defn critter [{:keys [l w num-legs period wavelength leg-length-coef motion-amp period foot-r]} time]
  (let [rad-per-leg (/ q/TWO-PI num-legs)
        ]
    (q/no-fill)
    (q/stroke-weight 2)
    (q/ellipse 0 0 l w)
    (q/stroke-weight 1)
    (doseq [rad (range q/PI 0 (- rad-per-leg))]
      (let [torso-x (* l 0.5 (q/cos rad))
            torso-y (* w 0.5 (q/sin rad))
            foot-d (* 2 foot-r)
            motion-t (* time (/ q/TWO-PI (* 1000 period)))
            motion-coef (* motion-amp (q/sin (+ motion-t
                                                (Math/abs (- (/ rad wavelength) q/PI)))))
            ;; phase offset for legs on each side
            sides-offset q/QUARTER-PI
            motion-coef-off (* motion-amp (q/sin (+ motion-t
                                                    (Math/abs (- (/ rad wavelength) q/PI))
                                                    sides-offset)))
            motion-dx 0 #_(* -1 motion-coef (q/cos rad)) ;non-zero motion-dx makes critters seem to be crawling rather than swimming
            motion-dy (* motion-coef (q/sin rad))
            motion-dy-off (* motion-coef-off (q/sin rad))
            ankle-x (+ motion-dx (* leg-length-coef torso-x))
            ankle-y (+ motion-dy (* leg-length-coef torso-y))
            ankle-y-off (+ motion-dy-off (* leg-length-coef torso-y))
            foot-x (+ ankle-x (* (q/cos rad) foot-r))
            foot-y (+ ankle-y (* (q/sin rad) foot-r))
            foot-y-off (+ ankle-y-off (* (q/sin rad) foot-r))]
        (q/line torso-x torso-y ankle-x ankle-y)
        (q/line torso-x (- torso-y) ankle-x (- ankle-y-off))
        (q/ellipse foot-x foot-y foot-d foot-d)
        (q/ellipse foot-x (- foot-y-off) foot-d foot-d)))))

(defn draw-state [state _]
  (q/background 160 12 240)
  (q/stroke 255)
  (doseq [c (:critters state)]
    (q/stroke (:hue c) 215 (:brightness state))
    (q/with-translation [(:cx c) (:cy c)]
      (q/with-rotation [(+ q/PI (apply u/angle (:line c)))]
        (critter c (:t state))))))

(comment
  (reset-state)
  (random-point-on-side 3)
  (swap! pause-state (fn [_] true))
)
