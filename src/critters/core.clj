(ns critters.core
  (:require [quil.core :as q]
            [quil.middleware :as m])
  (:require [critters.dynamic :as dynamic])
  (:require [genartlib.util :as u])
  (:gen-class))

(q/defsketch critter-sketch
  :title "Crawly Critters"
  :size [500 500]
  ; setup function called only once, during sketch initialization.
  :setup dynamic/setup
  ; update-state is called on each iteration before draw-state.
  :update dynamic/update-state
  :draw dynamic/draw-state
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])

(defn refresh []
  (use :reload 'critters.dynamic)
  (.redraw critter-sketch))

(defn pause []
	(swap! dynamic/pause-state (fn [_] true)))

(defn unpause []
  (swap! dynamic/pause-state (fn [_] false))
  (println "playing...")
  (.redraw critter-sketch))

(defn get-applet []
  critter-sketch)

(comment
  (get-applet)
  (pause)
  (unpause))
