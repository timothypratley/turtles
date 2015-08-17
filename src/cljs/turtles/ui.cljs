(ns turtles.ui
  (:require [turtles.canvas :as canvas]
            [turtles.session :as session]))

(defn main []
  [:div
   [canvas/draw-world @session/state]
   [:button.btn.btn-default
    {:on-click (fn [e]
                 (session/update-in! [:ticking] not))
     :style {:width "100%"}}
    (if (session/get :ticking)
      "Stop"
      "Start")]])

(defn ui []
  [:div
   [main]])
