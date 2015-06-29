(ns turtles.ui
  (:require [turtles.canvas :as canvas]
            [turtles.session :as session]))

(defn control-box []
  [:form {:on-submit (fn [e]
                       (.preventDefault e))}
   [:h4 "Write your own autonima"]
   [:div.form-group
    [:label {:for "program"}]
    [:textarea.form-control {:id "program"}]]
   [:button.btn.btn-default {:type "submit"}
    "Submit"]])

(defn main []
  [:div
   [canvas/draw-world @session/state]
   [:button.btn.btn-default
    {:on-click (fn [e]
                 (session/update-in! [:ticking] not))}
    (if (session/get :ticking)
      "Stop"
      "Start")]])

(defn ui []
  [:div
   [:h1 "Welcome to Turtles"]
   [main]])
