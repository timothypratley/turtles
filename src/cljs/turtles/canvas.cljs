(ns turtles.canvas
  (:require [clojure.string :as string]))

(defn draw-turtle [tname {:keys [x y steps pen]}]
  [:g
   [:path {:fill "transparent"
           :stroke pen
           :stroke-width 2
           :d (string/join " "
                           (cons (str "M" x "," y)
                                 (for [{:keys [x y]} steps]
                                   (str "L" x "," y))))}]
   [:text {:x x
           :y y}
    tname]])

(defn draw-world [{:keys [text turtles]}]
  [:div {:style {:height "70vh"}}
   [:svg {:style {:border "1px solid black"
                  :width "100%"
                  :height "100%"}
          :view-box (string/join " " [0 0 1000 1000])}
    (into [:g {:transform (str "translate(" 500 "," 500 ")")}]
          (for [[tname turtle] turtles]
            (draw-turtle tname turtle)))
    [:text {:x 40 :y 40}
     text]]])
