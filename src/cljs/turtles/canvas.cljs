(ns turtles.canvas
  (:require [clojure.string :as string]))

(defn draw-turtle [tname {:keys [x y steps pen]}]
  [:g
   [:path
    {:fill "transparent"
     :stroke pen
     :stroke-width 2
     :stroke-linecap "round"
     :stroke-linejoin "round"
     :d (string/join " " (cons (str "M" x "," y)
                               (for [{:keys [x y]} steps]
                                 (str "L" x "," y))))}]
   [:text
    {:x x
     :y y
     :stroke "#A16946"}
    tname]])

(defn draw-world [{:keys [tick text turtles]}]
  [:div
   {:style {:height "70vh"}}
   [:svg
    {:style {:border "1px solid #D8D8D8"
             :width "100%"
             :height "100%"}
     :view-box (string/join " " [0 0 1000 1000])}
    (into [:g {:transform (str "translate(" 500 "," 500 ")")}]
          (for [[tname turtle] turtles]
            (draw-turtle tname turtle)))
    [:text
     {:x 500 :y 500
      :stroke "#DC9656"
      :text-anchor "middle"
      :opacity (double (/ 30 (mod tick 30)))
      :style {:font-family "Cursive"
              :font-size 42}}
     text]]])
