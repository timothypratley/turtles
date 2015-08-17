(ns turtles.app
  (:require [reagent.core :as reagent]
            [turtles.ticker :as ticker]
            [turtles.ui :as ui]))

(enable-console-print!)

(defn root []
  [:div.container
   [ui/ui]])

(defn reload []
  (reagent/render-component root (.getElementById js/document "turtles")))

(defn init []
  (ticker/start)
  (reload))
