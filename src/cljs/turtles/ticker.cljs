(ns turtles.ticker
  (:require [turtles.session :as session]))

(declare spiral-step)
(declare linear-spiral-step)

(defn bound [x]
  (min 500 (max -500 x)))

(defn spiral-square-step
  [{:as turtle
    :keys [x y angle velocity acceleration spin next-step]
    :or {angle 0
         velocity 10}}
   world]
  (let [switch? (or (and (> velocity 50)
                         (> acceleration 1))
                    (and (< velocity 1)
                         (< acceleration 1)))]
    (assoc turtle
           :x (bound (+ x (* 5 velocity (Math/cos angle))))
           :y (bound (+ y (* 5 velocity (Math/sin angle))))
           :acceleration (if switch?
                           (/ 1 acceleration)
                           acceleration)
           :velocity (* velocity acceleration)
           :angle (+ angle (* spin 5 (/ Math/PI 2)))
           :spin (if switch?
                   (- spin)
                   spin)
           :next-step (if (and switch? (< velocity 1))
                        spiral-step
                        next-step))))

(defn spiral-step
  [{:as turtle
    :keys [x y angle spin velocity acceleration next-step]
    :or {angle 0
         spin 0.2
         velocity 10
         acceleration 1.1}}
   world]
  (let [switch? (or (and (> velocity 50)
                         (> acceleration 1))
                    (and (< velocity 1)
                         (< acceleration 1)))]
    (assoc turtle
           :x (bound (+ x (* velocity (Math/cos angle))))
           :y (bound (+ y (* velocity (Math/sin angle))))
           :angle (+ angle spin)
           :spin (if switch?
                   (- spin)
                   spin)
           :acceleration (if switch?
                           (/ 1 acceleration)
                           acceleration)
           :velocity (* acceleration velocity)
           :next-step (if (and switch? (< velocity 1))
                        linear-spiral-step
                        next-step))))

(defn linear-spiral-step
  [{:as turtle
    :keys [x y angle spin steps velocity acceleration next-step]
    :or {velocity 1
         spin 0.2
         acceleration 0.5}}
   {:keys [turtles]}]
  (let [switch? (or (and (> velocity 50)
                         (> acceleration 1))
                    (and (< velocity 1)
                         (< acceleration 1)))]
    (assoc turtle
           :x (bound (+ x (* velocity (Math/cos angle))))
           :y (bound (+ y (* velocity (Math/sin angle))))
           :angle (+ angle spin)
           :velocity (+ velocity (if (> acceleration 1)
                                   0.2
                                   -0.2))
           :spin (if switch?
                   (- spin)
                   spin)
           :acceleration (if switch?
                           (/ 1 acceleration)
                           acceleration)
           :next-step (if (and switch? (< velocity 1))
                        spiral-square-step
                        next-step))))

(defn normalize-angle [angle]
  (let [new-angle (atom angle)]
    (while (<= @new-angle (- Math/PI))
      (swap! new-angle + (* 2 Math/PI)))
    (while (> @new-angle Math/PI)
      (swap! new-angle - (* 2 Math/PI)))
    @new-angle))

(defn distance [{x1 :x y1 :y} {x2 :x y2 :y}]
  (let [a (- x2 x1)
        b (- y2 y1)]
    (Math/sqrt (+ (* a a) (* b b)))))

(defn follow-step
  [{:as turtle
    :keys [x y angle spin steps velocity acceleration]
    :or {velocity 10
         spin 0.2
         acceleration 0.5}}
   {:keys [turtles]}]
  (let [{tx :x ty :y} (last (sort-by #(distance turtle %) (vals turtles)))
        dx (- tx x)
        dy (- ty y)
        tangle (Math/atan2 dy dx)
        da (- tangle angle)
        angle (if (pos? (normalize-angle da))
                (+ angle spin)
                (- angle spin))]
    (assoc turtle
           :x (bound (+ x (* velocity (Math/cos angle))))
           :y (bound (+ y (* velocity (Math/sin angle))))
           :angle angle)))

(defn cons-window [xs x]
  (take 200 (cons x xs)))

(defn remember [{:as turtle :keys [next-step]} world]
  (update-in (next-step turtle world) [:steps]
             cons-window (select-keys turtle [:x :y])))

(defn intersect [[{x1 :x y1 :y} {x2 :x y2 :y}]
                 [{x3 :x y3 :y} {x4 :x y4 :y}]]
  (let [a-min-x (min x1 x2)
        a-max-x (max x1 x2)
        a-min-y (min y1 y2)
        a-max-y (max y1 y2)
        b-min-x (min x3 x4)
        b-max-x (max x3 x4)
        b-min-y (min y3 y4)
        b-max-y (max y3 y4)]
    (not (or (< a-max-x b-min-x)
             (> a-min-x b-max-x)
             (< a-max-y b-min-y)
             (> a-min-y b-max-y)))))

(defn ok? [tname turtle world]
  (let [from (first (:steps turtle))
        line [from turtle]]
    (or (nil? from)
        (not-any? #(intersect line %)
                  (for [[oname other] (:turtles world)
                        :when (not= oname tname)
                        :let [other-steps (:steps other)]
                        :when (seq other-steps)
                        line (map vector (cons other other-steps) other-steps)]
                    line)))))

(defn transact [world tname]
  (let [turtle (get-in world [:turtles tname])
        new-turtle (remember turtle world)]
    (assoc-in world [:turtles tname]
              (if (ok? tname new-turtle world)
                new-turtle
                (assoc new-turtle
                       :x (:x turtle)
                       :y (:y turtle))))))

(def script
  {1  "Once upon a time,"
   2  "Four master artists collaborated on a painting."
   7  "They all had slightly different styles"
   3  "Leonardo was obsessed with ratios and spirals"
   4  "Michelangelo was detail oriented"
   5  "Raphael was bold"
   6  "Donatello liked to get near the furthest"
   8  "All agreed to stop before crossing another's line"})

(defn tick! []
  ;; TODO: single update
  (session/update-in! [:tick] inc)
  (when-let [text (script (quot (session/get :tick) 10))]
    (session/put! :text text))
  (doseq [tname (shuffle (keys (session/get :turtles)))]
    (session/swap! transact tname)))

(defn ticker-loop []
  (when (session/get :ticking)
    (tick!)))

;; global state, coordinated turtles
(defn start []
  (session/put!
   :tick 0
   :turtles {"Leonardo" {:x -200
                         :y -200
                         :pen "green"
                         :spin 0.201
                         :velocity 1
                         :acceleration 1.05
                         :next-step spiral-step}
             "Michelangelo" {:x -200
                             :y 200
                             :pen "darkgreen"
                             :spin 0.22
                             :velocity 1
                             :acceleration 1.01
                             :next-step linear-spiral-step}
             "Raphael" {:x 200
                        :y -200
                        :pen "lightgreen"
                        :spin 0.25
                        :acceleration 1.02
                        :next-step spiral-square-step}
             "Donatello" {:x 200
                          :y 200
                          :spin 0.1
                          :pen "darkolivegreen"
                          :next-step follow-step}})
  (session/put! :ticking true)
  (js/setInterval ticker-loop 100))
