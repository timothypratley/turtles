(ns turtles.ticker
  (:require [turtles.session :as session]))

(declare spiral-step)
(declare linear-spiral-step)

(defn square-spiral-step
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
           :x (+ x (* 5 velocity (Math/cos angle)))
           :y (+ y (* 5 velocity (Math/sin angle)))
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
           :x (+ x (* velocity (Math/cos angle)))
           :y (+ y (* velocity (Math/sin angle)))
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
           :x (+ x (* velocity (Math/cos angle)))
           :y (+ y (* velocity (Math/sin angle)))
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
                        square-spiral-step
                        next-step))))

(defn rand-step
  [{:as turtle
    :keys [x y angle spin steps velocity acceleration]
    :or {velocity 10
         spin 0.2
         acceleration 0.5}}
   {:keys [turtles]}]
  (let [angle (+ angle spin)]
    (assoc turtle
           :x (+ x (* velocity (Math/cos angle)))
           :y (+ y (* velocity (Math/sin angle)))
           :angle angle
           :spin (if (< (rand) 0.02)
                   (- spin)
                   spin))))

(defn bound [x]
  (min 500 (max -500 x)))

(defn bounded [{:as turtle :keys [x y]}]
  (assoc turtle
         :x (bound x)
         :y (bound y)))

(defn cons-window [xs x]
  (take 200 (cons x xs)))

(defn remember [{:as turtle :keys [next-step]}]
  (update-in (bounded (next-step turtle)) [:steps]
             cons-window (select-keys turtle [:x :y])))

(defn intersect [[{x1 :x y1 :y} {x2 :x y2 :y}]
                 [{x3 :x y3 :y} {x4 :x y4 :y}]]
  (let [a-min-x (dec (min x1 x2))
        a-max-x (inc (max x1 x2))
        a-min-y (dec (min y1 y2))
        a-max-y (inc (max y1 y2))
        b-min-x (dec (min x3 x4))
        b-max-x (inc (max x3 x4))
        b-min-y (dec (min y3 y4))
        b-max-y (inc (max y3 y4))]
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
        new-turtle (remember turtle)]
    (assoc-in world [:turtles tname]
              (if (ok? tname new-turtle world)
                new-turtle
                (assoc new-turtle
                       :x (:x turtle)
                       :y (:y turtle))))))

(def script
  [""
   "Once upon a time,"
   "in the mystical Land of Lambda,"
   "lived four turtles"
   "who loved spirals."
   "Leonardo drew spirals with smooth ratios."
   "Michelangelo preferred tighter, more detailed spirals."
   "Raphael drew bold, strong lines,"
   "and Donatello preferred simple round shapes."
   ""
   "They all lived by a strict code of conduct;"
   "1. Take one step at a time."
   "2. Never cross another turtle's trail."
   "3. Stay within the Land of Lambda."
   ""
   "Donatello stuck to circular curves."
   "Leonardo, Michaelangelo and Raphael alternated"
   "between linear, exponential, and square spiralling."
   ""
   ""
   "Each turtle's step was taken"
   "as a function of their present;"
   "(step {:x 5, :y 10, :angle 0, :velocity 1})"
   "=> {:x 6, :y 10, :angle 0, :velocity 1}"
   "Each step produced a future value"
   "similar to their present value,"
   "but at a new location."
   ""
   "The successful application of a turtle's step"
   "was possible only by will of The Great Transactor,"
   "who permitted only steps within the code of conduct."
   ""
   "And so the turtles happily drew their spirals,"
   "with somewhat loose coordination,"
   "and rather strict capabilities,"
   "to form complex patterns."
   ""
   ""
   ""
   "On and on they continued..."
   "tirelessly spiralling,"
   "spiralling,"
   "spiralling."
   ""
   ""
   ""
   "Forever."
   ""
   ""
   ""
   "Well, perhaps more truthfully:"
   "for as long as you wish to watch them."])

(defn tick! []
  (doseq [tname (shuffle (keys (session/get :turtles)))]
    (session/swap! transact tname))
  (session/update-in! [:tick] inc)
  (if-let [text (get script (quot (session/get :tick) 30))]
    (session/put! :text text)
    (session/remove! :text)))

(defn ticker-loop []
  (when (session/get :ticking)
    (tick!)))

(defonce a-ticker
  (js/setInterval ticker-loop 100))

(defn start []
  (session/put!
   :tick 0
   :ticking true
   :turtles {"Leonardo" {:x -200
                         :y -200
                         :pen "#A1B56C"
                         :spin 0.201
                         :velocity 1
                         :acceleration 1.05
                         :next-step spiral-step}
             "Michelangelo" {:x -200
                             :y 200
                             :pen "#F7CA88"
                             :spin 0.22
                             :velocity 1
                             :acceleration 1.01
                             :next-step linear-spiral-step}
             "Raphael" {:x 200
                        :y -200
                        :pen "#86C1B9"
                        :spin 0.25
                        :acceleration 1.02
                        :next-step square-spiral-step}
             "Donatello" {:x 200
                          :y 200
                          :spin 0.1
                          :pen "#7CAFC2"
                          :next-step rand-step}}))
