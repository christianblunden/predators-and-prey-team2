(ns predators-and-prey.simulation
	(:use predators-and-prey.constants)
	(:use predators-and-prey.collisions)
        (:use predators-and-prey.vectors)
(:use [clojure.contrib.pprint :only [pprint]])
        )

(def predator {:max-velocity 7 :radius 20})
(def prey {:max-velocity 4 :radius 10})

(def animals (atom {}))

(defn create-predator [x y horizontal-velocity vertical-velocity]
	(conj {:x x :y y :vx horizontal-velocity :vy vertical-velocity} predator))
	
(defn create-prey
	([screen-size]
	(conj {:x (rand-int screen-size) :y (rand-int screen-size) :vx (rand-int (:max-velocity prey)) :vy (rand-int (:max-velocity prey))} prey))
	([x y vx vy]
	(conj {:x x :y y :vx vx :vy vy} prey)))
	
(defn prey-generator [screen-size]
	#(conj {:x (rand-int screen-size) :y (rand-int screen-size) :vx (rand-int (:max-velocity prey)) :vy (rand-int (:max-velocity prey))} prey))
	
(defn initial-state []
	{:predators [(create-predator 50 50 4 4)
	(create-predator (- screen-size 100) (- screen-size 100) -4 -4)]
	:prey (take 50 (repeatedly (prey-generator screen-size)))})

(defn move [animal]
	(let [x (:x animal) y (:y animal)
	vx (:vx animal) vy (:vy animal)]
	(assoc animal :x (mod (+ x vx) screen-size) :y (mod (+ y vy) screen-size))))

(defn surviving? [predators]
	(fn [prey]
		(if (nil? (some #(collides? prey %) predators)) true false)))


(def flee -)
(def target +)

(defn move-towards [point animal strategy]
  (let [difference (sub point [(:x animal) (:y animal)])
        unit-vec (unit difference)
        [vx vy] (mul (strategy (:max-velocity animal)) 
                     unit-vec)]
    (assoc animal :vx vx :vy vy )))

(defn animal-to-vec [{:keys [x y]}]
  [x y])

(defn distance-between [animal1 animal2]
  (len (sub (animal-to-vec animal1) (animal-to-vec animal2))))

(defn direction [strategy opponents animal state]
  (let [distance-to-target #(distance-between animal %)
        sorted #(sort-by distance-to-target %)
        desired-location (-> state opponents sorted first animal-to-vec)]
    (move-towards desired-location animal strategy)))

(defn think [current-state]
  (let [new-predators (map #(direction target :prey % current-state) (:predators current-state))
	remaining-prey (filter (surviving? new-predators) (:prey current-state))
        remaining-prey (map #(direction flee :predators % current-state) remaining-prey)
        remaining-prey (map move remaining-prey)
        new-predators (map move new-predators)]
    (-> current-state
        (assoc :predators new-predators :prey remaining-prey))))

(defn pulse []
	(let [bounded-screen-size (- screen-size 20)]
	(if (empty? @animals)
		(reset! animals (initial-state))
		(swap! animals think))))
