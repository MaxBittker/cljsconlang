(ns conlang.vector
  [:require
   [conlang.constants
     :refer [size
             half
             step
             tile-size
             grid-width
             lscale]]])

(defn add [& vs]
  (vec (apply map + vs)))

(defn subtract [& vs]
  (vec (apply map - vs)))

(defn magnitude [v]
  (Math/sqrt (reduce + (map #(Math/pow % 2) v))))

(defn normalize [v]
  (let [m (magnitude v)]
    (vec (map #(/ % m) v))))

(defn multiply [v scalar]
  (vec (map * (repeat scalar) v)))

(defn distance [a b]
  (magnitude (subtract a b)))

(defn to-polar [p]
  (let [[x y] (subtract p [half half])]
    [(Math/atan2 y x)
     (magnitude [x y])]))

(defn to-cartesian [[a r]]
  (let [p (multiply [(Math/cos a) (Math/sin a)]
            r)]
    (add p [half half])))

(defn random-2d []
 (normalize [(- (rand 2) 1.0) (- (rand 2) 1.0)]))
