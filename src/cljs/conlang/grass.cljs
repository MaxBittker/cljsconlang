(ns conlang.grass
    [:require
      [reagent.core :as reagent :refer [atom]]
      [clojure.string :as str]
      [conlang.point-utils :as pt]
      [conlang.vector :as v]
      [conlang.constants
       :refer [size
               half]]])


(defn stray [n d]
    (let [s (repeatedly (/ n 2) #(* d (rand)))]
        (shuffle (concat s (map #(* % -2) s)))))

(def margin 25)
(def rowH (/ (- size margin margin) 10))

(defn grass-blade [i s]
    (reductions 
        (fn [[x y] [xb yb]] [(+ x xb) (inc y)]) 
        [s 0]
        (map (fn [xs] [xs 0])
         (stray (- rowH 1.0) (if (zero? (mod i 2)) -1 1)))))
    ; (take-while (fn [[x y]] (> y size))
    ; (take (inc rowH)
    ;  (iterate 
    ;     (fn [[x y]]  [(+ x (v/nrand)) (inc y)]) 
    ;    [s 0])))    

(defn grass-vis []
  (apply concat  
    (map-indexed
        (fn [i y]
            (pt/point-map (fn [p] (v/add p [0 y]))
             (map #(grass-blade i %) (range margin (- size margin) 1))))
     (range margin (- size margin) rowH))))


