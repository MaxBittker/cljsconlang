(ns conlang.projections
    [:require
      [reagent.core :as reagent :refer [atom]]
      [clojure.string :as str]
      [conlang.point-utils :as pt]
      [conlang.vector :as v]
      [conlang.font-data :refer [fonts-data]]
      [conlang.face-data :refer [face-data]]
      [conlang.skull-data :refer [skull-data]]
      [conlang.eye-data :refer [eye-data]]
      [conlang.constants
       :refer [size
               half]]])


(defn project [p o d]
  (let [n  (v/normalize (v/subtract p o))
        sn (v/multiply n d)
        projected (v/add p sn)]
    projected))
     

(def letter-points
;  (apply concat  
  (pt/normalize-lines
    (pt/translate-points
      (->> fonts-data
        (:gothiceng)
        (:chars)
        (drop 33)
        (take 26) 
        (rand-nth)
        ; (#(nth % 3))
        (:d) 
        (pt/d-to-points))
     4
     [75
      75])))

(defn str-to-data [input]
    (map #(nth (:chars (:gothiceng fonts-data))
               (- (.charCodeAt %) 33)
               {:d "0,0" :o 4})
        input))
    

(defn str-to-points [input s]
    (let [dseq (str-to-data input)
          oseq (reductions + (map :o dseq))]
     (mapcat
      (fn [[{d :d o :o} oS]]
        (pt/normalize-lines
         (pt/translate-points
            (pt/d-to-points d)
            s
            [(* s 1.6 (- (+ 3 oS) o)) 
             (/ size 4.0)])))
      (map vector dseq oseq))))
       
(def word-points
    (str-to-points "nice" 3))
    
(defn sketch-to-points [data]
    (map
        (fn [[xs ys ts]]
            (map vector xs ys))
     (:drawing data)))
                
(defn face-points []
    (pt/translate-points
        (sketch-to-points (rand-nth skull-data))
        0.8
        [10 10]))

(def square-points 
    (let [a 150
          b 75]
     (pt/normalize-lines
        [[[a a][a b][b b][b a][a a]]])))
     
        
(defn projections [o d]
    (pt/point-map (fn [p] (project p o d))
    ;  square-points
    ;  word-points)) 
        (face-points)))
    ;    letter-points)) 

(def skulls (reagent/atom (face-points)))
    
(defn update-proj []
    (swap! skulls concat (face-points)))


(defn projections-vis [tickp]
    (let [
            tick 0        
            t (* (inc (Math/sin (/ tick 15))) 15)
            ;   t (+ 2 (mod tick 20))
            x (* half 0.22 (Math/sin (/ tick 15)))
            y (* half 0.25  (Math/cos (/ tick 15)))]
    ;  (projections (v/add [x y] [100 130]) t)))
        (reverse @skulls)))
            ; (map  face-points
                ; (range -10 100000 2.5))))
    
        ; (point-list-to-paths letter-points))))


