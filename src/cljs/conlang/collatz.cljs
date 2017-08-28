(ns conlang.collatz
  [:require
    [clojure.string :as str]
    [conlang.point-utils :as pt]
    [conlang.vector :as v]
    
    [conlang.constants
     :refer [size
             half]]])

(def collatz-values (atom (sorted-map 1 0)))

(defn collatz [n]
    (if (even? n)
     (/ n 2)
     (inc (* 3 n))))

(defn collatz-stump [vs n]
    (take-while 
        (fn [cv] (nil? (vs cv))) 
      (iterate collatz n)))

(defn translated-stump [vs n]
    (let [stump (collatz-stump vs n)]
        (if (empty? stump)
            stump
            (let [o (inc (vs (collatz (last stump))))]
                (map-indexed (fn [i v] [v (+ i o)]) (reverse stump))))))
        
(defn collatz-insert [vs n]
    (if (= n 1)
        vs
        (let [nv (collatz n)
              parent-depth (vs nv)]
            (into vs (translated-stump vs n)))))
            
(defn update-map [n]
    (swap! collatz-values collatz-insert n)     
  [n (@collatz-values n)])           


(defn collatz-seq [n]
  (->>
    (iterate collatz n)
    (take-while (fn [v] (> v 1))) 
    reverse
    (map-indexed vector)))

    ; (print (collatz-seq 13))
(def maxx 2000)

(defn modify [[i v]]
    (v/to-cartesian
      [(* i 0.036)
       (* (/ (Math/log v) (Math/log 5))
         14)]))
(defn collatz-vis []
  (let [raw (map collatz-seq (range 500 maxx))]
    (pt/point-map modify raw)))
