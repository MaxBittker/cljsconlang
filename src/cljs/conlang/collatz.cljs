(ns conlang.collatz
  [:require
    [clojure.string :as str]
    [conlang.point-utils :as pt]
    [conlang.vector :as v]
    
    [conlang.constants
     :refer [size
             half]]])

(defn collatz [n]
    (if (even? n)
     (/ n 2)
     (inc (* 3 n))))

(defn collatz-seq [n]
  (->>
    (iterate collatz n)
    (take-while (fn [v] (> v 1))) 
    reverse
    (map-indexed vector)))

(def maxx 2000)

(defn modify [[i v]]
    (v/to-cartesian
      [(* i 0.036)
       (* (/ (Math/log v) (Math/log 5))
         14)]))
(defn collatz-vis []
  (let [raw (map collatz-seq (range 500 maxx))]
    (pt/point-map modify raw)))
