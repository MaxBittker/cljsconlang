(ns conlang.spatial-grid
  (:require [conlang.constants :refer [size
                                            half
                                            step
                                            tile-size
                                            grid-width
                                            lscale]]))


(defn new-grid [n]
  (vec (take n (repeat (vec (take n (repeat '())))))))

(defn grid-loc [[x y] s]
 [(int (/ x s))
  (int (/ y s))])

(defn grid-insert [grid p s]
 (let [loc (grid-loc p s)]
   (if (get-in grid loc)
     (assoc-in grid loc
       (cons p (get-in grid loc)))
     (throw (js/Error. "Out of bounds!")))))

(defn grid-insert-many [grid points s]
  (reduce (fn [g p] (grid-insert g p s))
    grid
    points))

(defn neighbors-of [p s]
   (for [dx [-1 0 1] dy [-1 0 1]]
     (vec (map + [dx dy] (grid-loc p s)))))

(defn get-buckets [grid p s]
 (mapcat (fn [np] (get-in grid np))
  (neighbors-of p s)))
