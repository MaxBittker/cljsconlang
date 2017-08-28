(ns conlang.spatial-grid
  (:require [conlang.constants
                :refer [size
                          half
                          step
                          tile-size
                          grid-width
                          lscale]]
            [conlang.vector
              :refer [add
                      multiply
                      random-2d
                      distance
                      normalize
                      to-polar
                      to-cartesian]]
            [conlang.point-utils
              :refer [
                      point-map]]))


(enable-console-print!)

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
    grid)))
    ; (throw (js/Error. "Out of bounds!")
    ;  (println "out of bound"))))

(defn grid-insert-line [grid points s]
  (reduce (fn [g p] (grid-insert g p s))
    grid
    points))

(defn neighbors-of [p s]
   (for [dx [-1 0 1] dy [-1 0 1]]
     (vec (map + [dx dy] (grid-loc p s)))))

(defn get-buckets [grid p s]
 (mapcat (fn [np] (get-in grid np))
  (neighbors-of p s)))

(defn view-grid [grid s]
  (map-indexed
    (fn [x row]
      (map-indexed
        (fn [y val]
         (neighbors-of [x y] s))

        ; x))
       row))
   grid))

