(ns conlang.point-utils
  [:require
    [clojure.string :as string]
    [conlang.vector
      :refer [add
               subtract
               magnitude
               normalize
               multiply
               distance
               random-2d]]
   [conlang.constants
     :refer [size
             half
             step
             tile-size
             grid-width
             lscale]]])

(defn point? [p]
  (and
    (= 2 (count p))
    (every? number? p)))

(defn point-map [f s]
  (if (point? (first s))
    (map f s)
    (map (partial point-map f) s)))

(defn format-points [points]
   (string/join " " (map #(string/join "," %) points)))

(defn prune-line
 ([points] (prune-line points 1))
 ([points d]
  (map
    (fn [[a b c]] b)
    (filter
      (fn [[a b c]]
        (< d (distance a c)))
      (partition 3 1
        (concat [(last points)] points [(first points)]))))))

(defn normalize-points [[a b]]
    (if (< 3 (count (range 0 (distance a b) step)))
      (concat
        (map
          (fn [d] (add a
                    (multiply
                      (normalize (subtract b a))
                      d)))
          (range 0 (distance a b) (* 0.4 step)))
        [b]);end can get lost because of how step works
      (if (> 2 (distance a b))
        (list b)
        (list a b))))

(defn normalize-line [points]
  (concat
   [(first points)]
   (mapcat
    normalize-points
    (partition 2 1 points))
   [(last points)]))

(defn normalize-lines [lines]
  (map
    normalize-line
    lines))

(defn d-to-points [data-string]
 (map
   #(partition 2
        (map int (string/split % #"[\, ]")))
   (filter (complement empty?)
     (string/split
       (string/replace
         data-string
         #"L" "")
      #"M"))))

(defn translate-line
 ([line o] (translate-line line 1 o))
 ([line s o]
  (map
   (fn [p] (add (multiply p s) o))
   line)))


(defn translate-points
 ([pl o] (translate-points pl 1 o))
 ([pl s o]
  (map
    (fn [line-points]
      (translate-line line-points s o))
   pl)))

(defn close-loop [pl]
  (conj (vec pl) (first pl)))

(defn close-loops [ll]
  (map
    close-loop)
  ll)
