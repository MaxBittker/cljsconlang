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

(defn format-points [points]
  (string/join " " (map #(string/join "," %) points)))

(defn normalize-points [[a b]]
  (if (< 1 (count (range 0 (distance a b) step)))
    (concat
      (map
        (fn [d] (add a
                  (multiply
                    (normalize (subtract b a))
                    d)))
        (range 0 (distance a b) (* 0.4 step)))
      [b]);end can get lost because of how step works
    (list a b)))

(defn normalize-line [points]
  (mapcat
   normalize-points
   (partition 2 1 points)))

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

(defn translate-points [pl s o]
  (map
    (fn [line-points]
     (map
      (fn [p] (add (multiply p s) o))
      line-points))
   pl))

(defn close-loop [pl]
  (conj (vec pl) (first pl)))

(defn close-loops [ll]
  (map
    close-loop)
  ll)
