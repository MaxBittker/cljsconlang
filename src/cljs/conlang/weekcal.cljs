(ns conlang.week-cal
    [:require
      [reagent.core :as reagent :refer [atom]]
      [clojure.string :as str]
      [conlang.point-utils :as pt]
      [conlang.font-data :refer [fonts-data]]
      [conlang.vector :as v]
      [conlang.point-utils
       :refer [format-points
               normalize-points
               normalize-line
               normalize-lines
               d-to-points
               prune-line
               translate-points
               translate-line
               close-loop
               close-loops]]
      [conlang.constants
       :refer [size
               half]]])


(defn xor   
    "Exclusive or"
    ([a b]
     (or (and a (not b)) (and (not a) b))))
    
(def data 
  [
    ["January"  31 1]
    ["February"  28 4]
    ["March"  31 4]
    ["April"  30 0]
    ["May"  31 2]
    ["June"  30 5]
    ["July"  31 0]
    ["August"  31 3]
    ["September"  30 6]
    ["October"  31 1]
    ["November"  30 4]
    ["December"  31 6]])

(def sp "    ")

(defn format-week [days]   
    (map 
      (fn [d]
        (if (> d 0)
            (str d)
            ""))
     days))

(defn format-days [days wo name]
  (let [weeks (partition 7 7 [-1] 
                    (range (- (mod (+ -2 wo) 7)) (inc days)))]
    (map-indexed
     (fn [i wdays] 
         {:days (format-week wdays)
          :name (if (zero? i)
                  name
                  "")})

     (filter (fn [wdays] 
                (not (empty? (filter #(< 0 %) wdays))))
       weeks))))

(defn print-month [[name days wo]]
    (format-days days wo name))      

(def weeks 
    (apply concat (map print-month data)))
    

(defn str-to-data [input]
     (map #(nth (:chars (:timesrb fonts-data))
                (- (.charCodeAt %) 33)
                {:d "0,0" :o 4})
         input))
    

(defn str-to-points [input s [px py]]
    (let [dseq (str-to-data input)
          oseq (reductions + (map :o dseq))]
        (mapcat
           (fn [[{d :d o :o} oS]]
               (normalize-lines
                (translate-points
                    (d-to-points d)
                    s
                    [(+ (* s oS 1.8) px)
                     (+ 0 py)])))
           (map vector dseq oseq))))
       
(defn render-name [name]
    (str-to-points (take 3 name) 0.2 [-3 0]))

(defn render-week [i {days :days name :name}]
 (let [f (* Math/PI 2 (/ 1 7))
        r 12
       week-shape (map-indexed 
                        (fn [i n]
                            (str-to-points n 0.2 
                                [(* (Math/cos (* (- i 1.78) f))  r)
                                 (* (Math/sin (* (- i 1.78) f))  r)]))
                        days)
        week-circle (conj week-shape (render-name name))]
    (translate-points (apply concat week-circle)
     (v/add [20 40] 
        (v/multiply 
            [(mod i 7)
             (* (- i (mod i 7)) 0.13)] 
            (* 2.9 r))))))
        
; (println weeks)

(defn week-cal-vis []
  (apply concat 
    (map-indexed
        (fn [i week]
            (render-week i week))
        weeks)))

