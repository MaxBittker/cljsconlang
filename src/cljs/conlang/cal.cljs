(ns conlang.cal
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

(defn format-week [days isfirst]   
    (str 
     (str/join sp days) 
     (if (or isfirst (= 7 (count days))) 
        (str "." "   ")
        sp)))

(defn format-days [days wo]
  (let [weeks (partition 7 7 [-1] 
                    (range (- (mod (+ -2 wo) 7)) (inc days)))]
    (map-indexed
     (fn [i wdays] 
        (format-week wdays (zero? i)))
     (filter (complement empty?) 
      (map (fn [wdays] (filter #(< 0 %) wdays))
       weeks)))))

(defn print-month [[name days wo]]
    (str 
        (apply str (take 3 name))
        sp
        (str/join  ""
          (format-days days wo))      
        ""))

(def cal-text 
    (apply str (map print-month data)))
    

(defn str-to-data [input]
     (map #(nth (:chars (:timesrb fonts-data))
                (- (.charCodeAt %) 33)
                {:d "0,0" :o 4})
         input))
    
(def hm (* size 1.8))
(def hhm (* 0.5 hm))

(def sl 0.0001)

(defn str-to-points [input s]
    (let [dseq (str-to-data input)
          oseq (map :o dseq)
          vhoseq (reductions  
                    (fn [[v h] o]
                        (if (and (> h hm) (= o  4)) 
                            [(- v (* hm sl -13))  (* 0.0 (- h hm))]
                            [(+ v (if (> h hhm)
                                    (* sl (- h hhm))
                                    (* -1 sl (- hhm h))))
                             (if (and (= o 4) (= h 0))
                              0 (+ h o))]))
                    [1 0] oseq)]
        (mapcat
           (fn [[{d :d o :o} [voS hoS]]]
               (normalize-lines
                (translate-points
                    (d-to-points d)
                    s
                    [(* s hoS 1.7)
                     (* s voS 80)])))
           (map vector dseq vhoseq))))
       
(defn cal-vis []
    (str-to-points cal-text 0.21))


