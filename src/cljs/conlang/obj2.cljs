(ns conlang.obj2
  [:require
    [clojure.string :as str]
    [conlang.constants
     :refer [size
             half]]])

(defn sorted-set-to-vstring [s]
  (str/join "\n"
    (map
      (fn [[x y]] (str "v " x " " y))
     s)))

(defn point-to-index [pmap point]
  (get pmap point))

(defn line-sec [pmap line]
  (let [indexes (map
                  (fn [point] (get pmap point))
                  line)]
    (str "e " (first indexes) "\n"
         "l " (str/join " " indexes))))


(defn lines-to-2obj [lines]
  (let [points (apply sorted-set (apply concat lines))
        vsec (sorted-set-to-vstring points)
        pmap (zipmap points (iterate inc 1))
        lsecs (str/join "\n" (map #(line-sec pmap %) lines))]
    (str vsec "\n" lsecs)))
