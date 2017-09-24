(ns conlang.constants)


(def size 250)

(def half (/ size 2))

(def step 20)

(def tile-size (int (* step 3)))

(def grid-width (inc (int (/ size tile-size))))

(def lscale 2)
