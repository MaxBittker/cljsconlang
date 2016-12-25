
(ns conlang.core
    (:require [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [secretary.core :as secretary :include-macros true]
              [accountant.core :as accountant]))

;; -------------------------
;; Views

(def input-word (reagent/atom "test"))

(def size 200)
(def half (/ size 2))

(defn counting-component []
  [:div
   [:input {:type "text" :value @input-word
            :on-change #(reset! input-word (-> % .-target .-value))}]])

(defn format-points [points]
  (clojure.string/join " " (map #(clojure.string/join "," %) points)))

(defn random-points []
  (take 2
   (repeatedly
     (fn []
       [(dec (*  (Math/random)(Math/random) half))
        (dec (*  (Math/random)(Math/random) half))]))))

(defn random-movers []
  (take 3
   (repeatedly
     (fn []
       [[
          (dec (*  (Math/random)(Math/random) half))
          (dec (*  (Math/random)(Math/random) half))]

        [
          (dec (*  (Math/random) 2))
          (dec (*  (Math/random) 2))]]))))



; (def pts (reagent/atom (random-points)))
(def mvrs (reagent/atom (random-movers)))

(defn walk [mvrs]
  (map (fn [[x y] [vx vy]]
          [[(+ x (* 10 vx))]
           (+ y (* 10 vy))
           [vx
            vy]])
       mvrs))

(defn vib [points]
  (map (fn [[x y]]
        [(+ x (* 4 (- 0.5 (Math/random))))
         (+ y (* 4 (- 0.5 (Math/random))))])
       points))

(defn warp [points]
 (map (fn [[x y]]
       [(mod (+ (* x 0.999) half) half)
        (mod (+ (* y 0.999) half) half)])
      points))

(defn spin [points]
  (map (fn [[x y]]
        [(+ (* x 0.999) (* y 0.001))
         (+ (* y 0.999) (* x 0.001))])
      points))

(defn updater []
  (swap! @mvrs (comp walk))
  (. js/window (requestAnimationFrame updater)))

; (updater)

; (swap! @mvrs (comp walk))

(defn star [formated n]
    (map
      (fn [r]
        [:g {:key r :transform (str "rotate(" r ", 0, 0)")}
         [:polygon {:points formated}]])
      (range 0 360 (/ 360 n))))

(defn collection [points n]
  (let [formated (format-points points)]
    (map
      (fn [r]
        [:svg {:key r :width size :height size}
          [:g {:transform (str "translate(" half "," half")")}
            (star @mvrs r)]])
      (range 2 11))))

(defn star-page []
   [:div {:class "display"}
      (collection @mvrs 4)])

(defn add [& vs]
  (vec (apply map + vs)))

(defn subtract [& vs]
  (vec (apply map - vs)))

(defn magnitude [v]
  (Math/sqrt (reduce + (map #(Math/pow % 2) v))))

(defn normalize [v]
  (let [m (magnitude v)]
    (vec (map #(/ % m) v))))

(defn multiply [v scalar]
  (vec (map * (repeat scalar) v)))

(defn distance [a b]
  (magnitude (subtract a b)))

(defn random-2d []
 (normalize [(- (rand 2) 1.0) (- (rand 2) 1.0)]))

(def step 3)

(defn new-point [points]
  ((fn [v]
    (add (multiply (random-2d) step) v))
   (first points)))

(defn check-point [p points] ;can be improved with skipping
  (or
   (some (fn [op] (< (distance op p) step)) points)
   (> (distance p [half half]) (/ size 2))
   (< (distance p [half half]) (/ size 6))))

(defn next-checked-point [points history]
  (let [np (new-point points)]
    (if (check-point np (concat points history))
      points
      (cons np points))))

(defn random-walk [c history]
    (loop [cnt c
            points [(rand-nth history)]]
       (if (zero? cnt)
          points
          (recur (dec cnt) (next-checked-point points history)))))


; (defn many-walks []
;   (loop [cnt 50
;           walks [[[half half]]]]
;      (if (zero? cnt)
;         walks
;         (recur (dec cnt) (cons (random-walk (* cnt cnt) (apply concat walks)) walks)))))

(defn add-walk [walks]
  (let [newwalk (random-walk 500 (apply concat walks))]
    (if (< (count newwalk) 3)
      walks
      (cons newwalk walks))))


(def colony (reagent/atom (add-walk [[[(/ half 2) half]]])))

(defn new-grid [n]
  (vec (take n (repeat (vec (take n (repeat '())))))))

(def spatial (reagent/atom (new-grid 100)))

(def grid-size 10)

(defn grid-loc [[x y] s]
  [(int (/ x s))
   (int (/ y s))])

(defn grid-insert [grid p s]
  (let [loc (grid-loc p s)]
    (if (get-in grid loc)
     (assoc-in grid loc
        (cons p (get-in grid loc)))
     (throw (Exception. "out of bounds")))))

(defn neighbors-of [p s]
 (for [dx [-1 0 1] dy [-1 0 1]]
  (vec (map + [dx dy] (grid-loc p s)))))

(defn get-buckets [grid p s]
  (mapcat (fn [np] (get-in grid np))
    (neighbors-of p s)))


(defn uploop [cnt]
  (swap! colony add-walk)
  ; (js/console.log cnt)
 (if (zero? cnt)
  cnt
  (. js/window (requestAnimationFrame #(uploop (dec cnt))))))

(uploop 1000)

(defn squigles [lines]
   (print (count (flatten lines)))
   (map-indexed
     (fn [i line]
       [:polyline {:key i :points (format-points line)}])
    lines))

(defn dots [lines]
  (map-indexed
    (fn [i [x y]]
      [:circle {:key i :cx x :cy y :r "0.7"}])
    (partition 2 (flatten lines))))


; (defn sq-collection []
;     (map
;       (fn [r]
;         [:svg {:key r :width size :height size}
;             (squigles)])
;       (range 9)))


(defn lines-page []
  [:div {:class "display"}
   [:svg { :width size :height size}
    (squigles @colony)]])
    ; (dots @colony)]])


(defn about-page []
  [:div [:h2 "other page"]
   [:div [:a {:href "/stars"} "go to the star page"]]])

(defn current-page []
  [:div [(session/get :current-page)]])

;; -------------------------
;; Routes

(secretary/defroute "/stars" []
  (session/put! :current-page #'star-page))

(secretary/defroute "/about" []
  (session/put! :current-page #'about-page))

(secretary/defroute "/lines" []
  (session/put! :current-page #'lines-page))

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!
    {:nav-handler
     (fn [path]
       (secretary/dispatch! path))
     :path-exists?
     (fn [path]
       (secretary/locate-route path))})
  (accountant/dispatch-current!)
  (mount-root))
