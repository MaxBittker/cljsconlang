
(ns conlang.core
    (:require
      [reagent.core :as reagent :refer [atom]]
      [reagent.session :as session]
      [secretary.core :as secretary :include-macros true]
      [accountant.core :as accountant]
      [conlang.single-sans :refer [font-data]]
      [clojure.string :as string]
      [conlang.constants
        :refer [size
                half
                step
                tile-size
                grid-width
                lscale]]
      [conlang.spatial-grid
        :refer [new-grid
                grid-loc
                grid-insert
                grid-insert-many
                neighbors-of
                get-buckets]]
      [conlang.vector
        :refer [add
                multiply
                random-2d
                distance
                to-polar
                to-cartesian]]
      [conlang.point-utils
        :refer [format-points
                normalize-points
                normalize-line
                normalize-lines
                d-to-points
                translate-points]]))


;; -------------------------
;; Views

(def input-word (reagent/atom "test"))

(defn random-points []
  (doall
   (take 1
     (repeatedly
       (fn []
         [(dec (*  (Math/random) (Math/random) half))
          (dec (*  (Math/random) (Math/random) half))])))))

; (def pts (reagent/atom (random-points)))
(def mvrs (reagent/atom (random-points)))
; (def mvrs (reagent/atom (apply concat (d-to-points (:d (nth font-data 50))))))

(defn walk [mvrs]
  (map (fn [[x y] [vx vy]]
          [[(+ x (* 10 vx))]
           (+ y (* 10 vy))
           [vx
            vy]])
       mvrs))

(defn vib
  ([points a]
   (map (fn [[x y]]
            [(+ x (* a (- 0.5 (Math/random))))
             (+ y (* a (- 0.5 (Math/random))))])
     points))
 ([points] (vib points 4)))


(defn warp [points]
 (map (fn [[x y]]
       [(mod (+ (- (* x 0.99) 0.01) half) half)
        (mod (+ (- (* y 0.99) 0.01) half) half)])
      points))

(defn spin
  ([points a]
   (map (fn [[x y]]
            [(+ (* x (- 1 a)) (* y a))
             (+ (* y (- 1 a)) (* x a))])
      points))
  ([points] (spin points 0.1)))

(defn updater []
  (swap! mvrs (comp warp spin))
  (. js/window (requestAnimationFrame updater)))

; (swap! @mvrs vib)

(defn star [formated n]
    (map
      (fn [r]
        [:g {:key r :transform (str "rotate(" r ", 0, 0)")}
         [:polyline {:points formated}]])
      (range 0 360 (/ 360 n))))

(defn collection [points n]
  (let [formated (format-points points)]
    (map
      (fn [r]
        [:svg {:key r :width size :height size}
          [:g {:transform (str "translate(" half "," half")")}
            (star points r)]])
      (range 2 11))))

(defn star-page []
   [:div {:class "display"}
      (collection (doall @mvrs) 4)])

(def spatial-grid (reagent/atom (new-grid grid-width)))

(defn new-point [points]
  ((fn [v]
    (add (multiply (random-2d) step) v))
   (first points)))

(defn check-point [p points]
  (or
   (some (fn [op] (< (distance op p) step)) points)
   (> (distance p [half half]) (/ size 2))
   (< (distance p [half half]) (/ size 6))))

(defn fast-check-point [p points]
  (or
   (some
    (fn [op] (< (distance op p) step))
    (concat points (get-buckets @spatial-grid p tile-size)))
   (> (distance p [half half]) (/ size 2))))
  ;  (< (distance p [half half]) (/ size 6))))

(defn next-checked-point [points history]
  (let [np (new-point points)]
    (if (fast-check-point np points)
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
      (do
       (swap! spatial-grid
        (fn [grid] (grid-insert-many
                      grid newwalk tile-size)))
       (cons newwalk walks)))))


(def colony (reagent/atom (add-walk [[[(/ half 2) half]]])))

(defn uploop [cnt]
 (swap! colony add-walk)
 ; (cond
  ; (zero? (mod cnt 100)))
  ; (print (map count (apply concat @spatial-grid))))
 (if (zero? cnt)
  cnt
  (. js/window (requestAnimationFrame #(uploop (dec cnt))))))

;
(defn squigles [lines]
  ;  (print (count (flatten lines)))
   (map-indexed
     (fn [i line]
       [:polyline {:key i :points (format-points line)}])
    lines))

(defn dots [lines]
  (map-indexed
    (fn [i [x y]]
      [:circle {:key i :cx x :cy y :r "0.6"}])
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

(def grid-points
   (for [dx (range 3 18 5) dy (range 3 18 5)]
    [dx dy]))

(defn point-set [n]
  (set (take n (repeatedly #(rand-nth grid-points)))))

(defn random-connection [points]
  (vec (take 2 (repeatedly #(rand-nth (vec points))))))

(defn build-glyph [np nl]
  (let [ps (point-set np)]
    (take nl (repeatedly #(random-connection ps)))))

(defn glyph [np nl]
 (map-indexed
   (fn [i line]
     (if (= 1 (count (set line)))
      [:circle {:key i :cx (first (first line)) :cy (second (first line)) :r "1.5"}]
      [:polyline {:key i :points (format-points line)}]))
  (build-glyph np nl)))

(defn glyph-page [n]
  (map-indexed
    (fn [i g]
      [:svg {:key i :width 16 :height 16}
       g])
    ; (map
    ;  (fn [[np nl]] (glyph np nl))
    ;  (for [dx (range 3 11) dy (range 3 11)]
    ;   [dx dy]))
    (take n (repeatedly
              (fn [] (glyph
                       (+ 3 (rand-int 8))
                       (+ 3 (rand-int 8))))))))


(defn code-page []
  [:div {:class "display"}
    (glyph-page 1560)])


(defn about-page []
  [:div [:h2 "other page"]
   [:div [:a {:href "/stars"} "go to the star page"]]])

(defn point-list-to-paths [pl]
  [:g
   (map-indexed
     (fn [i p]
      [:g {:key i}
       [:polyline  {:points (format-points  (vib p))}]
       [:polyline  {:points (format-points  (vib p))}]]
      [:polyline  {:key i :points (format-points p)}])
    pl)])

(defn alphabet-page []
  (map-indexed
    (fn [i d]
      [:svg {:key i :width 40 :height 40}
       (point-list-to-paths (d-to-points (:d d)))])
    font-data))

(defn letter-page []
   [:div {:class "display"}
    (alphabet-page)])

(def letter-points
 (normalize-lines
  (translate-points
    (d-to-points (:d (nth font-data 50)))
    lscale
    [(/ size 4.5)
     (/ size 4.5)])))

(defn str-to-data [input]
 (map #(nth font-data
           (- (.charCodeAt %) 33)
           {:d "0,0" :o 4})
  input))

(defn str-to-points [input s]
 (let [dseq (str-to-data input)
       oseq (reductions + (map :o dseq))]
   (mapcat
     (fn [[{d :d o :o} oS]]
       (normalize-lines
        (translate-points
          (d-to-points d)
          s
          [(* s 1.8 (- oS o))
           (/ size 4.5)])))
    (map vector dseq oseq))))

(def word-points
 (str-to-points "maze" lscale))

(swap! spatial-grid grid-insert-many
                (apply concat word-points) tile-size)

(reset! mvrs (apply concat
                (translate-points
                  (d-to-points (:d (rand-nth font-data)))
                  lscale
                  [-20 -20])))

(defn negative-letters []
      [:div {:class "display"}
       [:svg { :width size :height size}
        (squigles @colony)
        (point-list-to-paths letter-points)]])

(defn words-page []
      [:div {:class "display"}
       [:svg {:width size :height size}
        (squigles @colony)
        (point-list-to-paths
         (str-to-points "moon" lscale))]])

; "marble moon", "mm", "innernette", or "maze"

(def vlines
  (partition 2
    (apply concat
      (map
        (fn [y] [[y 0] [y size]])
        (range 10 size 10)))))

(def hlines
    (map
      (fn [[[ax ay] [bx by]]]
       [[ay ax] [by bx]])
      vlines))

(def grid
  (apply concat
    (map vector hlines vlines)))

(defn line-to-polar [points]
  (map #(to-polar %) points))

(defn line-to-cartesian [points]
  (map #(to-cartesian %) points))

(defn hatch-page []
   [:div {:class "display thin"}
    [:svg {:width size :height size}
      (point-list-to-paths
        (apply concat
          (map
            #(map-indexed
               (fn [i line]
                  (map (fn [[x y]]
                          [(+ x %)
                           (+ y %)])
                    (vib line 0.8)))
              (normalize-lines grid))
           (range 7))))]])

(defn make-spiral [n j]
  (line-to-cartesian
   (map
    (fn [i] [i i])
    (range 0 n j))))


(defn make-circle [r]
 (normalize-line
   (map
    (fn [i] [i r])
    (range 0 (* 2 3.15) 0.1))))

(defn translate-line [pl s o]
   (map
    (fn [p] (add (multiply p s) o))
    pl))

(defn spiral-page []
  [:div {:class "display thin"}
   [:svg {:width size :height size}
    (point-list-to-paths
        (map
          (fn [i]
            (map
             (fn [[a r]]
               (let [[x y] (to-cartesian [a r])
                      amp 1
                      f 0.5]
                [(+ x (* amp (Math/cos (* f y))))
                 (+ y (* amp (Math/sin (* f x))))]))
             (make-circle i)))
         (range 0 size 2)))]])

(defn line-glyph [np nl]
  (map
    (fn [line]
      (if (= 1 (count (set line)))
       (vib (take 20 (repeat (first line))) 2)
       line))
   (build-glyph np nl)))

(defn random-letter-stamp []
  ; (map #(vib % 0.1)
   (normalize-lines
    (translate-points
      (line-glyph
               (+ 3 (rand-int 8))
               (+ 3 (rand-int 8)))
                ;(d-to-points (:d (nth font-data (+ (rand-int 26) 64))))
      (+ 0.3 (* 4 (Math/random)))
      [(rand-int (- size 80))
       (rand-int (- size 80))])))

(defn check-lines [lines grid]
  (not-any?
    (fn [point]
      (or
       (> (distance point [half half]) (* 0.9 half))
       (some
        (fn [op]
          (< (distance op point) (* 2 step)))
        (get-buckets grid point tile-size))))
    (apply concat lines)))

(defn non-overlapping-letters []
  (loop [grid (new-grid grid-width)
         letters []
         cnt 11000]
   (if (= cnt 0)
    letters
    (let [cletter (random-letter-stamp)]
      (if (check-lines cletter grid)
        (recur
         (grid-insert-many grid (apply concat cletter) tile-size)
         (cons cletter letters)
         (dec cnt))
        (recur
         grid
         letters
         (dec cnt)))))))


(defn text-page []
  [:div {:class "display med"}
   [:svg {:width size :height size}
    (point-list-to-paths
      (apply concat (non-overlapping-letters)))]])

(defn current-page []
  [:div [(session/get :current-page)]])

;; -------------------------
;; Routes

(secretary/defroute "/stars" []
  (updater)
  (session/put! :current-page #'star-page))

(secretary/defroute "/about" []
  (session/put! :current-page #'about-page))

(secretary/defroute "/lines" []
  (session/put! :current-page #'lines-page))

(secretary/defroute "/code" []
  (session/put! :current-page #'code-page))

(secretary/defroute "/letters" []
  (session/put! :current-page #'letter-page))

(secretary/defroute "/words" []
  (uploop 10000)
  (session/put! :current-page #'words-page))

(secretary/defroute "/negative-letters" []
  (uploop 10000)
  (session/put! :current-page #'negative-letters))

(secretary/defroute "/hatch" []
  (session/put! :current-page #'hatch-page))

(secretary/defroute "/spiral" []
  (session/put! :current-page #'spiral-page))

(secretary/defroute "/text" []
  (session/put! :current-page #'text-page))

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
