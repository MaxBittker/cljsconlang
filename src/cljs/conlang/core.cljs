(ns conlang.core
    (:require
      [reagent.core :as reagent :refer [atom]]
      [reagent.session :as session]
      [secretary.core :as secretary :include-macros true]
      [accountant.core :as accountant]
      [conlang.single-sans :refer [font-data]]
      [conlang.skull-data :refer [skull-data]]
      [conlang.eye-data :refer [eye-data]]
      [conlang.face-data :refer [face-data]]
      [conlang.font-data :refer [fonts-data]]
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
                get-buckets
                view-grid]]
      [conlang.vector
        :refer [add
                multiply
                random-2d
                distance
                orthog
                nrand
                normalize
                subtract
                magnitude
                cross-product
                to-polar
                to-cartesian]]
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
      [conlang.obj2 :refer [lines-to-2obj]]))


;; -------------------------
;; Views

(def tick (reagent/atom 0))

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

(def step-list
  [
    [1 0]
    [-1 0]
    [0 1]
    [0 -1]
    [1 1]
    [-1 -1]
    [-1 1]
    [1 -1]])

(defn new-point [points]
  ((fn [v]
    ; (add (multiply (random-2d) step) v)
    (add (multiply (normalize (rand-nth step-list)) (* step 1.1)) v))
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

(defn redraw [cnt]
  (swap! tick inc)
  (if (zero? cnt)
   cnt
   (. js/window (requestAnimationFrame #(redraw (dec cnt))))))

(defn uploop [cnt]
 (swap! colony add-walk)
 (if (zero? cnt)
  cnt
  (. js/window (requestAnimationFrame #(uploop (dec cnt))))))

(defn update-loop [update-fn cnt]
 (swap! tick inc)
 (update-fn cnt)
 (if (zero? cnt)
  cnt
  (. js/window (requestAnimationFrame #(update-loop update-fn (dec cnt))))))

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
  ;  [:pre
    ;  (lines-to-2obj @colony))]])
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
      ;  [:polyline  {:points (format-points  (vib p))}]
      ;  [:polygon  {:points (format-points  (vib p))}]]
        [:polyline  {:key i :points (format-points p)}]])
    pl)])


(defn point-list-to-poly [pl]
  [:g
   (map-indexed
     (fn [i p]
      [:g {:key i}
      ;  [:polyline  {:points (format-points  (vib p))}]
      ;  [:polygon  {:points (format-points  (vib p))}]]
        [:polygon  {:key i :points (format-points p)}]])
    pl)])
; (defn point-list-to-poly-paths [pl])
  ; (point-list-to-paths (pl)))

(defn alphabet-page []
  (map-indexed
    (fn [i d]
      [:svg {:key i :width 40 :height 40}
       (point-list-to-paths (d-to-points (:d d)))])
    (apply concat (map #(:chars %) (vals fonts-data)))))

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
 (map #(nth (:chars (:gothicger fonts-data))
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
          [(* s 1.6 (- oS o))
           (/ size 2.5)])))
    (map vector dseq oseq))))

(def word-points
 (str-to-points "mycelium" lscale))

; (swap! spatial-grid grid-insert-many
                ; (apply concat word-points) tile-size))

(reset! mvrs (apply concat
                (translate-points
                  (d-to-points (:d (rand-nth font-data)))
                  lscale
                  [-20 -20])))

(defn negative-letters []
      [:div {:class "display"}
       [:svg { :width size :height size}
        (squigles @colony)]])
        ; (point-list-to-paths word-points)]])

(defn words-page []
      [:div {:class "display"}
       [:svg {:width size :height size}
        (squigles @colony)
        (point-list-to-paths word-points)]])

; "marble moon", "mm", "innernette", or "maze"

(def vlines
  (partition 2
    (apply concat
      (map
        (fn [y] [[y 0] [y size]])
        (range 5 size 5)))))

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
    ; [:pre]
      ; (lines-to-2obj)]
    [:svg {:width size :height size}
      (point-list-to-paths
        (apply concat
          (map-indexed
            #(map-indexed
               (fn [i line]
                  (map (fn [[x y]]
                          (let [[a r] (to-polar [x y])]
                            (to-cartesian
                              [(* (+ a (* 0.25 Math/PI)) 2)
                               r])))
                            ; [(+ x (* % 3 (Math/sin (*  0.1 y))))]))
                            ;  (+ y (* % 3 (Math/sin (*  0.1 x))))]))
                    (vib line (* 0.4 %))))
              (normalize-lines grid))
           (range 3))))]])

(defn make-spiral [n j]
  (line-to-cartesian
   (map
    (fn [i] [i i])
    (range 0 n j))))


(defn make-circle
  ([r] (make-circle r 0.5))
  ([r s]
   (line-to-cartesian
     (normalize-line
       (map
        (fn [i] [i r])
        (range 0 (* 2 3.15) (* (/ 1 (inc r)) s)))))))

(defn spiral-page []
  [:div {:class "display thin"}
  ;  [:pre {:width size :height size}
    ; (lines-to-2obj
     [:svg {:width size :height size}
      (point-list-to-paths
        (map
          (fn [i]
            (map
             (fn [[x y]]
               (let [[a r] (to-polar [x y])
                      amp (- 8 (* 8 (/ i half) (/ i half) (/ i half)))
                      f .1]
                [(+ x (* amp (Math/cos (* f y))))
                 (+ y (* amp (Math/sin (* f x))))]))
             (make-circle i)))
         (range 1 half 1)))]])

(defn line-glyph [np nl]
  (map
    (fn [line]
      (if (= 1 (count (set line)))
       (map-indexed
        (fn [i [x y]]
          [(+ x (Math/sin (* i 0.9)))
           (+ y (Math/cos (* i 0.9)))])
        (take 10 (repeat (first line))))
       line))
   (build-glyph np nl)))

(defn random-letter-stamp []
  ; (map #(vib % 0.1)
   (normalize-lines
    (translate-points
      ; (line-glyph
              ;  (+ 3 (rand-int 8))
              ;  (+ 3 (rand-int 8)
            ; (d-to-points (:d (rand-nth
            ;                       (apply concat (map #(:chars %) (vals fonts-data))))))
            ; (d-to-points (:d (rand-nth (:chars (:astrology fonts-data)))))

          (d-to-points (:d (nth (:chars (:astrology fonts-data))
                                (rand-nth (concat
                                             [0]
                                             (range 2 7)
                                             (range 9 11)
                                             [12]
                                             [14]
                                             (range 25 32)
                                             [58]
                                             (range 60 65)
                                             (range 90 95))))))



      ; (+ 0.3 (* 2 (Math/random)))
      0.5
      [(rand-int (- size 10))
       (rand-int (- size 10))])))


(defn sketch-to-points [data]
 (map
   (fn [[xs ys ts]]
     (map vector xs ys))
   (:drawing data)))

(defn random-skull-stamp []
 ; (map #(vib % 0.1)
 (normalize-lines
   (translate-points
     (sketch-to-points (rand-nth face-data))
     0.05
     [(rand-int (- size 10))
      (rand-int (- size 10))])))


(defn check-lines [lines grid]
  (not-any?
    (fn [point]
      ; (or
      ;  (> (distance point [half half]) (* 0.9 half))
       (some
        (fn [op]
          (< (distance op point) (* 0.6 step)))
        (get-buckets grid point tile-size)))
    (apply concat lines)))

(defn non-overlapping-letters []
  (loop [grid (new-grid grid-width)
         letters []
         cnt 50]
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

(defn non-overlapping-skulls []
 (loop [grid (new-grid grid-width)
        letters []
        cnt 4000]
  (if (= cnt 0)
   letters
   (let [cletter (random-skull-stamp)]
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
  (let [pl (apply concat (non-overlapping-letters))]
    [:div {:class "display med"}
     [:svg {:width size :height size}
      (point-list-to-paths pl)]
     [:pre (lines-to-2obj pl)]]))


(defn draws-page []
  (let [pl (apply concat (non-overlapping-skulls))]
  ; (let [pl (sketch-to-points (rand-nth skull-data))]
   [:div {:class "display med"}
    [:svg {:width size :height size}
     (point-list-to-paths pl)]]))
    ; [:pre pl]]))



(defn thing-field [thing o]
  (for [x (range (- half) half o) y (range (- half) half o)]
    (map
      #(translate-line
         %
         1
         [x y])
      thing)))

(def skulls
  (map
    #(sketch-to-points (rand-nth face-data))
   (range 1000)))

(defn offset-thing-field [thing o]
  (let [sz (* 0.8 half)]
    (for [x (range (- sz) sz (* o 0.85)) y (range (- sz) sz o)]
    ; (for [x (range 0 (* 2 sz) o) y (range 0 (* 2 sz) o)]
        (map
          #(vib
            (translate-line
             %
             0.08
             [(+ x half)
              (+ y half (* o  0.5 (mod (/ x (* o 0.85)) 2)))])
            1)
            ; (/ x sz))
          (nth skulls (int (+ @tick (* 0.001 (+ x y) (+ x y)))))))))

; (defn concentric-circles [r stp]
;   (map
;    (fn [i] (vib (make-spiral i 0.1) 1))
;    (range 4 r stp)))


(defn ngon [r n]
  (map
    #(to-cartesian
       [(* 2 3.14 (/ % n)) r])
   (range 0 (inc n))))


(defn field-page []
 (let [pl (apply concat (offset-thing-field
                             #(translate-points
                               (sketch-to-points (rand-nth face-data))
                               0.1
                               [half half])
                             25))]
  ;  #(line-glyph 4 20) 10
   [:div {:class "display thin"}
    [:svg {:width size :height size :class @tick}
     (point-list-to-paths pl)]
    [:pre (lines-to-2obj pl)]]))

(defn random-circle-point [r]
  (to-cartesian
    [(rand 100) r]))

(defn chord [cr]
 (vib
   (map
     (fn [[x y]]
       (let [[a r] (to-polar [x y])
             amp (- 4 (* 4 (/ r half) (/ r half) (/ r half) (/ r half)))
             f 0.5]
        ; (to-cartesian)
          ; [(+ a (* 0.003 (/ half r) (/ half r)))
          ;  r)
        [(+ x (* amp (Math/sin (* f y))))
         y]));  (+ y (* amp (Math/cos (* f x))))]))
     (normalize-line
         [(random-circle-point cr)
          (random-circle-point cr)]))
  0.5))

(defn marble-page []
 (let [pl (take 1000 (repeatedly #(chord half)))]
   [:div {:class "display thin"}
    [:svg {:width size :height size}
      (point-list-to-paths pl)]]))
    ; [:pre (lines-to-2obj pl)]]))

(defn shading []
  (for [x (range 0 size 30) y (range size)]
   (vib (normalize-line [[y y] [x y]])
    (/ y size 0.4))))

(defn shade-page []
 (let [pl (shading)]
   [:div {:class "display thin"}
    [:svg {:width size :height size}
      (point-list-to-paths pl)]]))
    ; [:pre (lines-to-2obj pl)]]))


(defn circle-width [h r]
  (Math/sqrt
    (- (* r r) (* h h))))

(defn hatched-circle [r a]
    (map
      (fn [h]
       (map
         (fn [[ang rad]]
            (to-cartesian [(+ ang a) rad]))
         (line-to-polar
           (map
             (fn [[x y]] [(+ half x) (+ half y)])
            [[(circle-width h r) h]
             [(- (circle-width h r)) h]]))))
     (range (- r) r)))

(defn hatch-composition []
  (let [edge  (- size 40)
        ehalf  (/ edge 2)]
    (take 10
      (repeatedly
        (fn []
          (translate-points
           (hatched-circle 30 (rand 100))
           1
           [(- ehalf (rand-int edge))
            (- ehalf (rand-int edge))]))))))

(defn overlap-page []
 ; (let [pl (apply concat (hatch-composition))])
 (let [pl (apply concat (offset-thing-field #(hatched-circle 9 (rand 100)) 10))]
   [:div {:class "display thin"}
    [:svg {:width size :height size}
      (point-list-to-paths pl)]
    [:pre (lines-to-2obj pl)]]))

(def archive-piles
  (reagent/atom []))

(def pile-points
   (reagent/atom
      ; (line-to-polar (make-circle half 5))
     (line-to-polar
      (translate-line
        (normalize-line
          (ngon 90 4))
            ; (+ 4 (rand-int 3))))
        [0 0]))))

(defn get-closest [neighbors origin]
  (reduce
    (fn [a b]
      (if (<
            (magnitude (subtract a origin))
            (magnitude (subtract b origin)))
       a b))
    neighbors))

(defn repel [self neighbor]
    (let [d (subtract neighbor self)]
      (if (< (magnitude d) 2)
        [0 0]
        d)))

(defn sum-forces [self neighbors]
  (let [summed
        (apply add
          (map
           #(repel self %)
           neighbors))]
   (add
     (normalize summed)
     (normalize (subtract self [0 0])))))

(defn field [[a r]]
  (let [[x y] (to-cartesian [a r])]
    (*
      (Math/sin x)
      (Math/cos y))))

(defn soften [points n]
  (map
    (fn [[a b c]]
      (to-polar (multiply
                    (add a (multiply b n) c [0 0])
                    (/ 1 (+ n 2)))))
   (partition 3 1
     (concat [(last points)] points [(first points)]))))

(defn add-scale [points]
  (map
    (fn [[a r]]
       [(+ a (* 0.001 a (rand)) 0.004)
        (- r 0.1)])
          ; (* 0.000 r))])
          ; (* 0.8 (field [a r]))
   (soften
    ; (normalize-line (line-to-cartesian points)
     (translate-line
      (vib
        (prune-line (line-to-cartesian points) 1)
        1)
      [0 0])
    5)))
    ; points)))

(defn update-pile [t]
  ; (print @pile-points)
  (cond (zero? (mod t 3))
    (swap! archive-piles conj
      (close-loop (line-to-cartesian @pile-points))))
  (print (count @pile-points))
  (swap! pile-points add-scale))

(defn pile-page []
 (let [pl (conj (rest @archive-piles)
            (line-to-cartesian @pile-points))]
   [:div {:class "display thin"}
    [:svg {:width size :height size}
      (point-list-to-paths pl)]
    (cond (> (count @archive-piles) 100)
     [:pre (lines-to-2obj (rest @archive-piles))])]))

      ; (point-list-to-paths grid-vis)]]))

(defn seedpt []
  (let [a (add [half half]
            (multiply (random-2d) (* half 0.3
                                    (+ 1.0(rand)))))
        b (add a (random-2d))]
    [a b]))

(def sol-points
   (reagent/atom
     (repeatedly 300
       seedpt)))
        ; #(identity
        ;   [[half half]
        ;    (add
        ;      [half half]
        ;      [(nrand) (nrand)])]))))

(defn rotate [[x y] a]
  [
   (- (* x (Math/cos a))
      (* y (Math/sin a)))
   (+ (* x (Math/sin a))
      (* y (Math/cos a)))])

(defn grow-squig [points]
  (let [[sl l] (take-last 2 points)
         dir (normalize (subtract sl l))
         rotdir (normalize (rotate dir (+ Math.PI -1.1 (rand 1.5))))
         new-dir (multiply rotdir 3)
         pull (multiply
                (subtract [half half] l)
                (/ 1 half))
         f (multiply
            (add pull
             (multiply (normalize pull) -0.5))
            1.5)
         new (add l new-dir f)]
    ; (print (magnitude f))
    (conj points new)))

(defn grow-squigs [sqs]
  (map grow-squig sqs))

(def buffer
  (reagent/atom @sol-points))

(defn update-sol [t]
  (if (zero? (mod @tick 50))
    (reset! buffer
       (mapcat
         #(partition 14 13 %)
        @sol-points))
    (swap! sol-points grow-squigs)))

(defn sol-page []
 (let [pl @buffer]
   [:div {:class "display thin"}
    ; [:pre (str pl)]]))
    [:svg {:width size :height size}
      ; (point-list-to-paths [pl])
      (point-list-to-paths pl)]]))

    ; (cond (> (count @archive-piles) 100)
    ;  [:pre (lines-to-2obj (rest @archive-piles))]]]))))

      ; (point-list-to-paths grid-vis)]]))


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
  (uploop 10000)
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

(secretary/defroute "/field" []
  (redraw 10000)
  (session/put! :current-page #'field-page))

(secretary/defroute "/shade" []
  (session/put! :current-page #'shade-page))

(secretary/defroute "/marble" []
  (session/put! :current-page #'marble-page))

(secretary/defroute "/overlap" []
  (session/put! :current-page #'overlap-page))

(secretary/defroute "/draws" []
  (session/put! :current-page #'draws-page))

(secretary/defroute "/pile" []
  (update-loop update-pile 1520)
  (session/put! :current-page #'pile-page))

(secretary/defroute "/sol-squig" []
  (update-loop update-sol 152000)
  (session/put! :current-page #'sol-page))


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
