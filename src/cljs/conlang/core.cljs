
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
  (take 10
   (repeatedly
     (fn []
       [(dec (*  (Math/random)(Math/random) half))
        (dec (*  (Math/random)(Math/random) half))]))))

(def pts (reagent/atom (random-points)))

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
  (swap! pts (comp vib warp))
  (. js/window (requestAnimationFrame updater)))

(updater)

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
            (star @pts r)]])
      (range 2 11))))

(defn home-page []
   [:div {:class "display"}
      (collection @pts 4)])

(defn about-page []
  [:div [:h2 "About conlang"]
   [:div [:a {:href "/"} "go to the home page"]]])

(defn current-page []
  [:div [(session/get :current-page)]])

;; -------------------------
;; Routes

(secretary/defroute "/" []
  (session/put! :current-page #'home-page))

(secretary/defroute "/about" []
  (session/put! :current-page #'about-page))

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
