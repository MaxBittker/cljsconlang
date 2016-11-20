(ns conlang.core
    (:require [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [secretary.core :as secretary :include-macros true]
              [accountant.core :as accountant]))

;; -------------------------
;; Views

(def input-word (reagent/atom "test"))

(defn counting-component []
  [:div
   [:input {:type "text" :value @input-word
            :on-change #(reset! input-word (-> % .-target .-value))}]])

(defn format-points [points]
  (clojure.string/join " " (map #(clojure.string/join "," %) points)))

(defn random-points []
  (take 700
   (repeatedly
     (fn []
       [(dec (*  (Math/random)(Math/random) 250))
        (dec (*  (Math/random)(Math/random) 250))]))))

(def pts (reagent/atom (random-points)))

(defn vib [points]
  (map (fn [[x y]]
        [(+ x (- 0.5 (Math/random)))
         (+ y (- 0.5 (Math/random)))])
       points))

(defn warp [points]
 (map (fn [[x y]]
       [(mod (- x 0.99) 250)
        (mod (- y 0.99) 250)])
      points))

(defn updater []
  (swap! pts (comp vib warp))
  (. js/window (requestAnimationFrame updater)))

(updater)

(defn home-page []
  (let [formated (format-points @pts)]
   [:div {:class "display"}
    ; [counting-component]
    [:svg {:width 500 :height 500}
      [:g {:transform "translate(250, 250)"}
        [:g {:transform "rotate(0, 0, 0)"}
         [:polygon {:points formated}]]
        [:g {:transform "rotate(90, 0, 0)"}
         [:polygon {:points formated}]]
        [:g {:transform "rotate(180 ,0, 0)"}
         [:polygon {:points formated}]]
        [:g {:transform "rotate(270, 0, 0)"}
         [:polygon {:points formated}]]]]]))


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
