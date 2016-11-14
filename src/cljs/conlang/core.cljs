(ns conlang.core
    (:require [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [secretary.core :as secretary :include-macros true]
              [accountant.core :as accountant]))

;; -------------------------
;; Views
(defn format-points [points]
  (clojure.string/join " " (map #(clojure.string/join "," %) points)))

(defn random-points []
  (take (* 100 (Math/random) (Math/random))
   (repeatedly
     (fn []
       [(dec (* (Math/random) (Math/random) 250))
        (dec (* (Math/random) (Math/random) 250))]))))


(defn home-page []
  (let [pts (random-points)]
   [:div {:class "display"}
    [:svg {:width 500 :height 500}
      [:g {:transform "translate(250, 250)"}
        [:g {:transform "rotate(0, 0, 0)"}
         [:polygon {:points (format-points pts)}]]
        [:g {:transform "rotate(90, 0, 0)"}
         [:polygon {:points (format-points pts)}]]
        [:g {:transform "rotate(180 ,0, 0)"}
         [:polygon {:points (format-points pts)}]]
        [:g {:transform "rotate(270, 0, 0)"}
         [:polygon {:points (format-points pts)}]]]]]))

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
