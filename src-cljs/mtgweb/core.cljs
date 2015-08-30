(ns mtgweb.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent.session :as session]
            [secretary.core :as secretary :include-macros true]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]
            [markdown.core :refer [md->html]]
            [cognitect.transit :as t]
            [ajax.core :refer [GET POST]])
  (:import goog.History))


(defn home-page []
  [:div.container
   [:div.jumbotron
    [:h1 "Welcome to mtgweb"]
    [:p "Time to start building your site!"]
    [:p [:a.btn.btn-primary.btn-lg {:href "http://luminusweb.net"} "Learn more »"]]]
   [:div.row
    [:div.col-md-12
     [:h2 "Welcome to ClojureScript"]]]
   (when-let [docs (session/get :docs)]
     [:div.row
      [:div.col-md-12
       [:div {:dangerouslySetInnerHTML
              {:__html (md->html docs)}}]]])])

(def sample-card {:layout "normal", :name "Deadapult", :type "Enchantment", :colors ["Red"], :types ["Enchantment"], :cmc 3, :manaCost "{2}{R}", :printings ["PLS"], :legalities [{:format "Commander", :legality "Legal"} {:format "Freeform", :legality "Legal"} {:format "Invasion Block", :legality "Legal"} {:format "Legacy", :legality "Legal"} {:format "Prismatic", :legality "Legal"} {:format "Singleton 100", :legality "Legal"} {:format "Tribal Wars Legacy", :legality "Legal"} {:format "Vintage", :legality "Legal"}], :imageName "deadapult", :text "{R}, Sacrifice a {U} Zombie: Deadapult deals 2 damage to target creature or player.\nTarget judge sucks {U} at magic {G}."})

;[:img {:src (street-view-url (@app-state :street) (@app-state :city))}]

(def mana-symbol-urls {
                   "{0}" "http://mtgjson.com/images/0.png"
                   "{1}" "http://mtgjson.com/images/1.png"
                   "{2}" "http://mtgjson.com/images/2.png"
                   "{3}" "http://mtgjson.com/images/3.png"
                   "{4}" "http://mtgjson.com/images/4.png"
                   "{R}" "http://mtgjson.com/images/r.png"
                   "{G}" "http://mtgjson.com/images/g.png"
                   "{W}" "http://mtgjson.com/images/w.png"
                   "{U}" "http://mtgjson.com/images/u.png"
                   "{B}" "http://mtgjson.com/images/B.png"

                   })

(defn get-mana-symbol-url [symbol]
  (let [r clojure.string/replace
        s (-> symbol
              (r "{" "")
              (r "}" "")
              (r "/" "")
              (clojure.string/lower-case)
              
              )]
    (str "http://mtgjson.com/images/" s ".png"))
  )

(defn mana-symbol [symbol]
  [:span
   (if-let [mana-url (get-mana-symbol-url symbol)]
     [:img {:src (get-mana-symbol-url symbol)
            :height 18
            }]
     symbol)])

(defn mana-symbols [mana-cost]
  (let [symbols (re-seq  #"\{.+?\}" mana-cost)]
    [:span#mana.manacost
      (for [symbol symbols] [mana-symbol symbol])]))

(defn magic-text [s]
  [:div
   (let [lines (clojure.string/split-lines s)
         mana-symbol-splitter #(re-seq #"\{.+?\}|[A-Za-z0-9., ]+" %)]
      (for [line lines]
        [:p
          (let [split-line (mana-symbol-splitter line)]
          (for [elem split-line]
              (if (re-matches #"\{.+?\}" elem)
              [mana-symbol elem]
              elem)))]))])

(defn magic [c]
  (when-not (empty? c)
    [:div
     [:p [:b (:name c)] (when-not (nil? (:manaCost c)) [mana-symbols (:manaCost c)])
      ]
     [:p (:type c)]
     [:p
      [magic-text (:text c)]

      ]]))



;; ###################### HTTP
(defn get-base [] js/document.location.origin)

(defn find-card-by-name [name a]
  (go (let [resp (<! (http/get (str (get-base) "/find?name=" name)))]
        (reset! a (:body resp)))))

(defn atom-input [value]
  [:input {:type "text"
           :value @value
           :on-change #(reset! value (-> % .-target .-value))}])

(defn shared-state []
  (let [val (reagent/atom "Akroma, Angel of Wrath")
        card (reagent/atom "{}")]
    (fn []
      [:div
       [:p "card: " (do (find-card-by-name @val card)[atom-input val])]
       ;;[:p "card: " (:name (t/read (t/reader :json) @card))]
        (let [cards (t/read (t/reader :json) @card)]
          (for [card cards]
            (let [formatted-card (clojure.walk/keywordize-keys card)]
              [:div
               ;(:name formatted-card)
               [magic formatted-card]
               ]))
          )
       ])))


;; ####################### 

(defn about-page []
     [shared-state]
     )




(def pages
  {:home #'about-page
   })

(defn page []
  [(pages (session/get :page))])

;; -------------------------
;; Routes
(secretary/set-config! :prefix "#")

(secretary/defroute "/" []
  (session/put! :page :home))

;; -------------------------
;; History
;; must be called after routes have been defined
(defn hook-browser-navigation! []
  (doto (History.)
        (events/listen
          EventType/NAVIGATE
          (fn [event]
              (secretary/dispatch! (.-token event))))
        (.setEnabled true)))

;; -------------------------
;; Initialize app
(defn fetch-docs! []
  (GET "/docs" {:handler #(session/put! :docs %)}))

(defn mount-components []
  (reagent/render [#'page] (.getElementById js/document "app")))

(defn init! []
  (fetch-docs!)
  (hook-browser-navigation!)
  (mount-components))
