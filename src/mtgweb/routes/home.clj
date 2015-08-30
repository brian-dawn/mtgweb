(ns mtgweb.routes.home
  (:require [mtgweb.layout :as layout]
            [compojure.core :refer [defroutes GET]]
            [ring.util.http-response :refer [ok]]
            [cheshire.core :refer :all]
            [mtgweb.mtg :as mtg]
            [clojure.java.io :as io]))

(defn home-page []
  (layout/render "home.html"))

(defroutes home-routes
  (GET "/" [] (home-page))
  ;(GET "/search" [query] (mtg/query))
  (GET "/find" [name] (if (< 0 (.length name))
                        (generate-string (take 20 (mtg/fuzzy @mtg/cards name)))
                        "{}"))
  (GET "/query" [query] (generate-string (mtg/query query @mtg/cards)))
  (GET "/docs" [] (ok (-> "docs/docs.md" io/resource slurp))))

