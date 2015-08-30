(ns mtgweb.mtg
  (:require [cheshire.core :refer :all])
  (:gen-class))

;;######################
;;# Main defs
;;######################

(def json-file "cards.json")
(def download-location "http://mtgjson.com/json/AllCards-x.json")
(defn copy-uri-to-file [uri file]
  (with-open [in (clojure.java.io/input-stream uri)
              out (clojure.java.io/output-stream file)]
    (clojure.java.io/copy in out)))

(when-not (.exists (clojure.java.io/as-file json-file))
  (copy-uri-to-file download-location json-file))

(defonce cards (atom []))

(defn load-cards [] (sort-by :name (map second (parse-string (slurp "cards.json") true))))

(defn fetch-card-db! []
   (when-not (.exists (clojure.java.io/as-file json-file))
     (copy-uri-to-file download-location json-file))
   (reset! cards (load-cards)))

;;######################
;;# Generic Helper Functions
;;######################

(def not-nil? (complement nil?))

(defn format-text [text] (clojure.string/replace (str "\n" text) "\n" "\n\t"))

(defn empty-string-if-nil [s]
  (if (nil? s)
    ""
    s))

(defn print-card [c] (println (:name c) (:types c) (:manaCost c)
                              (str (empty-string-if-nil (:power c))
                                   (if (:power c) "/" "")
                                   (empty-string-if-nil (:toughness c)))
                               (format-text (:text c)) "\n"))

(defn print-cards [cs] (doseq [c cs] (print-card c)))

(def un complement) ;; (un red?) sounds better than (complement red?)

(def cmds (atom {}))
(defmacro defc [name body]
  (let [evaled (eval body)]
    (swap! cmds assoc (str name) evaled)
    (intern *ns* name evaled)))




;;######################
;;# Attribute Readers
;;######################
(defn parse-mtg-numeric
  "Coerce '1' to 1
  '*' to 0
  1  to 1
          nil to 0"
  [s]
  (try (int (bigdec s))
       (catch Exception e
         0)))

(defn power     [c] (parse-mtg-numeric (:power c)))
(defn toughness [c] (parse-mtg-numeric (:toughness c)))
(def cmc :cmc)
(def name :name)
(def mana-cost :manaCost)

;;######################
;;# Filter Functions
;;######################
(defn- generic-test-builder
  [field value]
  (fn [c] (not-nil? (some #(= value %) (field c)))))

(def color= (partial generic-test-builder :colors))
(defc black? (color= "Black"))
(defc red?   (color= "Red"))
(defc green? (color= "Green"))
(defc white? (color= "White"))
(defc blue?  (color= "Blue"))

(defc not-red?   (complement red?))
(defc not-green? (complement green?))
(defc not-white? (complement white?))
(defc not-blue?  (complement blue?))
(defc not-black? (complement black?))

(defc colorless? (every-pred not-red? not-green? not-white? not-blue? not-black?))

(defc only-black? (every-pred not-red? not-green? not-white? not-blue? black?))
(defc only-red?   (every-pred red?     not-green? not-white? not-blue? not-black?))
(defc only-green? (every-pred not-red? green?     not-white? not-blue? not-black?))
(defc only-white? (every-pred not-red? not-green? white?     not-blue? not-black?))
(defc only-blue?  (every-pred not-red? not-green? not-white? blue?     not-black?))

(defn- optional-operator-test-builder
  "Used for the creation of comparison functions for numeric values."
  [key operator val]
  (fn [c]
    (let [card-val (key c)]
      (if (nil? card-val)
        false
        (operator (parse-mtg-numeric card-val) val)))))
(def cmc<  (partial optional-operator-test-builder :cmc <))
(def cmc<= (partial optional-operator-test-builder :cmc <=))
(def cmc=  (partial optional-operator-test-builder :cmc =))
(def cmc>  (partial optional-operator-test-builder :cmc >))
(def cmc>= (partial optional-operator-test-builder :cmc >=))

(def power<  (partial optional-operator-test-builder :power <))
(def power<= (partial optional-operator-test-builder :power <=))
(def power=  (partial optional-operator-test-builder :power =))
(def power>  (partial optional-operator-test-builder :power >))
(def power>= (partial optional-operator-test-builder :power >=))

(def toughness<  (partial optional-operator-test-builder :toughness <))
(def toughness<= (partial optional-operator-test-builder :toughness <=))
(def toughness=  (partial optional-operator-test-builder :toughness =))
(def toughness>  (partial optional-operator-test-builder :toughness >))
(def toughness>= (partial optional-operator-test-builder :toughness >=))

(defn legal= [fmt]
  (fn [c]
    (= "Legal"
       (-> (filter #(= (:format %) fmt) (:legalities c))
           first
           :legality))))
(defc modern-legal?    (legal= "Modern"))
(defc standard-legal?  (legal= "Standard"))
(defc commander-legal? (legal= "Commander"))
(defc legacy-legal?    (legal= "Legacy"))
(defc vintage-legal?   (legal= "Vintage"))

(def type= (partial generic-test-builder :type))
(defc artifact?     (type= "Artifact"))
(defc creature?     (type= "Creature"))
(defc land?         (type= "Land"))
(defc enchantment?  (type= "Enchantment"))
(defc planeswalker? (type= "Planeswalker"))
(defc instant?      (type= "Instant"))
(defc sorcery?      (type= "Sorcery"))
(defc tribal?       (type= "Tribal"))

(def supertype= (partial generic-test-builder :supertypes))
(defc legendary? (supertype= "Legendary"))
(defc snow?      (supertype= "Snow"))
(defc world?     (supertype= "World"))
(defc basic?     (supertype= "Basic"))

(def subtype= (partial generic-test-builder :subtypes))
(defc angel?   (subtype= "Angel"))

(defn has-text
  "Returns whether or not a card contains a regex expression. Ignores case."
  [text]
  (fn [c]
    (let [txt (:text c)]
      (if (nil? txt)
        false
        (not-nil? (re-find (re-pattern (clojure.string/lower-case text))
                           (clojure.string/lower-case (:text c))))))))

;; Useful compositions.
(defc flample? (every-pred (has-text "flying") (has-text "trample") creature?))

;;######################
;;# Find Functions
;;######################

(defn choose [cards & predicates]
  (filter (apply every-pred predicates) cards))

(def pchoose (comp print-cards (partial choose cards)))

(defn find-by-name [cs name] (first (filter #(= (:name %) name) cs)))

;;######################
;;# Fuzzy Functions
;;######################

(use 'clojure.test)

(defn build-re [s]
  (re-pattern (apply str ".*" (interleave s (repeat ".*")))))

(is (= ".*z.*g.*" (str (build-re "zg"))))

(defn match-strength
  "How strongly does the partial guess match up with the actual?
  Just returns negative the number of letters that are the same."
  [a b]
  (- (count (filter #(= (first %) (second %)) (map vector a b)))))

(defn fuzzy [cs partial-name]
  (let [re (build-re (clojure.string/lower-case partial-name))]
    (sort-by #(match-strength (clojure.string/lower-case partial-name) (clojure.string/lower-case (:name %)))
             (filter #(re-matches re (clojure.string/lower-case (:name %))) cs))))

(defn query [s cards]
  (let [preds (re-seq #"[A-Za-z0-9\-\?]+" s)]
    (apply choose cards (map #(get @cmds %) preds))))




;; Lets build a regex up
;; zg -> Zameck Guildmage -> z.*g
;; za -> should expand into things.
;; zguildmage




;;######################
;;# Examples
;;######################

;; ((cmc< 3) (first cards))

;; (p (map :name (take 10 (filter (cmc= 3) cards))))

;; (p (red? (first cards)))

;; (red? (first cards))

;; (print-cards (take 10 (sort-by :name (filter red? cards))))

;; (filter #(= (:name %) "Ghostfire") (filter red? cards))


;; (filter #(= (:name %) "Ghostfire") cards)

;; ;; (p (map :name (take 10 (sort-by :name cards))))

;; (standard-legal? (find-by-name cards "Ghostfire"))

;; (artifact? (find-by-name cards "Black Lotus"))

;; (planeswalker? (find-by-name cards "Liliana of the Veil"))

;; (print-cards (filter planeswalker? cards))

;; (print-cards (choose cards (cmc> 4) red? creature?))

;; (pchoose (has-text "Suspend") (cmc> 9) red? creature?)



;; (pchoose (has-text "flying") (has-text "trample") (cmc> 2) red? creature?)

;; (first cards)

;; ((has-text "apply") (first cards))

;; (pchoose flample? (un creature?))

;; (pchoose flample? creature? black? not-red? not-green? not-white? not-blue?)

;; (print-cards (sort-by :cmc (choose cards flample? creature? black? not-red? not-green? not-white? not-blue?)))

;; (pchoose (cmc> 5) (power< 4) flample?)

;; (cmc (second (choose cards creature?)))
