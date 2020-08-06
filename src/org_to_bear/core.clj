(ns org-to-bear.core
  (:gen-class)
  (:import [java.net URLEncoder])
  (:require [clojure.string :as str]
            [clojure.zip :as z]
            [hickory.core :as hc]
            [hickory.render :as hr]
            [hickory.zip :as hz]
            [hickory.select :as s]))

(defn sanitize-title [t] (-> t str/trim (str/replace "\u00A0" "")))
(defn parse-title [loc] (-> loc z/next z/right z/node sanitize-title))
(defn parse-id [loc] (-> loc z/node :attrs :id))
(defn parse-tags [loc] (->> loc z/node
                           (s/select (s/child (s/class "tag") (s/tag :span)))
                           (map :content) flatten))

(defn parse-note [loc]
  (let [h (s/select-next-loc (s/tag :h1) loc)]
    {:id (parse-id h)
     :title (parse-title h)
     :tags (parse-tags h)
     :loc h}))

(defn tags->hickory [tags]
  {:type :element
   :tag :p
   :content (vec (interpose " " (map #(str "#" %) tags)))})

(defn replace-tags [entry]
  (let [tags (vec (:tags entry))]
    (-> (:loc entry)
        (z/edit #(assoc % :content [(:title entry)]))
        (z/insert-right (tags->hickory tags)))))

(defn update-tree [loc updater]
  (loop [loc loc]
    (if (z/end? loc)
      (z/root loc)
      (if-let [updated (updater loc)]
        (recur (z/next updated))
        (recur (z/next loc))))))

;; Replace links
(defn html-tag? [loc tag]
  (= (-> (z/node loc) :tag) tag))

(defn bear-link [title]
  (let [title (str/replace (URLEncoder/encode title "UTF-8") "+" "%20")
        error-callback (format "bear://x-callback-url/search?term=%s" title)]
    (format "bear://x-callback-url/open-note?title=%s&x-error=%s"
            title
            (URLEncoder/encode error-callback "UTF-8"))))

(defn try-replace-link [loc entries-index]
  (when (html-tag? loc :a)
    (when-let [href (get-in (z/node loc) [:attrs :href])]
      (when (str/starts-with? href "#")
        (let [entry-id (subs href 1)]
          (when-let [entry (get entries-index entry-id)]
            (z/edit loc #(assoc-in % [:attrs :href] (bear-link (:title entry))))))))))

(defn replace-links [loc entries-index]
  (update-tree loc #(try-replace-link % entries-index)))

;; Update headings

(defn heading-level [n]
  (when-let [tag (:tag n)]
    (when-let [m (re-matches #"h(\d)" (name tag))]
      (Integer. (second m)))))

(defn heading-tag [lvl]
  (keyword (str "h" lvl)))

(defn try-update-heading [loc]
  (when-let [lvl (heading-level (z/node loc))]
    (z/edit loc #(assoc % :tag (heading-tag (dec lvl))))))

(defn update-headings [loc]
  (update-tree loc try-update-heading))

(defn -main
  [& args]
  (when-not (first args)
    (println "you have to provide path to .html file that has been exported from org-mode")
    (System/exit 0))

  (let [parsed-doc (-> (slurp (first args)) hc/parse hc/as-hickory)
        parsed-doc (-> parsed-doc hz/hickory-zip update-headings)
        entries (s/select-locs (s/class "outline-2") parsed-doc)
        entries-index (into {} (map #(vector (:id %) %)) (map parse-note entries))
        processed-doc (-> parsed-doc hz/hickory-zip (replace-links entries-index))
        processed-entries (->> processed-doc (s/select-locs (s/class "outline-2"))
                               (map parse-note)
                               (map #(assoc % :loc (replace-tags %))))]
    (doseq [entry processed-entries]
      (spit (str "./tmp/" (:id entry) ".html") (hr/hickory-to-html (-> entry :loc z/up z/node))))))
