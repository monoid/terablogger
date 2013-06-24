(ns terablogger.core
  (:require [clostache.parser :refer :all]
            [clojure.string :as string]
            [terablogger.cfg :as cfg]
            [terablogger.apath :as apath]
            [terablogger.format-html])
  (:gen-class))

(def ^:dynamic *cats*
  "Parsed categories.")

(def ^:dynamic *posts*
  "Parsed posts hash.")


(def list-cats
  "Function that return list of category files."
  (apath/data-lister #"\.db$" compare))

(def list-posts
  "Function that return list of category posts."
  (apath/data-lister #"\.txt$" #(compare %2 %1)))

(defn paginated-filename [idx]
  (if (= 1 idx)
    "index.html"
    (format "index-page%d.html" idx)))

(defn paginated-bar
  "HTML fragment of page bar for multi-page sequences.

  i -- current page.
  n -- total number of pages.
  prefix -- prefix url."
  [i n prefix]
  (string/join
   " "
   (for [idx (range 1 (inc n))]
     (if (= i idx)
       (format "[%d]" i)
       (format "[<a%s href='%s'>%d</a>]"
               (cond
                (= (inc idx) i) " rel='prev'" ; idx = i-1
                (= (inc i) idx) " rel='next'" ; idx = i+1
                :else "")
               (str prefix "/" (paginated-filename idx))
               idx)))))

(defn paginate
  "Split sequence of posts into pages of :page-size length."
  [posts url-prefix]
  (let [numbers (iterate inc 1)
        pages   (partition-all (:page-size cfg/*cfg*) posts)
        npages  (count pages)]
    (map list pages
              (map #(paginated-bar % npages url-prefix) numbers)
              (map paginated-filename numbers))))

(defn truncatechars
  "If msg's length exeeds n, truncate it, appending '...'. "
  [msg n]
  (if (> (count msg) n)
    (str (string/trimr ; Trim for better text appearance
          (subs msg 0 (- n 3))) "...")
    msg))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Posts
;;;

(declare feed-apath month-link month-apath)

(defn parse-headers
  "Parse post's headers, returning a hash."
  [headers]
  (into {}
         (map #(let [[key val] (string/split % #": " 2)]
                    [(keyword key) val])
                 headers)))

(defn post-apath
  ([id]
     (conj (subvec (re-matches #"^(\d+)-(\d+)-(\d+)(T[0-9_]+).*" id)
                   1)
          "index.html")))

(defn post-ts
  "Return timestamp for post id."
  [id]
  (if (nil? id)
    nil
    (let [[year month day hours minutes seconds] 
          (map #(Integer/parseInt %)
               (subvec (re-matches #"^(\d+)-(\d+)-(\d+)T([0-9]+)_([0-9]+)_([0-9]+).*" id)
                       1))
          ;; tz  (/ (.getOffset (java.util.TimeZone/getDefault)
          ;;                    0 year month day 2 ; TODO TODO TODO
          ;;                    (* 1000
          ;;                       (+ seconds
          ;;                          (* 60
          ;;                             (+ minutes
          ;;                                (* 60 hours))))))
          ;;        1000 60)
          ;; tzs (format "%+03d:%02d" (quot tz 60) (Math/abs (rem tz 60)))
          tzs "Z"]
      (format "%4d-%02d-%02dT%02d:%02d:%02d%s"
              year month day hours minutes seconds tzs))))

(defn parse-post 
  "Parse blog post."
  [cats id]
  (let [txt (slurp (apath/data-path id))
        lines (string/split-lines txt)
        [headers body] (split-with #(not (re-seq #"^-----$" %)) lines)
        categories (sort #(compare (:id %1) (:id %2))
                         (filter #((:set %) id) cats))
        month (month-apath id)
        fmt (symbol (format "terablogger.format-%s/fmt" (:format cfg/*cfg*)))]
    (assoc (parse-headers headers)
      :BODY (fmt (string/join "\n" (butlast (rest (rest body))))
                 cfg/*cfg*)
      :categories categories
      :categories2 (string/join ", " (map :name categories))
      :categories3 (string/join ", "
                                (map #(format "<a href=\"%s\">%s</a>"
                                              (terablogger.format-html/html-escape
                                               (:url %))
                                              (terablogger.format-html/html-escape
                                               (:name %)))
                                     categories))
      :ID id
      :month month
      :month-link (month-link month)
      :ts (post-ts id)
      :permalink (apath/full-url-path (post-apath id)))))

(defn month-apath
  "Post's month path from id."
  [id]
  (subvec (re-matches #"^(\d+)-(\d+).*" id)
          1))

(defn months
  "Posts grouped by month-path."
  [posts]
  (group-by month-apath posts))

(defn get-cached-post-part
  "Load part from cache."
  [post-id]
  (slurp (apath/blog-path (apath/cache (post-apath post-id)))))

(defn month-text
  [m]
  (let [sym (java.text.DateFormatSymbols/getInstance)]
    ;; Order: month year.  It is not clear if
    ;; there is a locale-specific way for
    ;; formatting as it is not date, only month
    ;; and year.
    (format "%s %s"
            (get (.getMonths sym)
                 (dec (Integer/parseInt (nth m 1))))
            (nth m 0))))

(defn write-month
  [[month-id posts]]
  (let [sorted-posts (sort #(compare %2 %1) posts)
        p (apath/full-url-path (apath/archive month-id))
        pages (paginate sorted-posts p)]
    ;; Paginate
    (dorun
     (for [[pposts ptab pfname] pages
           :let [apath (apath/archive (conj month-id pfname))]]
       ;; Write each page
       (apath/spit* apath
                    (render (slurp "./blog/templates/month-archive.mustache")
                            {:body (string/join "" (map get-cached-post-part pposts))
                             :tab ptab
                             :cfg cfg/*cfg*
                             :feed (apath/full-url-path (feed-apath []))
                             :month (month-text month-id)
                             }))))))

(defn write-months
  [months]
  (dorun
   (for [month months]
     (write-month month))))

(defn write-post
    "Write post to cache and to archive."
    [post]
    (let [cfg      (assoc cfg/*cfg* :archive
                          (apath/full-url-path (apath/archive [])))
          entry    (assoc post
                     :categories? (boolean (seq (:categories post))))
          capath    (apath/cache (post-apath (:ID post)))
          aapath    (apath/archive (post-apath (:ID post)))
          content  (render (slurp "./blog/templates/entry.mustache")
                          {:cfg cfg :entry entry})
          pcontent (render (slurp "./blog/templates/permalink-entry.mustache")
                           {:cfg cfg :entry entry})]
      ;; Cached part
      (apath/spit* capath content)
      ;; Full article page
      (apath/spit* aapath (render (slurp "./blog/templates/permalink.mustache")
                                  {:body pcontent
                                   :cfg cfg/*cfg*
                                   :title (:TITLE entry)
                                   :feed (apath/full-url-path (feed-apath []))}))))

(defn ls-posts
  [posts]
  (dorun
   (for [[p n] (map list posts (range))]
     (let [pcats (:categories p)]
       (print (format "%d. %s"
                      (inc n)
                      (truncatechars (:TITLE p) 32)))
       (when (seq pcats)
         (print (format " - [%s]" (string/join ", " (map :name pcats)))))
       (println (format " - %s" (:DATE p)))))))


(defn write-pages
  [template posts apath params]
  (let [url (apath/full-url-path apath)]
    (dorun
     (for [[pposts ptab pfname] (paginate posts url)
           ;; File path
           :let [papath (conj apath pfname)]]
       (apath/spit* papath
                    (render template
                            (assoc params
                              :cfg cfg/*cfg*
                              :body (string/join "" (map get-cached-post-part pposts))
                              :tab ptab
                              :feed (apath/full-url-path (feed-apath apath)))))))))

(defn main-categories-html
  [cats]
  (string/join "<br>\n"
               (for [cat cats]
                 (format "<a href='%s'>%s</a>&nbsp;%d"
                         (terablogger.format-html/html-escape (:url cat))
                         (terablogger.format-html/html-escape (:name cat))
                         (count (:files cat))))))

(defn month-link
  [m]
  (format "<a href=\"%s\">%s</a>"
                         (terablogger.format-html/html-escape
                          (apath/full-url-path (apath/archive (conj m ""))))
                         (terablogger.format-html/html-escape
                          (month-text m))))

(defn main-month-links
  [months]
  (string/join "<br>\n"
               (for [m (take (:page-size cfg/*cfg*) (map first months))]
                 (month-link m))))

(defn write-main-pages
  [posts cats months]
  (write-pages (slurp "./blog/templates/main-index.mustache")
               posts
               []
               {:feed (apath/full-url-path (feed-apath []))
                :categories (main-categories-html cats)
                :archive-index (apath/full-url-path (apath/archive ["index.html"]))
                :month-links (main-month-links months)
                :count (count posts)
                :last (:DATE (parse-post cats (first posts)))
                :contacts (format (:contacts cfg/*cfg*)
                                  (:author cfg/*cfg*))
                :calendar nil ; TODO
                :articles nil ; TODO
                :links (slurp "./blog/templates/main-links.mustache")     ; TODO
               }
               ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Atom Feed
;;;

(defn feed-apath
  [apath]
  (conj apath "atom.xml"))

(defn write-feed
  [apath posts]
  (let [apath (feed-apath apath)
        feed-url (apath/full-url-path apath)]
    (apath/spit* apath
                 (render (slurp "./blog/templates/atom.mustache")
                         {:cfg cfg/*cfg*
                          :entries (take (:page-size cfg/*cfg*) posts)
                          :lastmodified (post-ts (:ID (first posts)))
                          :self-url feed-url 
                          }))
    feed-url))

(defn archive-index
  [posts cats months]
  (render (slurp "./blog/templates/all-posts.mustache")
          {:posts posts
           :cats cats
           :months (map (comp (partial hash-map :month) month-link first) months)
           :cfg cfg/*cfg*}))

(defn write-archive-index
  [posts cats months]
  (apath/spit* (apath/archive ["index.html"])
         (render (slurp "./blog/templates/makepage.mustache")
                 {:body (archive-index posts cats months)
                  :feed (apath/full-url-path (feed-apath []))
                  :cfg cfg/*cfg*})))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Categories
;;;
(defn parse-cat
  "Parse category file."
  [file]
  (let [txt (slurp (apath/data-path file))
        [name & files] (string/split-lines txt)
        [_ id] (re-matches #"cat_([0-9]+).db$" file)]
    {:id id
     :url (apath/full-url-path (apath/archive [(str "cat_" id) ""]))
     :name name
     :files files
     :count (count files)
     :set (set files)}))

(defn write-cat
  [cat]
  (let [{id :id
         url :url
         name :name
         posts :files} cat
         cat-apath ["archive" (format "cat_%s" id)]]
    (write-pages (slurp "./blog/templates/category-archive.mustache")
                 posts
                 cat-apath
                 {:cat cat})
    (cfg/with-config (assoc cfg/*cfg*
                       :title (format "%s : %s"
                                      name (:title cfg/*cfg*)))
      (write-feed cat-apath
                  (map *posts* (take (:page-size cfg/*cfg*) posts))))))

(defn write-cats
  [cats]
  (dorun
   (for [cat cats]
     (write-cat cat))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (cfg/with-config (cfg/load-config)
    (binding [*cats* (map parse-cat (list-cats))]
      (let [plist (list-posts)
            posts (map (partial parse-post *cats*)
                       plist)]
        (binding [*posts* (into {} (map #(vector (:ID %) %) posts))]
          (let [m (months (list-posts))]
            (dorun
             (for [post posts]
               (write-post post)))
            ;; Main feed
            (write-feed [] posts)
            ;; Month archive
            (write-months m)
            ;; Category archive
            (write-cats *cats*)
            ;; Archive index
            (write-archive-index posts *cats* m)
            ;; Main page
            (write-main-pages plist *cats* m)
            ;; List recent posts
            (ls-posts (take (:page-size cfg/*cfg*) posts))))))))

