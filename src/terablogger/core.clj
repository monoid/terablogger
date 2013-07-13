(ns terablogger.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [terablogger.cfg :as cfg]
            [terablogger.apath :as apath])
  (:import java.util.Calendar)
  (:use
   [terablogger.templates :only (render tmpl)]
   [terablogger.format-html :only [html-escape]])
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
               (->> idx
                    paginated-filename
                    (str prefix "/")
                    html-escape)
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



(declare feed-apath month-link month-apath)


(defn fmt
  [txt cfg]
  ((resolve (symbol (format "terablogger.format-%s/fmt" (:format cfg))))
   txt cfg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Articles
;;;

(defn article-id
  [filename]
  (subs filename 0 (- (count filename) 4)))

(defn parse-articles
  ([]
     (parse-articles (apath/list-articles)))
  ([articles]
     (for [a articles]
       (with-open [rdr (io/reader (apath/blog-path (apath/articles [a])))]
         (let [[title & b] (line-seq rdr)
               aid (article-id a)
               body (string/join "\n" b)]
          {:id aid
           :html [aid "index.html"]
           :title title
           :body (fmt body cfg/*cfg*)
           })))))

(defn write-article
  [art]
  (let [apath (apath/articles (:html art))]
    (apath/spit* apath
                 (render "makepage"
                         {:cfg cfg/*cfg*
                          :body (:body art)
                          :title (:title art)
                          :feed (apath/full-url-path (feed-apath []))
                          }))))

(defn write-articles
  [articles]
  (dorun
   (for [art articles]
     (write-article art))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Posts
;;;

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
        month (month-apath id)]
    (assoc (parse-headers headers)
      :BODY (fmt (string/join "\n" (butlast (rest (rest body))))
                 cfg/*cfg*)
      :categories categories
      :categories2 (string/join ", " (map :name categories))
      :categories3 (string/join ", "
                                (map #(format "<a href=\"%s\">%s</a>"
                                              (html-escape (:url %))
                                              (html-escape (:name %)))
                                     categories))
      :ID id
      :month month
      :month-link (month-link month)
      :ts (post-ts id)
      :permalink (apath/full-url-path (apath/archive (post-apath id))))))

(defn month-apath
  "Post's month path from id."
  [id]
  (subvec (re-matches #"^(\d+)-(\d+).*" id)
          1))

(defn day-apath
  "Post's month path from id."
  [id]
  (subvec (re-matches #"^(\d+)-(\d+)-(\d+).*" id)
          1))

(defn days
  "Group post by day."
  [posts]
  (group-by day-apath posts))

(defn fmap [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn get-cached-post-part
  "Load part from cache."
  [post-id]
  (slurp (apath/blog-path (apath/cache (post-apath post-id)))))

(defn month-text
  [[y m]]
  (let [sym (java.text.DateFormatSymbols/getInstance)]
    ;; Order: month year.  It is not clear if
    ;; there is a locale-specific way for
    ;; formatting as it is not date, only month
    ;; and year.
    (format "%s %s"
            (get (.getMonths sym)
                 (dec (Integer/parseInt m)))
            y)))

 (defn cal-header [cal sym]
   (let [week-start (.getFirstDayOfWeek cal)
         weekdays (.getShortWeekdays sym)
         week-len 7] ; Is it always so?
     (format
      "<tr>%s</tr>"
      (string/join ""
                   (for [i (range week-len)]
                     (format "<th class=\"calendarday\">%s</th>"
                             (get weekdays (inc (mod (+ i week-start -1)
                                                     week-len)))))))))

 (defn cal-body [month days-list posts-grouped cal]
   (let [week-start (.getFirstDayOfWeek cal)
         fstday (.get cal Calendar/DAY_OF_WEEK)
         week-len 7  ; Is it always so?
         weeks (partition
                week-len
                week-len
                (repeat week-len "")
                (concat
                 (repeat (mod (+ week-start fstday) ; Monday has index
                              week-len)
                         "")
                 days-list))]
     (string/join "\n"
                  (for [week weeks]
                    (format "<tr>%s</tr>"
                            (string/join
                             ""
                             (for [d week]
                               (if-let [posts (get posts-grouped (conj month d))]
                                 (format "<td class=\"calendar\"><a href=\"%s#%s\">%s</a></td>"
                                         ;; TODO: month archive is paginated!!!
                                         (apath/full-url-path (apath/archive (conj month "index.html")))
                                         (first posts)
                                         d)
                                 (if (= "" d)
                                   "<td></td>"
                                   (format "<td class=\"calendar\">%s</td>" d))))))))))

 (defn month-cal [month posts]
   (let [[year mon] month
         ;; Grouped and sorted within each group
         posts-grouped (fmap sort (days posts))
         cal (Calendar/getInstance)  ; TODO: we cannot work with arabic
                                     ; or chinese
         sym (java.text.DateFormatSymbols/getInstance)]
     ;; Setup cal

     (doto cal
        (.clear)
        (.set Calendar/YEAR (Integer/parseInt year))
        (.set Calendar/MONTH (dec (Integer/parseInt mon)))
        (.set Calendar/DAY_OF_MONTH 1))

     (let [days-list (map (partial format "%02d")
                          (range 1 (.getActualMaximum cal
                                                      Calendar/DAY_OF_MONTH)))
           week-start (.getFirstDayOfWeek cal)]
       (format
        "<table><caption>%s</caption>\n%s\n%s</table>"
        (month-text month)
        (cal-header cal sym)
        (cal-body month days-list posts-grouped cal)))))

 (defn months
   "Posts grouped by month-path."
   [posts]
   (into {}
         (for [[month-id posts] (group-by month-apath posts)]
           [month-id {:posts posts
                      :cal (month-cal month-id posts)}])))

(defn write-month
  [[month-id info]]
  (let [posts (:posts info)
        sorted-posts (sort #(compare %2 %1) posts)
        apath (apath/archive (conj month-id "index.html"))]
    (apath/spit* apath
                 (render "month-archive"
                         {:body (string/join "" (map get-cached-post-part sorted-posts))
                          :cfg cfg/*cfg*
                          :feed (apath/full-url-path (feed-apath []))
                          :month (month-text month-id)
                          :calendar (:cal info)
                          }))))

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
          content  (render "entry"
                          {:cfg cfg :entry entry})
          pcontent (render "permalink-entry"
                           {:cfg cfg :entry entry})]
      ;; Cached part
      (apath/spit* capath content)
      ;; Full post page
      (apath/spit* aapath (render "permalink"
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
                         (html-escape (:url cat))
                         (html-escape (:name cat))
                         (count (:files cat))))))

(defn month-link
  [m]
  (format "<a href=\"%s\">%s</a>"
          (html-escape
           (apath/full-url-path (apath/archive (conj m ""))))
          (html-escape
           (month-text m))))

(defn main-month-links
  [months]
  (string/join "<br>\n"
               (for [m (take (:page-size cfg/*cfg*) (map first months))]
                 (month-link m))))

(defn articles-links
  [articles]
  (string/join "<br>\n"
               (for [art articles]
                 (format "<a href=\"%s\">%s</a>"
                         (html-escape
                          (apath/full-url-path (apath/articles (:html art))))
                         (html-escape
                          (:title art))))))

(defn write-main-pages
  [posts cats months articles cal]
  (write-pages "main-index"
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
                :calendar cal
                :articles (articles-links articles)
                :links (tmpl "main-links")
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
                 (render "atom"
                         {:cfg cfg/*cfg*
                          :entries (take (:page-size cfg/*cfg*) posts)
                          :lastmodified (post-ts (:ID (first posts)))
                          :self-url feed-url 
                          }))
    feed-url))

(defn archive-index
  [posts cats months]
  (render "all-posts"
          {:posts posts
           :cats cats
           :months (map (comp (partial hash-map :month) month-link first) months)
           :cfg cfg/*cfg*}))

(defn write-archive-index
  [posts cats months]
  (apath/spit* (apath/archive ["index.html"])
         (render "makepage"
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
        posts (sort #(compare %2 %1) posts)
        cat-apath (apath/archive [(format "cat_%s" id)])]
    (write-pages "category-archive"
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
                       plist)
            articles (parse-articles)
            months1 (months (list-posts))]
        (binding [*posts* (into {} (map #(vector (:ID %) %) posts))]
          (let [m (sort #(compare (nth %2 0) (nth %1 0)) months1)]
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
            ;; Articles
            (write-articles articles)
            ;; Main page
            (write-main-pages plist *cats* m articles (:cal (nth (first m) 1)))
            ;; List recent posts
            (ls-posts (take (:page-size cfg/*cfg*) posts))))))))
