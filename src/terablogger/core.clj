(ns terablogger.core
  (:require [clojure.java.io :as io]
            [clojure.set :refer [difference intersection union]]
            [clojure.string :as string]
            [clojure.tools.cli :refer [cli]]
            [terablogger.cfg :as cfg]
            [terablogger.apath :as apath]
            [terablogger.format-markdown]
            [terablogger.format-textile]
            [terablogger.templates :refer [render render* tmpl]]
            [terablogger.utils :as u]
            [terablogger.format-html :refer [html-escape]])
  (:import java.io.File
           java.util.Calendar
	   java.util.GregorianCalendar
           java.text.DateFormat
           java.text.DateFormatSymbols
           java.text.SimpleDateFormat)
  (:gen-class))

(def ^:dynamic *cats*
  "Parsed categories."
  nil)

(def ^:dynamic *posts*
  "Parsed posts hashmap.  It maps entry ID to delay with Entry."
  nil)

(def list-cats
  "Function that return list of category files."
  (apath/data-lister #"\.db$" compare))

(def list-posts
  "Function that return list of category posts."
  (apath/data-lister (:input-regex cfg/*cfg*) #(compare %2 %1)))

(def list-articles
  "Function that return list of articles."
  (apath/data-lister ["articles"] (:input-regex cfg/*cfg*) compare))

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

(defn href [apath text]
  (format "<a href=\"%s\">%s</a>"
          (html-escape (apath/full-url-path apath))
          (html-escape text)))


(declare feed-apath month-link month-apath)

(defn file-format
  "File format based on file extension."
  [p]
  (let [ext (second (re-matches #".*\.([^.]*)$" p))]
    (if (= ext "txt")
      "html"
      ext)))

(defn fmt
  "Format txt of type `type`."
  [txt type cfg]
  ((resolve (symbol (format "terablogger.format-%s/fmt" type)))
   txt cfg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Articles
;;;

(defn article-id
  [filename]
  (second (re-matches #"([^.]*)(\.[^.]+)?" filename)))

(defn parse-articles
  ([]
     (parse-articles (list-articles)))
  ([articles]
     (for [a articles]
       (with-open [rdr (io/reader (apath/blog-path (apath/articles [a])))]
         (let [[title & b] (line-seq rdr)
               aid (article-id a)
               body (string/join "\n" b)]
           {:id aid
            :html [aid "index.html"]
            :title title
            :body (fmt body (file-format a) cfg/*cfg*)
            })))))

(defn write-article
  [art]
  (let [apath (apath/articles (:html art))]
    (render* {:cfg cfg/*cfg*
              :body (:body art)
              :title? true
              :title (:title art)
              :feed (apath/full-url-path (feed-apath []))
              }
             "makepage"
             apath)))

(defn write-articles
  [articles]
  (doseq [art articles]
    (write-article art)))


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
  [id]
  (conj (subvec (re-matches #"^(\d+)-(\d+)-(\d+)(T[0-9_]+).*" id)
                1)
        "index.html"))

(defn post-htmlid
  "Id that is used for HTML article id."
  [id]
  (str "e" id))

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


(defn posts-cats
  "Sequence of categories that post ids belongs to."
  [post-ids]
  (let [post-ids-set (set post-ids)]
    (for [c *cats*
          :when (seq (intersection post-ids-set (:set c)))]
      c)))

(defn post-cats
  "Categories that article belongs to."
  [id]
  (filter #((:set %) id)   ; Check if article id is in set of posts.
          *cats*))

(defrecord Post
    [TITLE
     AUTHOR
     DATE
     DESC
     BODY
     ID])

(defrecord Entry
    [TITLE
     AUTHOR
     DATE
     DESC
     BODY
     ID
     categories
     categories2
     categories3
     categories?
     hid
     month
     month-link
     ts
     permalink])

(defn extend-post
  "Convert Post to Entry, adding derived field values used in templates."
  [post]
  (let [id (:ID post)
        categories (post-cats id)
        month (month-apath id)]
    (map->Entry
     (assoc post
       :categories categories
       :categories2 (string/join ", " (map :name categories))
       :categories3 (string/join ", "
                                 (map #(href (:apath %)
                                             (:name %))
                                      categories))
       :categories? (boolean (seq categories))
       ;; HTML id starts with letter; we add 'e' for compatibility
       ;; with nanoblogger.
       :hid (post-htmlid id)
       :month month
       :month-link (month-link month)
       :ts (post-ts id)
       :permalink (apath/full-url-path (apath/archive (post-apath id)))))))

(defn parse-post
  "Parse blog post."
  [id]
  (let [txt (slurp (apath/data-path id))
        lines (string/split-lines txt)
        [headers body] (split-with #(not (re-seq #"^-----$" %)) lines)]
    (->>
     (assoc (parse-headers headers)
       :ID id
       :BODY (fmt (string/join "\n" (butlast (rest (rest body))))
                  (file-format id)
                  cfg/*cfg*))
     map->Post
     extend-post)))

(defn post-id-by-num
  "Convert post ID (1-based) from command line to real ID (timestamp)."
  [plist num]
  (nth plist (dec num)))

(defn get-post-map
  "Map of post ID to delay with Entry for *posts* dynamic."
  []
  (let [plist (list-posts)
        posts (map #(delay (parse-post %)) plist)]
    (zipmap plist posts)))

(defmacro with-posts
  "Bind *posts* to value of (get-post-map) and execute body."
  [& body]
  `(binding [*posts* (get-post-map)]
     ~@body))

(defn post-by-id
  "Find entry by id."
  [id]
  (force (*posts* id)))


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
  "Group posts by day."
  [posts]
  (group-by day-apath posts))


(declare write-post)

(defn get-cached-post-part
  "Load part from cache."
  [post-id]
  (let [filename (-> post-id
                     post-apath
                     apath/cache
                     apath/blog-path)]
    (when-not (.exists (io/file filename))
      (write-post (post-by-id post-id)))
    (slurp filename)))

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
        week-len 7]                     ; Is it always so?
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
        week-len 7                      ; Is it always so?
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
                                        (apath/full-url-path (apath/archive (conj month "index.html")))
                                        (post-htmlid (first posts))
                                        d)
                                (if (= "" d)
                                  "<td></td>"
                                  (format "<td class=\"calendar\">%s</td>" d))))))))))

(defn month-cal [month posts]
  (let [[year mon] month
        ;; Grouped and sorted within each group
        posts-grouped (u/fmap u/sort* (days posts))
        cal (GregorianCalendar.)
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

(defn group-by-months
  "Posts grouped by month-path (hash month-id ->
 {:posts posts :cal calendar})."
  [posts]
  (for [[month-id posts] (group-by month-apath posts)]
    [month-id {:posts posts
               :cal (month-cal month-id posts)}]))

(defn sorted-months
  "Posts grouped by month-path and sorted (sequence)."
  [posts]
  (sort #(compare (nth %2 0) (nth %1 0))
        (group-by-months posts)))

(defn posts-month-ids
  "Set of month IDs for group of post IDs."
  [post-ids]
  (set (map month-apath post-ids)))

(defn sorted-months-subset
  "Subset of months for group of post IDs"
  [post-ids months]
  (let [pm (posts-month-ids post-ids)]
    (filter (comp pm first) months)))

(defn write-month
  [[month-id info]]
  (let [posts (:posts info)
        sorted-posts (u/sort* posts)
        apath (apath/archive (conj month-id "index.html"))]
    (render* {:body (string/join "" (map get-cached-post-part sorted-posts))
              :cfg cfg/*cfg*
              :feed (apath/full-url-path (feed-apath []))
              :month (month-text month-id)
              :calendar (:cal info)
              }
             "month-archive"
             apath)))

(defn write-months
  [months]
  (doseq [month months]
    (write-month month)))

(defn save-post
  "Write post data."
  [post]
  (with-open [wrtr (io/writer (apath/data-path (:ID post)))]
    ;; Header
    (doseq [field '(:TITLE :AUTHOR :DATE :DESC)]
      (.write wrtr (format "%s: %s\n" (name field) (field post))))
    ;; Body
    (doto wrtr
      (.write "-----\nBODY:\n")
      (.write (:BODY post))
      (.write "\n-----\n"))))

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
    (render* {:body pcontent
              :cfg cfg/*cfg*
              :title (:TITLE entry)
              :feed (apath/full-url-path (feed-apath []))}
             "permalink"
             aapath)))

(defn ls-posts
  [posts]
  (doseq [[p n] (map list posts (rest (range)))]
    (let [pcats (:categories p)]
      (print (format "%d. %s"
                     n
                     (u/truncatechars (:TITLE p) 32)))
      (when (seq pcats)
        (print (format " - [%s]" (string/join ", " (map :name pcats)))))
      (println (format " - %s" (:DATE p))))))


(defn write-pages
  [template posts apath params]
  (let [url (apath/full-url-path apath)]
    (doseq [[pposts ptab pfname] (paginate posts url)
            ;; File path
            :let [papath (conj apath pfname)]]
      (render* (assoc params
                 :cfg cfg/*cfg*
                 :body (string/join "" (map get-cached-post-part pposts))
                 :tab ptab
                 :feed (apath/full-url-path (feed-apath apath)))
               template
               papath))))

(defn main-categories-html
  []
  (->> *cats*
       (map #(format "%s&nbsp;%d"
                     (href (:apath %) (:name %))
                     (count (:files %))))
       (string/join "<br>\n")))

(defn month-link
  [m]
  (href (apath/archive (conj m "")) (month-text m)))


(defn main-month-links
  [months]
  (->> months
       (map (comp month-link first))
       (take (:page-size cfg/*cfg*))
       (string/join "<br>\n")))

(defn article-link [art]
  (href (apath/articles (:html art))
        (:title art)))

(defn articles-links
  "HTML string of <br>-separated article links."
  [articles]
  (->> articles
       (map article-link)
       (string/join "<br>\n")))

(defn write-main-pages
  [posts months articles]
  (let [cal (:cal (nth (first months) 1))]
    (write-pages "main-index"
                 posts
                 []
                 {:feed (apath/full-url-path (feed-apath []))
                  :categories (main-categories-html)
                  :archive-index (apath/full-url-path (apath/archive ["index.html"]))
                  :month-links (main-month-links months)
                  :count (count posts)
                  :last (:DATE (parse-post (first posts)))
                  :contacts (format (:contacts cfg/*cfg*)
                                    (:author cfg/*cfg*))
                  :calendar cal
                  :articles (articles-links articles)
                  :links (tmpl "main-links")})))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Atom Feed
;;;

(defn feed-apath
  [apath]
  (conj apath "atom.xml"))

(defn write-feed
  [apath post-ids]
  (let [apath (feed-apath apath)
        feed-url (apath/full-url-path apath)
        posts (map post-by-id post-ids)]
    (render* {:cfg cfg/*cfg*
              :entries (take (:page-size cfg/*cfg*) posts)
              :lastmodified (post-ts (:ID (first posts)))
              :self-url feed-url}
             "atom"
             apath)
    feed-url))

(defn archive-index
  [plist months]
  (render "all-posts"
          {:posts (map post-by-id plist)
           :cats *cats*
           :months (map (comp (partial hash-map :month) month-link first)
                        months)
           :cfg cfg/*cfg*}))

(defn write-archive-index
  [plist months]
  (render* {:body (archive-index plist months)
            :feed (apath/full-url-path (feed-apath []))
            :title? false
            :cfg cfg/*cfg*}
           "makepage"
           (apath/archive ["index.html"])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Categories
;;;

(defrecord Category
    [id
     apath
     name
     file
     files
     count
     set])

(defn parse-cat
  "Parse category file."
  [file txt]
  (let [[name & files] (string/split-lines txt)
        [_ id] (re-matches #"cat_([0-9]+).db$" file)]
    (map->Category
     {:id id
      :apath (apath/archive [(str "cat_" id) ""])
      :file file
      :name name
      :files files
      :count (count files)
      :set (set files)})))

(defn read-cat
  "Load category data from file."
  [file]
  (let [txt (slurp (apath/data-path file))]
    (parse-cat file txt)))

(defn read-cats
  []
  (map read-cat (list-cats)))

(defmacro with-cats
  [& body]
  `(binding [*cats* (read-cats)]
     ~@body))

(defn save-cat
  "Save category data to text file."
  [cat]
  (with-open [wrtr (io/writer (apath/data-path (:file cat)))]
    ;; First line: category title
    (.write wrtr (:name cat))
    (.write wrtr "\n")
    ;; Rest of file is list of posts
    (doseq [fid (:files cat)]
      (doto wrtr
        (.write fid)
        (.write "\n")))))

(defn write-cat
  [cat]
  (let [{id :id
         name :name
         posts :files} cat
         posts (u/sort* posts)
         cat-apath (apath/archive [(format "cat_%s" id)])]
    (write-pages "category-archive"
                 posts
                 cat-apath
                 {:cat cat})
    (cfg/with-config (assoc cfg/*cfg*
                       :title (format "%s : %s"
                                      name (:title cfg/*cfg*)))
      (write-feed cat-apath
                  (take (:page-size cfg/*cfg*) posts)))))

(defn write-cats
  [cats]
  (doseq [cat cats]
    (write-cat cat)))

(defn add-post-to-cat
  [post-id cat]
  (let [new-set (conj (:set cat) post-id)]
    (assoc cat
      :set new-set
      :count (count new-set)
      :files (sort new-set))))

(defn del-posts-from-cat
  [post-ids cat]
  (let [new-set (difference (:set cat) (set post-ids))]
    (assoc cat
      :set new-set
      :count (count new-set)
      :files (sort new-set))))

(defn find-cat-by-id
  "Resolve cat ID into Category."
  [cat-id]
  (first (filter #(= (str cat-id) (:id %))
                 *cats*)))

(defn regen-posts-with-deps
  "Regenerate HTML for post IDs and associated archives and main page.
If post-ids is nil, regenerate everything."
  ([post-ids]
     (regen-posts-with-deps post-ids false))
  ([post-ids articles-p]
     (let [plist (list-posts)
           months (sorted-months plist)
           posts-months (if (nil? post-ids)
                          months
                          (sorted-months-subset post-ids months))
           cats (if (nil? post-ids)
                  *cats*
                  (posts-cats post-ids))
           post-ids (if (nil? post-ids) plist post-ids)
           articles (parse-articles)]
       ;; Posts and their's parts
       (doseq [p post-ids]
         (write-post (post-by-id p)))
       ;; Categories
       (write-cats cats)
       ;; Months
       (write-months posts-months)
       ;; Feed
       (write-feed [] plist)
       ;; Archive
       (write-archive-index plist months)
       ;; Articles
       (when articles-p
         (write-articles articles))
       ;; Main
       (write-main-pages plist months articles))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Operations
;;

(defn options-cats
  "Resolve comma-separated list of category IDs into seq of Category objects."
  [options]
  (map find-cat-by-id (u/split* (:cat options) #",")))

(defn read-post-body
  []
  (let [tmpfile (File/createTempFile "post-" ".txt")]
    (try
      ;; Open text editor with new file
      (u/exec-editor tmpfile)
      ;; Read body
      (slurp tmpfile)

      (finally
        (.delete tmpfile)))))

(defn add-post
  "Add post."
  [options]
  (let [ts (java.util.Date.)
        title (or
               (:title options)
               (u/ask-user "Title"))
        desc (or
              (:desc options)
              (u/ask-user "Description"))
        author (or (:author options)
                   (u/ask-user "Author" (:author cfg/*cfg*)))
        post-id (.format (SimpleDateFormat. "yyyy-MM-dd'T'HH_mm_ss'.txt'") ts)
        date    (.format (DateFormat/getDateTimeInstance
                          DateFormat/LONG
                          DateFormat/LONG)
                         ts)
        body (read-post-body)
        post (map->Post
              {:ID post-id
               :TITLE title
               :AUTHOR author
               :DATE date
               :DESC desc
               :BODY body})]

    (save-post post)
    ;; Update categories
    (let [cats (set (options-cats options))
          cat-ids (set (map :id cats))
          cats* (map (fn [cat]
                       (if (contains? cats cat)
                         (add-post-to-cat post-id cat)
                         cat))
                     *cats*)]
      (binding [*cats* cats*]
        ;; Save data
        ;; 1. Post
        (write-post (extend-post post))
        ;; 2. Categories
        (doseq [cat *cats*
                :when (cat-ids (:id cat))]
          (save-cat cat))
        ;; Regenerate HTML
        (with-posts
          (regen-posts-with-deps [post-id]))
        *cats*))))

(defn category-next-id
  []
  (str
   (inc (reduce max 0
                (map #(Integer/parseInt (:id %))
                     *cats*)))))

(defn add-cat
  "Add category."
  [options]
  (let [title (or
               (:title options)
               (u/ask-user "Category title"))
        new-id (category-next-id)
        file (str "cat_" new-id)]
    (save-cat
     (map->Category {:id new-id
                     :apath (apath/archive [file ""])
                     :file file
                     :name title
                     :files []
                     :count 0
                     :set #{}}))))

(defn delete-post
  "Delete post."
  [options]
  (try
    (with-cats
      (with-posts
        (let [plist (list-posts)
              post-nums (map #(Integer. %)
                             (u/split* (:delete options) #","))
              post-ids (set (remove nil?
                                    (map (partial post-id-by-num plist)
                                         post-nums)))
              posts (map post-by-id post-ids)]
          ;; Remove files
          (doseq [p posts]
            (do
              ;; Data
              (-> (:ID p)
                  apath/data-path
                  (io/delete-file true))
              ;; Cache
              (-> (:ID p)
                  post-apath
                  apath/cache
                  apath/blog-path
                  (io/delete-file true))))

          ;; Filter out from post db
          (binding [*posts* (apply dissoc *posts* post-ids)]
            ;; We collect only ids because category objects will be
            ;; updated later.
            (let [cat-ids-to-upd (map :id
                                      (filter #(->> %
                                                    :set
                                                    (intersection post-ids)
                                                    seq)
                                              *cats*))
                  plist (list-posts)
                  articles (list-articles)]
              (binding [*cats* (map (partial del-posts-from-cat post-nums)
                                    *cats*)]
                (doseq [c cat-ids-to-upd]
                  (let [cat (find-cat-by-id c)]
                    (save-cat cat)))
                (regen-posts-with-deps post-ids)))))))
    (catch NumberFormatException e
      (throw (ex-info "Illegal post number." {})))))

(defn delete-cats
  "Delete categories."
  [options]
  (with-cats
    (let [cat-ids (u/split* (:cat options) #",")
          cats (set (map find-cat-by-id cat-ids))
          posts (reduce union {} (map :set cats))]
      ;; Remove db files
      (doseq [cat cats]
        (-> (:file cat)
            apath/data-path
            (io/delete-file true)))
      ;; Regenerate HTML
      (binding [*cats* (remove (partial contains? cats)
                               *cats*)]
        (with-posts
          (regen-posts-with-deps posts)
          *cats*)))))


(defn edit-post
  "Edit post."
  [options]
  (try
    (let [plist (list-posts)
          post-num (Integer. (:edit options))
          post-id (post-id-by-num plist post-num)
          post-file (apath/data-path post-id)]
      (u/exec-editor post-file)
      (with-cats
        (with-posts
          (regen-posts-with-deps [post-id]))))
    (catch NumberFormatException e
      (throw (ex-info "Illegal post number." {})))))

(defn edit-cat
  "Edit category."
  [options]
  (with-cats
    (if-let [cat (find-cat-by-id (:cat options))]
      (let [new-title (or (:title options)
                          (u/ask-user "Category title"))
            new-cat (assoc cat :name new-title)]
        (binding [*cats* (map #(if (= (:id %) (:id cat))
                                 new-cat
                                 %)
                              *cats*)]
          (save-cat new-cat)

          (with-posts
            (regen-posts-with-deps (:files new-cat)))))
      (throw (ex-info "Category not found." {})))))


(defn move-post
  "Move post to categories.
Remove from old, add to new, regenerate everything."
  [options]
  (try
    (with-cats
      (let [plist (list-posts)
            post-num (Integer. (:move options))
            post-id (post-id-by-num plist post-num)
            old-cats (post-cats post-id *cats*)
            old-cats* (set (map :id old-cats))
            cats (options-cats options)
            cats* (set (map :id cats))
            removed (difference old-cats* cats*)
            added (difference cats* old-cats*)
            to-regen (union cats* old-cats*)]
        (binding [*cats* (map #(cond
                                (removed (:id %))
                                (del-posts-from-cat [post-id] %)
                                (added (:id %))
                                (add-post-to-cat post-id %)
                                :else %)
                              *cats*)]
          (with-posts
            (doseq [c *cats*
                    :when (to-regen (:id c))]
              (do
                (when (or (removed (:id c))
                          (added (:id c)))
                  (save-cat c))
                (write-cat c)))
            (regen-posts-with-deps [post-id])))))
    (catch NumberFormatException e
      (throw (ex-info "Illegal post number." {})))))

(defn command-add
  "Handle --add option."
  [options]
  (with-cats
    (if (= "new" (:cat options))
      (add-cat options)
      (add-post options))))

(defn command-del
  "Handle --delete option."
  [options]
  (if (= "cat" (:delete options))
    (delete-cats options)
    (delete-post options)))

(defn command-edit
  "Handle --edit option."
  [options]
  (if (= "cat" (:edit options))
    (edit-cat options)
    (edit-post options)))

(defn command-move
  "Handle --move <ID> command line option."
  [options]
  (move-post options))

(defn command-update
  [options]
  (with-cats
    (with-posts
      (case (:update options)
        "all"
        (regen-posts-with-deps nil true)
        "articles"
        (regen-posts-with-deps [] true)
        "current"
        (regen-posts-with-deps (take 1 (list-posts)))
        "main"
        (regen-posts-with-deps (take (:page-size cfg/*cfg*)
                                     (list-posts)))
        ;; Otherwise
        (throw (ex-info "Unknown --update value." {}))))))

(defn command-list
  "Handle --list <all,cat,current> command line option."
  [options]
  (with-cats
    (case (:list options)
      ;; We also accept "last" and "new".  Undocumented feature :)
      ("current" "last" "new" nil)
      (let [plist (list-posts)
            posts (map parse-post plist)]
        (ls-posts (take (:page-size cfg/*cfg*) posts)))

      "all"
      (let [plist (list-posts)
            posts (map parse-post plist)]
        (ls-posts posts))

      "cat"
      ;; List categories
      (doseq [c *cats*]
        (println (format "%s. %s (%d)" (:id c) (:name c) (:count c))))
      ;; Default
      (throw (ex-info "Unknown --list value." {})))))


(def ^:const CLI-OPTIONS
  [
   ;; Operations

   ;; Declaring --add as a boolean is kludge as tools.cli has no
   ;; other means to provide option without value. --no-add is just useless...
   ;; TODO: another option parsing library?
   ["-a" "--add" "Add post (default) or category (with -c new)." :flag true]
   ["-d" "--delete" "<ID,cat> Delete post or category (-d ID, -d cat)"]
   ["-e" "--edit" "<ID,cat>Edit article or category (-e ID, -e cat)."]
   ["-m" "--move" "<ID> Move post to other categories."]
   ["-u" "--update" "<all,articles,current,main> Update blog (regenerate HTML)."]
   ["-l" "--list" "<all,cat,current> List posts."]
   ["-h" "--help" "Print help."]
   ;; Category
   ["-c" "--cat" "Category id, or comma-separted list, or 'new' for addition."]
   ;; Parameters for post creation:
   ["-n" "--author" "<text> Set entry's author."]
   ["-D" "--desc"   "<text> Set entry's description."]
   ["-t" "--title" "<text> Set entry's or category's title."]])


(defn command-number
  [params]
  (+ (count
      (intersection (set (keys params))
                    #{:delete :edit :move :update :list}))
     ;; :add is special case as it is a boolean flag (KLUDGE)
     (if (:add params)
       1
       0)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))

  (try
    (let [[options ignored banner] (apply cli args CLI-OPTIONS)
          has-cat (contains? options :cat)
          cmd-num (command-number options)]
      (cfg/with-config (cfg/load-config options)
        (cond
         ;; Help is first because it executed when there are several options
         ;; are passed by mistake.
         (or (> cmd-num 1)
             (contains? options :help))
           (u/println* *err* banner)
         (or (= cmd-num 0)
             (contains? options :list))
           (command-list options)
         (:add options)
           (command-add options)
         (contains? options :delete)
           (command-del options)
         (contains? options :edit)
           (command-edit options)
         (contains? options :move)
           (command-move options)
         (contains? options :update)
           (command-update options)
         :else (throw (ex-info "Not implemented yet." {})))))
    (catch clojure.lang.ExceptionInfo e
      (u/println* *err* "Error:" (.getMessage e)))))
