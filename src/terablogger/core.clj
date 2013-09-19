(ns terablogger.core
  (:require [clojure.java.io :as io]
            [clojure.set :refer [intersection]]
            [clojure.string :as string]
            [clojure.tools.cli :refer [cli]]
            [terablogger.cfg :as cfg]
            [terablogger.apath :as apath]
            [terablogger.format-markdown]
            [terablogger.format-textile]
            [terablogger.templates :refer [render tmpl]]
            [terablogger.format-html :refer [html-escape]])
  (:import java.io.File
           java.util.Calendar
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

(defn truncatechars
  "If msg's length exeeds n, truncate it, appending '...'. "
  [msg n]
  (if (> (count msg) n)
    (-> msg
        (subs 0 (- n 3))
        (string/trimr)  ; Trim whitespaces at end for better appearance
        (str "..."))
    msg))

(defn sort*
  "Sort in reverse order."
  ([seq]
     (sort #(compare %2 %1) seq))
  ([cmp seq]
     (sort #(cmp %2 %1) seq)))

(defn split*
  "Split string with regexp.  If first argument is nil or empty string,
return []."
  [str-or-nil regex]
  (if (seq str-or-nil)
    (string/split str-or-nil regex)
    []))

(defn ask-user
  ([prompt]
     (print (format "%s: " prompt))
     (flush)
     (read-line))
  ([prompt default]
     (print (format "%s [%s]: " prompt default))
     (flush)
     (let [resp (read-line)]
       (if (string/blank? resp)
         default
         resp))))

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
    (apath/spit* apath
                 (render "makepage"
                         {:cfg cfg/*cfg*
                          :body (:body art)
                          :title? true
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

(defn post-htmlid [id]
  "Id that is used for HTML article id."
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

(defn post-cats
  "Categories that article belongs to."
  [id cats]
  (sort #(compare (:id %1) (:id %2))  ; Order by category numerical id.
        (filter #((:set %) id)   ; Check if article id is in set of posts.
                cats)))

(defrecord Post
  [TITLE
   AUTHOR
   DATE
   DESC
   BODY
   categories
   categories2
   categories3
   ID
   hid
   month
   month-link
   ts
   permalink])

(defn parse-post
  "Parse blog post."
  [cats id]
  (let [txt (slurp (apath/data-path id))
        lines (string/split-lines txt)
        [headers body] (split-with #(not (re-seq #"^-----$" %)) lines)
        categories (post-cats id cats)
        month (month-apath id)]
    (map->Post
     (assoc (parse-headers headers)
       :BODY (fmt (string/join "\n" (butlast (rest (rest body))))
                  (file-format id)
                  cfg/*cfg*)
       :categories categories
       :categories2 (string/join ", " (map :name categories))
       :categories3 (string/join ", "
                                 (map #(href (:apath %)
                                             (:name %))
                                      categories))
       :ID id
       ;; HTML id starts with letter; we add 'e' for compatibility
       ;; with nanoblogger.
       :hid (post-htmlid id)
       :month month
       :month-link (month-link month)
       :ts (post-ts id)
       :permalink (apath/full-url-path (apath/archive (post-apath id)))))))


(defn get-post-map
  "Map of post ID to delay with Entry for *posts* dynamic."
  []
  (let [plist (list-posts)
        posts (map #(delay (parse-post *cats* plist)))]
    (zipmap plist posts)))

(defmacro with-posts
  [& body]
  `(binding [*posts* (get-post-map)]
     ~@body))


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
                                         (apath/full-url-path (apath/archive (conj month "index.html")))
                                         (post-htmlid (first posts))
                                         d)
                                 (if (= "" d)
                                   "<td></td>"
                                   (format "<td class=\"calendar\">%s</td>" d))))))))))

 (defn month-cal [month posts]
   (let [[year mon] month
         ;; Grouped and sorted within each group
         posts-grouped (fmap sort* (days posts))
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
        sorted-posts (sort* posts)
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

(defn save-post
  "Write post data."
  [post]
  (with-open [wrtr (io/writer (apath/data-path (:ID post)))]
    ;; Header
    (dorun
     (for [field '(:TITLE :AUTHOR :DATE :DESC)]
       (.write wrtr (format "%s: %s\n" field (field post)))))
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
  (->> cats
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
    (->> {:cfg cfg/*cfg*
          :entries (take (:page-size cfg/*cfg*) posts)
          :lastmodified (post-ts (:ID (first posts)))
          :self-url feed-url}
         (render "atom")
         (apath/spit* apath))
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
                  :title? false
                  :cfg cfg/*cfg*})))


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

(defn save-cat
  "Save category data to text file."
  [cat]
  (with-open [wrtr (io/writer (apath/data-path (:file cat)))]
    ;; First line: category title
    (.write wrtr (:name cat))
    (.write wrtr "\n")
    ;; Rest of file is list of posts
    (dorun
     (for [fid (:files cat)]
       (doto wrtr
         (.write fid)
         (.write "\n"))))))

(defn write-cat
  [cat]
  (let [{id :id
         name :name
         posts :files} cat
        posts (sort* posts)
        cat-apath (apath/archive [(format "cat_%s" id)])]
    (write-pages "category-archive"
                 posts
                 cat-apath
                 {:cat cat})
    (cfg/with-config (assoc cfg/*cfg*
                       :title (format "%s : %s"
                                      name (:title cfg/*cfg*)))
      (write-feed cat-apath
                  (map (comp force *posts*)
                       (take (:page-size cfg/*cfg*) posts))))))

(defn write-cats
  [cats]
  (dorun
   (for [cat cats]
     (write-cat cat))))

(defn add-post-to-cat
  [post-id cat]
  (let [new-set (conj (:set cat) post-id)]
    (assoc cat
      :set new-set
      :count (count new-set)
      :files (sort new-set))))

(defn del-post-from-cat
  [post-id cat]
  (let [new-set (disj (:set cat) post-id)]
    (assoc cat
      :set new-set
      :count (count new-set)
      :files (sort new-set))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Operations
;;

(defn read-post-body
  []
  (let [tmpfile (File/createTempFile "post-" ".txt")]
    ;; Open text editor with new file
    (-> (Runtime/getRuntime)
         (.exec (->> tmpfile
                     .getPath
                     (conj (:editor cfg/*cfg*))
                     into-array))
         (.waitFor))
    ;; Read body
    (let [body (slurp tmpfile)]
      (.delete tmpfile)
      body)))

(defn add-post
  "Add post."
  [options]
  (let [ts (java.util.Date.)
        title (or
               (:title options)
               (ask-user "Title"))
        desc (or
              (:desc options)
              (ask-user "Description"))
        author (or (:author options)
                   (ask-user "Author" (:author cfg/*cfg*)))
        post-id (.format (SimpleDateFormat. "yyyy-MM-dd'T'HH-mm-ss'.txt'") ts)
        date    (.format (DateFormat/getDateTimeInstance
                          DateFormat/LONG
                          DateFormat/LONG)
                         ts)
        body (read-post-body)]

    (save-post (map->Post
                {:ID post-id
                 :TITLE title
                 :AUTHOR author
                 :DATE date
                 :DESC desc
                 :BODY body}))
    ;; Update categories

    

    ;; Regenerate HTML
    ;; 1. Post
    ;; 2. Month
    ;; 3. Main
    ;; 4. Categories
    ))


(defn category-next-id
  [cats]
  ;; TODO FIXME STUB
  "100")

(defn add-cat
  "Add category."
  [options]
  (let [title (or
               (:title options)
               (ask-user "Category title"))
        new-id (category-next-id *cats*)
        file (str "cat_" new-id)]
    (save-cat
     (map->Category {:id new-id
                     :apath (apath/archive [file ""])
                     :file file
                     :name title
                     :files []
                     :count 0
                     :set #{}}))))

(defn del-post
  "Delete post."
  []
  (throw (ex-info "Not implemented.")))

(defn del-cat
  "Delete category."
  []
  (throw (ex-info "Not implemented.")))


(defn edit-post
  "Edit post."
  []
  (throw (ex-info "Not implemented.")))

(defn edit-cat
  "Edit post."
  []
  (throw (ex-info "Not implemented.")))


(defn move-post
  "Move post to categories.
Remove from old, add to new, regenerate everything."
  []
  (throw (ex-info "Not implemented.")))

(defn command-add
  "Handle --add option."
  [options]
  (if (= "new" (:cat options))
    (add-cat options)
    (add-post options)))

(defn find-cat-by-id
  "Resolve cat ID into Category."
  [cat-id]
  (first (filter #(= (str cat-id) (:id %))
                 *cats*)))

(defn options-cats
  "Resolve comma-separated list of category IDs into seq of Category objects."
  [options]
  (map find-cat-by-id (split* (:cat options) #",")))

(defn command-list
  "Handle --list <all,cat,current> command line option."
  [options]
  (binding [*cats* (map read-cat (list-cats))]
    (case (:list options)
      ;; We also accept "last" and "new".  Undocumented feature :)
      ("current" "last" "new" nil)
      (let [plist (list-posts)
            posts (map (partial parse-post *cats*)
                       plist)]
        (ls-posts (take (:page-size cfg/*cfg*) posts)))

      "all"
      (let [plist (list-posts)
            posts (map (partial parse-post *cats*)
                       plist)]
        (ls-posts posts))

      "cat"
      (let [cat (first (options-cats options))]
        (when cat
          (let [plist (sort* (:set cat))
                posts (map (partial parse-post *cats*)
                           plist)]
            (ls-posts (take (:page-size cfg/*cfg*) posts)))))

      ;; Default:
      ;; TODO: throw an exception?
      nil)))


(def ^:const CLI-OPTIONS
  [
   ;; Operations

   ;; Declareing --add as a boolean is kludge as tools.cli has no
   ;; other means to provide option without value. --no-add is just useless...
   ;; TODO: another option parsing library?
   ["-a" "--add" "Add post (default) or category (with -c new)." :flag true]
   ["-d" "--delete" "<ID,cat> Delete post or category (-d ID, -d cat)"]
   ["-e" "--edit" "<ID,cat>Edit article or categori (-e ID, -e cat)."]
   ["-m" "--move" "<ID> Move post to another categories."]
   ["-u" "--update" "<all,current,main> Update blog (regenerate HTML)."]
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
  (count
   (intersection (set (keys params))
                 (set [:add :delete :edit :move :update :list]))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))

  (let [[options ignored banner] (apply cli args CLI-OPTIONS)
        has-cat (contains? options :cat)
        cmd-num (command-number options)
        is-command-add  (:add options)
        is-command-help (or (> cmd-num 2) ; There's always :add
                            (contains? options :help))
        is-command-list (or (= cmd-num 1) ; There's always :add
                            (contains? options :list))]
    (cfg/with-config (cfg/load-config options)
      (cond
       is-command-add
          (command-add options)
       is-command-help
          (println banner)
       is-command-list
          (command-list options)
       true
          (println "Not implemented yet."))))

  (comment
    (cfg/with-config (cfg/load-config)
      (binding [*cats* (map read-cat (list-cats))]
        (let [
              plist (list-posts)
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
              (ls-posts (take (:page-size cfg/*cfg*) posts)))))))))
