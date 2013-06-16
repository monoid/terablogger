(ns terablogger.core
  (:import java.io.File)
  (:use clostache.parser)
  (:require [clojure.string :as string]
            [clojure.java.io :as io])
  (:gen-class))

(def PAGE-SIZE
  "Default page size"
  10)

(defn default-config []
  "Return default config."
  {:blog-dir (or (System/getenv "BLOG_DIR")
                 "./blog")
   :page-size PAGE-SIZE
   :url "http://example-blog.com/blog/"
   :domain "example-blog.com"
   :path "/blog/"
   :style "styles/tb_clean.css"
   :lang "en"
   :title "Example blog"
   :description "Long blog description."
   :author "George W. Bush Jr."
   :permalink true
   ;; Input format
   :format "html" ; the only supported
   :html-auto-break true})


(def ^:dynamic *cfg*
  "Config."
  (default-config))

(def ^:dynamic *blog-dir*
  "Blog's base dir."
  (:blog-dir *cfg*))

(def ^:dynamic *cats*
  "Parsed categories.")

(def ^:dynamic *posts*
  "Parsed posts hash.")

(defmacro with-config
  "Bind *cfg*, *blog-dir* and *data-dir* to values in cfg and execute body."
  [cfg & body]
  `(binding [*cfg* ~cfg]
     (binding [*blog-dir* (:blog-dir *cfg*)]
       ~@body)))

(defn url-path
  [apath]
  (string/join "/" apath))

(defn blog-path
  "Path for blog file."
  [& filenames]
  (string/join File/separator (cons *blog-dir* filenames)))

(defn blog-file
  "java.io.File for blog file."
  [& filenames]
  (File. (apply blog-path filenames)))


(defn data-path
  "Path for data file."
  [& filenames]
  (string/join  File/separator (list* *blog-dir* "data" filenames)))

(defn data-file
  "java.io.File for data file."
  [& filenames]
  (File. (apply data-path filenames)))

(defn archive-path [& filenames]
  (string/join File/separator (list* *blog-dir* "archive" filenames)))

(defn cache-path [& filenames]
  (string/join File/separator (list* *blog-dir* "parts" filenames)))

(defn data-lister
  "List files in data dir that match regex"
  [regex cmp]
  (fn []
    (let [dir (blog-file "data")]
      (sort cmp
            (filter (partial re-seq regex)
                    (map (memfn getName)
                         (file-seq dir)))))))

(def list-cats
  "Function that return list of category files."
  (data-lister #"\.db$" compare))

(def list-posts
  "Function that return list of category posts."
  (data-lister #"\.txt$" #(compare %2 %1)))

(defn spit*
  "Spit data into path, ensuring its parents exists."
  [path data]
  (io/make-parents path)
  (spit path data))

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
        pages   (partition (:page-size *cfg*) posts)
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

(declare feed-apath)

(defn parse-headers
  "Parse post's headers, returning a hash."
  [headers]
  (into {}
         (map #(let [[key val] (string/split % #": " 2)]
                    [(keyword key) val])
                 headers)))

(defn post-path
  ([id]
     (post-path "/" id))
  ([sep id]
     (str (string/join sep (rest (re-matches #"^(\d+)-(\d+)-(\d+)(T[0-9_]+).*" id)))
          sep "index.html")))

(defn post-ts
  "Return timestamp for post id."
  [id]
  (if (nil? id)
    nil
    (let [[year month day hours minutes seconds] 
          (map #(Integer/parseInt %)
               (rest (re-matches #"^(\d+)-(\d+)-(\d+)T([0-9]+)_([0-9]+)_([0-9]+).*" id)))
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
  (let [txt (slurp (data-file id))
        lines (string/split-lines txt)
        [headers body] (split-with #(not (re-seq #"^-----$" %)) lines)
        categories (filter #((:set %) id) cats)
        ]
    (assoc (parse-headers headers)
      :BODY (string/join "\n" (butlast (rest (rest body))))
      :categories categories
      :categories2 (string/join ", " (map :name categories))
      :ID id
      :ts (post-ts id)
      :permalink (post-path id))))

(defn month-path
  "Post's month path from id."
  ([id]
     (month-path "/" id))
  ([sep id]
     (string/join sep (rest (re-matches #"^(\d+)-(\d+).*" id)))))

(defn months
  "Posts grouped by month-path."
  [posts]
  (group-by month-path posts))

(defn get-cached-post-part
  "Load part from cache."
  [post-id]
  (slurp (cache-path (post-path File/separator post-id))))

(defn write-months-parts
  [months]
  (dorun
   (for [[month posts] months
         :let [sorted-posts (sort #(compare %2 %1) posts)
               p (cache-path month)
               pages (paginate sorted-posts p)]
         [pposts ptab pfname] pages
         :let [path (cache-path month pfname)]]
     ;; Write each page
     ;; TODO: month-past has Web path separators...
     (spit* path
            (string/join "" (map get-cached-post-part pposts))))))

(defn write-post
    "Write post to cache and to archive."
    [post]
    (let [cfg      (assoc *cfg* :archive (str (:url *cfg*) "/archive"))
          entry    (assoc post
                     :categories? (boolean (seq (:categories post))))
          cpath    (cache-path (post-path File/separator (:ID post)))
          apath    (archive-path (post-path File/separator (:ID post)))
          content  (render (slurp "./blog/templates/entry.mustache")
                          {:cfg cfg :entry entry})
          pcontent (render (slurp "./blog/templates/permalink-entry.mustache")
                           {:cfg cfg :entry entry})]
      ;; Cached part
      (spit* cpath content)
      ;; Full article page
      (spit* apath (render (slurp "./blog/templates/permalink.mustache")
                           {:body pcontent
                            :cfg *cfg*
                            :title (:TITLE entry)
                            :feed (url-path (feed-apath []))}))))

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
        feed-url (url-path apath)]
    (spit (apply blog-path apath)
          (render (slurp "./blog/templates/atom.mustache")
                  {:cfg *cfg*
                   :entries (take (:page-size *cfg*) posts)
                   :lastmodified (post-ts (:ID (first posts)))
                   :self-url feed-url 
                   }))
    feed-url))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Categories
;;;
(defn parse-cat
  "Parse category file."
  [file]
  (let [txt (slurp (data-file file))
        [name & files] (string/split-lines txt)
        [_ id] (re-matches #"cat_([0-9]+).db$" file)]
    {:id id
     :url (str (:url *cfg*) (url-path ["archive" (str "cat_" id)]) "/")
     :name name
     :files files
     :set (set files)}))

(defn write-pages
  [template posts apath params]
  (let [url (str (:url *cfg*) (string/join "/" apath))]
    (dorun
     (for [[pposts ptab pfname] (paginate posts url)
           ;; File path
           :let [path (apply blog-path (conj apath pfname))]]
       (spit* path
              (render template
                      (assoc params
                        :cfg *cfg*
                        :body (string/join "" (map get-cached-post-part pposts))
                        :tab ptab
                        :feed (url-path (feed-apath apath)))))))))
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
    (write-feed cat-apath
                (map *posts* (take (:page-size *cfg*) posts)))))

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
  (binding [*cats* (map parse-cat (list-cats))]
    (let [posts (map (partial parse-post *cats*)
                     (list-posts))]
      (binding [*posts* (into {} (map #(vector (:ID %) %) posts))]
        (let [m (months (list-posts))]
          (dorun
           (for [post posts]
             (write-post post)))
          ;; Main feed
          (write-feed [] posts)
          (write-months-parts m)
          (write-cats *cats*)
          (ls-posts (take (:page-size *cfg*) posts)))))))

