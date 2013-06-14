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

(def ^:dynamic *data-dir*
  "Blog's data dir."
  (str *blog-dir* File/separator "data"))

(defmacro with-config
  "Bind *cfg*, *blog-dir* and *data-dir* to values in cfg and execute body."
  [cfg & body]
  `(binding [*cfg* ~cfg]
     (binding [*blog-dir* (:blog-dir *cfg*)]
       (binding [*data-dir* (str *blog-dir* File/separator "data")]
         ~@body))))

(defn blog-path
  "Path for blog file."
  [filename]
  (str *blog-dir* File/separator filename))

(defn blog-file
  "java.io.File for blog file."
  [filename]
  (File. (blog-path filename)))


(defn data-path
  "Path for data file."
  [filename]
  (str *data-dir* File/separator filename))

(defn data-file
  "java.io.File for data file."
  [filename]
  (File. (data-path filename)))

(defn archive-path [filename]
  (str *blog-dir* File/separator "archives" File/separator filename))

(defn cache-path [filename]
  (str *blog-dir* File/separator "parts" File/separator filename))

(defn data-lister
  "List files in data dir that match regex"
  [regex cmp]
  (fn []
    (let [dir (blog-file "/data")]
      (sort cmp
            (filter (partial re-seq regex)
                    (map
                     ;; Last component of path
                     #(last (string/split (.getName %)
                                          (re-pattern File/separator)))
                     (file-seq dir)))))))

(def list-cats
  "Function that return list of category files."
  (data-lister #"\.db$" compare))

(def list-posts
  "Function that return list of category posts."
  (data-lister #"\.txt$" #(compare %2 %1)))


(defn paginated-name [idx]
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
       ;; TODO: rel nex, prev
       (format "[<a href='%s'>%d</a>]"
               (str prefix "/" (paginated-name idx))
               idx)))))

(defn paginate
  "Split sequence of posts into pages of :page-size length."
  [posts url-prefix]
  (let [numbers (iterate inc 1)
        pages   (partition (:page-size *cfg*) posts)
        npages  (count pages)]
    (map list pages
              (map #(paginated-bar % npages url-prefix) numbers)
              (map paginated-name numbers))))

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
  "Return "
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
  ([id]
     (month-path "/" id))
  ([sep id]
     (string/join sep (rest (re-matches #"^(\d+)-(\d+).*" id)))))

(defn months
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
               pages (paginate sorted-posts p)]]
     ;; Write each page
     ;; TODO: month-past has Web path separators...
     (dorun
      (for [[pposts ptab pfname] pages
            :let [path (str p File/separator pfname)]]
        (do
          (io/make-parents path)
          (spit path
                (string/join "" (map get-cached-post-part pposts)))))))))

(defn write-post-part
    "Write post's part to cache."
    [post]
    (let [cfg   (assoc *cfg* :archive (str (:url *cfg*) "/archive"))
          entry (assoc post
                  :categories? (boolean (seq (:categories post))))
          cpath (cache-path (post-path File/separator (:ID post)))]
      (io/make-parents cpath)
      (spit
       cpath
       (render
        (slurp "./blog/templates/entry.mustache")
        {:cfg cfg :entry entry}))))


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
;;; Categories
;;;
(defn parse-cat
  "Parse category file."
  [file]
  (let [txt (slurp (data-file file))
        [name & files] (string/split-lines txt)
        [_ id] (re-matches #"cat_([0-9]+).db$" file)]
    {:id id
     :url (str (:url *cfg*) "/archives/cat_" id "/")
     :name name
     :files files
     :set (set files)}))

(defn write-cat
  [cat]
  (let [{id :id
         url :url
         name :name
         posts :files} cat
         ;; File base path
         p (archive-path (format "cat_%s" id))]
    (dorun
     (for [[pposts ptab pfname] (paginate posts url)
           ;; File path
           :let [path (str p File/separator pfname)]]
       (do
         (io/make-parents path)
         (spit path
               (render (slurp "./blog/templates/category-archive.mustache")
                       {:cfg *cfg*
                        :cat cat
                        :body (string/join "" (map get-cached-post-part pposts))
                        :tab ptab})))))))


(defn write-cats
  [cats]
  (dorun
   (for [cat cats]
     (write-cat cat))))

(defn write-feed
  [posts]
  (spit (blog-path "atom.xml")
        (render (slurp "./blog/templates/atom.mustache")
                {:cfg *cfg*
                 :entries (take (:page-size *cfg*) posts)
                 :lastmodified (post-ts (:ID (first posts)))
                 })))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (let [cats (map parse-cat (list-cats))
        posts (map (partial parse-post cats)
                   (list-posts))
        m (months (list-posts))]
    (dorun
     (for [post posts]
       (write-post-part post)))
    (write-feed posts)
    (write-months-parts m)
    (write-cats cats)
    (ls-posts (take (:page-size *cfg*) posts))))

