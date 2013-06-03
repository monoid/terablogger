(ns terablogger.core
  (:import java.io.File)
  (:use clostache.parser)
  (:require [clojure.string :as string])
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
  (str *blog-dir* File/separator "cache" File/separator filename))

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

(defn paginate
  "Split sequence of posts into pages of :page-size length."
  [posts]
  (partition (:page-size *cfg*) posts))

(defn truncatechars
  "If msg's length exeeds n, truncate it, appending '...'. "
  [msg n]
  (if (> (count msg) n)
    (str (string/trimr ; Trim for better text appearance
          (subs msg 0 (- n 3))) "...")
    msg))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Categories
;;;
(defn parse-cat
  "Parse category file."
  [file]
  (let [txt (slurp (data-file file))
        [name & files] (string/split-lines txt)]
    {:name name
     :files files
     :set (set files)}))

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

(defn parse-post 
  "Parse blog post."
  [file]
  (let [txt (slurp (data-file file))
        lines (string/split-lines txt)
        [headers body] (split-with #(not (re-seq #"^-----$" %)) lines)]
    [(parse-headers headers)
     (string/join "\n" (butlast (rest (rest body))))
     file]))

(defn write-post-part
  "Write post's part to cache."
  [post]
  (let [[h b f] post
        cfg (assoc *cfg* :archive (str (:url *cfg*) "/archive"))
        entry (assoc h :BODY b :ID f :permalink "STUB" :categories nil)]
    (spit
     (cache-path (str f ".html"))
     (render
      (slurp "./blog/templates/entry.mustache")
      {:cfg cfg :entry entry}))))


(defn posts-cats
  "Return list of cats post belongs to."
  [post cats]
  (let [[_ _ f] post]
   (filter #((:set %) f) cats)))

(defn ls-posts
  ([posts]
     (ls-posts posts (map parse-cat (list-cats))))
  ([posts cats]
     (dorun
      (for [[post n] (map list posts (range))]
        (let [[p _ _] post
              pcats (posts-cats post cats)]
          (print (format "%d. %s"
                         (inc n)
                         (truncatechars (:TITLE p) 32)))
          (when (seq pcats)
            (print (format " - [%s]" (string/join ", " (map :name pcats)))))
          (println (format " - %s" (:DATE p))))))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (dorun
   (map write-post-part
        (map parse-post (list-posts))))
  (ls-posts (map parse-post (take (:page-size *cfg*) (list-posts)))))
