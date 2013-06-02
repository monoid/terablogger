(ns terablogger.core
  (:import java.io.File)
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
   ;; Input format
   :format "html" ; the only supported
   :html-auto-break true})


(def ^:dynamic *cfg*
  "Config."
  (default-config))

(def ^:dynamic *blog-dir*
  "Blog's base dir."
  (:blog-dir *cfg*))

(defmacro with-config
  "Bind *cfg* and *blog-dir* to values if cfg and execute body."
  [cfg & body]
  `(binding [*cfg* ~cfg]
     (binding [*blog-dir* (:blog-dir *cfg*)]
       ~@body)))

(defn blog-path
  [path]
  (str *blog-dir* path))

(defn blog-file
  [path]
  (File. (blog-path path)))


(defn data-lister
  "List files in data dir that match regex"
  [regex cmp]
  (fn []
    (let [dir (blog-file "/data")]
      (sort cmp
            (filter (comp (partial re-seq regex) 
                          #(.getName %))
                    (file-seq dir))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Categories
;;;
(defn parse-cat [file]
  (let [txt (slurp file)
        [name & files] (string/split-lines txt)]
    {:name name
     :files files}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Posts
;;;

(defn parse-headers [headers]
  (apply hash-map
         (mapcat #(let [[key val] (string/split % #": " 2)]
                    (list (keyword key) val))
                 headers)))

(defn parse-post [file]
  (let [txt (slurp file)
        lines (string/split-lines txt)
        [headers body] (split-with #(not (re-seq #"^-----$" %)) lines)]
    [(parse-headers headers) (string/join "\n" (butlast (rest (rest body))))]))

(defn ls-posts [posts]
  (dorun
   (for [[[p b] n] (map list posts (range))]
     (println (format "%d. %s - %s"
                      (inc n)
                      (:TITLE p)
                      (:DATE p))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (ls-posts (map parse-post (take 10 (list-posts)))))
