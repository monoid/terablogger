(ns terablogger.cfg
  (:require clojure.edn))

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
   :path "/blog"
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

(defmacro with-config
  "Bind *cfg*, *blog-dir* and *data-dir* to values in cfg and execute body."
  [cfg & body]
  `(binding [*cfg* ~cfg]
     (binding [*blog-dir* (:blog-dir *cfg*)]
       ~@body)))

(defn load-config
  []
  ;; STUB implementation
  (or (clojure.edn/read-string (slurp "terablogger.conf"))
      *cfg*))
