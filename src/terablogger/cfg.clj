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
   :contacts "<a href=\"http://ammywhitehouse.gov\">%s</a>"
   :permalink true
   :input-regex #"\.(txt|html|markdown|textile)$"
   ;; Input format
   ;; 1. HTML
   :format "html"
   :html-auto-break true
   :html-base-url true
   :html-base-url-str "%base_url%"
   ;; ;; 2. Markdown
   ;; :format "markdown"
   ;; :markdown-params {}
   ;; ;; 3. Textile
   ;; :format "textile"
   })


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
  (into *cfg* (or (clojure.edn/read-string (slurp "terablogger.conf"))
                  {})))
