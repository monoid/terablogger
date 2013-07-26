(ns terablogger.templates
  (:require [clostache.parser :as parser]
            [terablogger.apath :as apath]
            [terablogger.cfg :as cfg]))

(defn tmpl [name]
  ;; TODO: lookup in serveral places, perhaps
  (let [path (apath/blog-path ["templates"
                    (str name ".mustache")])]
    (slurp path :encoding (:encoding cfg/*cfg*))))

(defn render [name params]
  (parser/render (tmpl name)
                 params))
