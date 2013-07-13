(ns terablogger.format-markdown
  (:require [markdown.core :as md]))

(defn fmt
  "Format HTML text with possible paragraph auto-break feature."
  [txt cfg]
  (apply md/md-to-html-string txt (get cfg :markdown-params {})))
