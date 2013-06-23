(ns terablogger.format-html
  (:require [clojure.string :as string]))

(defn auto-break [txt]
  (string/replace txt #"(\r\r|\n\n|\r\n\rn)" "<br>\n"))

(defn html-escape [txt]
  (string/escape txt
                 {\& "&amp;"
                  \" "&quot;"
                  \< "&lt;"
                  \> "&gt;"
                  \' "&#39;"}))

(defn fmt
  "Format HTML text with possible paragraph auto-break feature."
  [txt cfg]
  (let [auto-broke (if (:html-auto-break cfg)
                     (auto-break txt)
                     txt)]
    (if (:html-base-url cfg)
      (string/replace auto-broke
                      (:html-base-url-str cfg)
                      (html-escape (:url cfg)))
      auto-broke)))
