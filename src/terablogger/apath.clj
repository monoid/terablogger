(ns terablogger.apath
  (:import java.io.File)
  (:require [terablogger.cfg :as cfg]
            [clojure.string :as string]))

(defn url-path
  [apath]
  (string/join "/" apath))

(defn full-url-path
  [apath]
  [str (:url cfg/*cfg*) (url-path apath)])

(defn blog-path
  "Path for blog file."
  [apath]
  (string/join File/separator (cons cfg/*blog-dir* apath)))

(defn data-path
  "Path for data file."
  [filename]
  (string/join  File/separator (list cfg/*blog-dir* "data" filename)))

(defn archive [apath]
  (into ["archive"] apath))

(defn cache [apath]
  (into ["parts"] apath))

(defn data-lister
  "List files in data dir that match regex"
  [regex cmp]
  (fn []
    (let [dir (blog-path ["data"])]
      (sort cmp
            (filter (partial re-seq regex)
                    (map (memfn getName)
                         (file-seq (File. dir))))))))

