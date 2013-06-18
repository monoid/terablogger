(ns terablogger.apath
  (:import java.io.File)
  (:require [terablogger.cfg :as cfg]
            [clojure.string :as string]))

(defn url-path
  [apath]
  (string/join "/" apath))

(defn blog-path
  "Path for blog file."
  [apath]
  (string/join File/separator (cons cfg/*blog-dir* apath)))

(defn blog-file
  "java.io.File for blog file."
  [apath]
  (File. (blog-path apath)))


(defn data-path
  "Path for data file."
  [filename]
  (string/join  File/separator (list cfg/*blog-dir* "data" filename)))

(defn archive-apath [apath]
  (into ["archive"] apath))

(defn archive-path [apath]
  (string/join File/separator (list* cfg/*blog-dir* "archive" apath)))

(defn cache-path [apath]
  (string/join File/separator (list* cfg/*blog-dir* "parts" apath)))

(defn data-lister
  "List files in data dir that match regex"
  [regex cmp]
  (fn []
    (let [dir (blog-file ["data"])]
      (sort cmp
            (filter (partial re-seq regex)
                    (map (memfn getName)
                         (file-seq dir)))))))

