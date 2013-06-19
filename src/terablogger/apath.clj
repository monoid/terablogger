(ns terablogger.apath
  (:import java.io.File)
  (:require [terablogger.cfg :as cfg]
            [clojure.string :as string]))

;;;
;;; Apath is a vector of strings that represents path relative to both
;;; blog dir and blog url.
;;;
;;; File pathes and URL may have different separators, so we delay
;;; converting path to string until last moment.
;;;

(defn url-path
  "Just relative path (withoug (*cfg* :url) or something like this)."
  [apath]
  (string/join "/" apath))

(defn full-url-path
  "Convert apath to full URL (with (*cfg* :url))."
  [apath]
  [str (:url cfg/*cfg*) (url-path apath)])

(defn blog-path
  "Path for file in blog dir."
  [apath]
  (string/join File/separator (cons cfg/*blog-dir* apath)))

(defn data-path
  "Path for file in data dir.  Note that it accepts just filename, as 
data dir has no nested folders."
  [filename]
  (string/join  File/separator (list cfg/*blog-dir* "data" filename)))

(defn archive
  "Construct apath within archive."
  [apath]
  (into ["archive"] apath))

(defn cache
  "Construct apath within a cache."
  [apath]
  (into ["parts"] apath))

(defn data-lister
  "List files in data dir that match regex."
  [regex cmp]
  (fn []
    (let [dir (blog-path ["data"])]
      (sort cmp
            (filter (partial re-seq regex)
                    (map (memfn getName)
                         (file-seq (File. dir))))))))

