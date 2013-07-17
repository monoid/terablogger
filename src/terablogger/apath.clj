(ns terablogger.apath
  (:import java.io.File)
  (:require [terablogger.cfg :as cfg]
            [clojure.java.io :as io]
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
  (str (:url cfg/*cfg*) (url-path apath)))

(defn blog-path
  "Path for file in blog dir."
  [apath]
  (string/join File/separator (cons cfg/*blog-dir* apath)))

(defn data-path
  "Path for file in data dir.  Note that it accepts just filename, as 
data dir has no nested folders."
  [filename]
  (string/join File/separator (list cfg/*blog-dir* "data" filename)))

(defn archive
  "Construct apath within archive."
  [apath]
  (into ["archives"] apath))

(defn cache
  "Construct apath within a cache."
  [apath]
  (into ["parts"] apath))

(defn articles
  "Construct apath within article dir."
  [apath]
  (into ["articles"] apath))

(defn data-lister
  "List files in data dir that match regex."
  ([regex cmp]
     (data-lister ["data"] regex cmp))
  ([apath regex cmp]
     (fn []
       (->> (blog-path apath)
            (File.)
            (file-seq)
            (map (memfn getName))
            (filter (partial re-seq regex))
            (sort cmp)))))

(defn spit*
  "Spit data into apath, ensuring its parents exists."
  [apath data]
  (let [path (blog-path apath)]
   (io/make-parents path)
   (spit path data)))

