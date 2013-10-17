(ns terablogger.utils
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [terablogger.cfg :as cfg]))

(defn truncatechars
  "If msg's length exeeds n, truncate it, appending '...'. "
  [msg n]
  (if (> (count msg) n)
    (-> msg
        (subs 0 (- n 3))
        (string/trimr)  ; Trim whitespaces at end for better appearance
        (str "..."))
    msg))

(defn sort*
  "Sort in reverse order."
  ([seq]
     (sort #(compare %2 %1) seq))
  ([cmp seq]
     (sort #(cmp %2 %1) seq)))

(defn fmap
  "Apply f to all values of a map m, leaving keys as is."
  [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn split*
  "Split string with regexp.  If first argument is nil or empty string,
return []."
  [str-or-nil regex]
  (if (seq str-or-nil)
    (string/split str-or-nil regex)
    []))

(defn ask-user
  ([prompt]
     (print (format "%s: " prompt))
     (flush)
     (read-line))
  ([prompt default]
     (print (format "%s [%s]: " prompt default))
     (flush)
     (let [resp (read-line)]
       (if (string/blank? resp)
         default
         resp))))

(defn println*
  "println to stream out."
  [out & more]
  (binding [*out* out]
     (apply println more)))

(defn exec-editor [path]
  (-> (Runtime/getRuntime)
      (.exec (->> path
                  io/as-file
                  .getPath
                  (conj (:editor cfg/*cfg*))
                  into-array))
      (.waitFor)))

