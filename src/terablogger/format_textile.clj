(ns terablogger.format-textile)

(defn fmt
  "Format HTML text with possible paragraph auto-break feature."
  [txt cfg]
  (.process (net.sf.textile4j.Textile.) txt))
