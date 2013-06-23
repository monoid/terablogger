(ns terablogger.format-html
  )

(defn auto-break [txt]
  ; STUB
  txt
  )

(defn format
  "Format HTML text with possible paragraph auto-break feature."
  [txt cfg]
  (if (:html-auto-break cfg)
    (auto-break txt)
    txt))
