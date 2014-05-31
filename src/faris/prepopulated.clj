(ns faris.prepopulated)

(defn try-to-int
  [some-value]
  (try (Integer/parseInt some-value)
       (catch NumberFormatException error some-value)
       (catch ClassCastException error some-value)))

(defn try-to-float
  [some-value]
  (try (Float/parseFloat some-value)
       (catch NumberFormatException error some-value)
       (catch ClassCastException error some-value)))

(defn try-lower-case
  [value]
  (if (string? value)
    (clojure.string/lower-case value)
    value))

(defn header-value-to-map
  [string-value]
  (letfn [(trim [value] (clojure.string/trim value))
          (split-value [regex value] (clojure.string/split value regex))
          (construct-map [value]
            (let [[the-key the-value] (split-value #"=" (trim value))]
              {the-key (try-to-int the-value)}))]
    (try (->> string-value
              (split-value #";")
              (reduce (fn [previous-val next-val]
                        (into previous-val (construct-map next-val))) {})
              (clojure.walk/keywordize-keys))
         (catch IllegalArgumentException error string-value)
         (catch ClassCastException error string-value)
         (catch NullPointerException error nil))))
