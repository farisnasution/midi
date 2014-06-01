(ns faris.midi)

(defmacro defpreware
  [name & args]
  (let [[docstring args] (if (string? (first args))
                           [(first args) (next args)]
                           [nil args])
        [function args] [(first args) (next args)]]
    `(do
       (def ~name
         (with-meta
           (fn [h#]
             (fn [m#]
               (h# (reduce (fn [p# n#]
                             (let [av# (if (vector? n#) n# [n#])
                                   cv# (get-in p# av#)]
                               (if-not (nil? cv#)
                                 (assoc-in p# av# (~function cv#))
                                 p#))) m# (list ~@args)))))
           {:fn-used ~function}))
       (alter-meta! (var ~name) assoc :doc ~docstring))))

(defmacro defpostware
  [name & args]
  (let [[docstring args] (if (string? (first args))
                           [(first args) (next args)]
                           [nil args])
        [function args] [(first args) (next args)]]
    `(do
       (def ~name
         (with-meta
           (fn [h#]
             (fn [m#]
               (let [v# (h# m#)]
                 (reduce (fn [p# n#]
                           (let [av# (if (vector? n#) n# [n#])
                                 cv# (get-in p# av#)]
                             (if-not (nil? cv#)
                               (assoc-in p# av# (~function cv#))
                               p#))) v# (list ~@args)))))
           {:fn-used ~function}))
       (alter-meta! (var ~name) assoc :doc ~docstring))))

(defn keywordize-keys
  [handler]
  (fn [request]
    (-> request
        clojure.walk/keywordize-keys
        handler)))
