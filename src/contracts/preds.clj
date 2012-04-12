(in-ns 'contracts.core)

;; TODO: tests!

(defn boolean? [x] (instance? Boolean x))
(defn either [f vals] #(some (partial f %) vals))
(def any (constantly true))

(defmacro match? [pattern]
  `(fn [x#] (match x# ~pattern true)))

(def and every-pred)
(def or some-fn)
(def not complement)

(defn ^:private make-collection-pred [type-pred]
  (fn [pred]
    (fn [x] (clj/and (type-pred x) (every? pred x)))))

(def coll-of (make-collection-pred coll?))
(def map-of (make-collection-pred map?))
(def vector-of (make-collection-pred vector?))
(def seq-of (make-collection-pred seq?))
(def sequential-of (make-collection-pred sequential?))

