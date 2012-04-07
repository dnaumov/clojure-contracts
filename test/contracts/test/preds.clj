(ns contracts.preds
  (:refer-clojure :exclude [= == not= < > <= >=
                            identical? instance? extends? satisfies? isa?
                            contains? every? not-every? some not-any?
                            and or not])
  (:require [clojure.core :as c]))

;; TODO: tests!

(defn boolean? [x] (c/instance? Boolean x))

(def and c/every-pred)
(def or c/some-fn)
(def not c/complement)

(defn ^:private make-collection-pred [type-pred]
  (fn [pred]
    (fn [x] (c/and (type-pred x) (c/every? pred x)))))

(def coll-of (make-collection-pred coll?))
(def map-of (make-collection-pred map?))
(def vector-of (make-collection-pred vector?))
(def seq-of (make-collection-pred seq?))
(def sequential-of (make-collection-pred sequential?))

(defmacro ^:private defcurried
  ([name]
     (let [clj-var (ns-resolve 'clojure.core name)]
       `(-> (defn ~name
              ([a#] (partial ~clj-var a#))
              ([a# b#] (~clj-var a# b#)))
            (alter-meta! merge (meta ~clj-var)))))
  ([name & names]
     `(do (defcurried ~name)
          (defcurried ~@names))))

(defcurried
  = == not= < > <= >=
  identical? instance? extends? satisfies? isa?
  contains? every? not-every? some not-any?)
