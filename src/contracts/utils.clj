(ns contracts.utils
  "For internal use only."
  (:use [clojure.core.match :only [match]]
        [clojure.walk :only [postwalk]]))

(defmacro match-s
  "Like match, but there is no need to wrap checks in (... :seq) in
  order to match list."
  [expr & clauses]
  (letfn [(wrap-in-seq [form]
            (postwalk #(if (and (list? %)
                                (not (keyword? (first %))))
                         (list % :seq)
                         %)
                      form))]
    `(match ~expr ~@(->> clauses
                         (partition 2)
                         (map (fn [[q a]] [(wrap-in-seq q) a]))
                         (apply concat)))))

(defn fn-contract-expr?
  "Checks whether expr is (c/=> ...)"
  [expr]
  (and (seq? expr)
       (symbol? (first expr))
       (= (resolve (first expr)) (find-var 'contracts.core/=>))))

(defn wrap-in-list-if [pred x]
  (if (pred x)
    (list x)
    x))

(defn amp? [x]
  (= x '&))

(defn split-at-amp
  "Returns [things-before-& things-after-&]"
  [coll]
  (let [[normal maybe-rest] (split-with (complement amp?) coll)]
    [normal (when (amp? (first maybe-rest))
              (next maybe-rest))]))
