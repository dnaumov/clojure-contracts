(ns contracts.core
  (:refer-clojure :exclude [and or not vector-of])
  (:require [clojure.core :as clj])
  (:use [clojure.core.match :only [match]]
        [clojure.walk :only [postwalk]]))

(declare =>)
(def current-var (atom nil))

(defn humanize-symbol-name [s]
  (if (.startsWith s "%")
    (format "<%s arg>"
            (case s
              ("%" "%1") "first"
              "%2" "second"
              "%3" "third"
              (str (subs s 1) "th")))
    s))


(defn humanize-pred-expr [pred-expr checked-expr]
  (match pred-expr ([(:or fn fn*) [arg] body] :seq)
    (-> pred-expr
        (nth 2)
        (->> (postwalk #(if (= % arg) checked-expr %))))))

(defn report [{:keys [type var pred expr value]}]
  (let [humanized-expr (if (= type :post)
                         "<result>"
                         (humanize-symbol-name (pr-str expr)))
        expecting (if-let [humanized-pred (humanize-pred-expr pred (symbol humanized-expr))]
                    (pr-str humanized-pred)
                    (format "%s is: %s" humanized-expr pred))]
    (format "%s failed for %s %n Expecting: %s %n Given: %s"
            (case type
              :pre "Precondition"
              :post "Postcondition")
            (clj/or var "<undefined>")
            expecting
            (pr-str value))))

(defn contract-expr? [expr]
  (clj/and (seq? expr)
           (symbol? (first expr))
           (= (resolve (first expr)) #'=>)))

(defn gen-check [type exprs+preds]
  (->> (for [[expr pred] exprs+preds
             :let [[cond ret] (if (contract-expr? pred)
                                [`(fn? ~expr) `(~pred ~expr)]
                                [`(~pred ~expr) expr])]]
         `['~expr (if ~cond
                    ~ret
                    (throw (AssertionError.
                            (report {:value ~expr
                                     :type ~type
                                     :pred '~pred
                                     :expr '~expr
                                     :var ~(deref current-var)}))))])
       (into {})))

(defn wrap-in-list-if [pred x]
  (if (pred x)
    (list x)
    x))

(defn gen-constrained-body [f post pre args]
  (let [[pre-check-results result] (map gensym ["pre-check-results" "result"])
        [normal-args [maybe-amp :as maybe-rest]] (split-with #(not= % '&) args)
        rest-args (if (= '& maybe-amp)
                    (next maybe-rest)
                    maybe-rest)]
    `([~@args]
        (let [~pre-check-results ~(gen-check :pre pre)
              ;; contract can alter the values of args, so we rebind them
              ~@(mapcat (fn [arg] [arg `(get ~pre-check-results '~arg ~arg)])
                        (concat normal-args rest-args))
              ~result (apply ~f ~@normal-args ~(clj/or (first rest-args) []))]
          ~(-> (gen-check :post {result post}) first val)))))

(defmacro =>
  ([pre post]
     (let [pre (cond
                (clj/and (list? pre) (every? vector? pre)) pre
                (vector? pre) (list pre)
                :else (list [pre]))
           args (map #(if (= 1 (count %))
                        [(symbol "%")]
                        (->> (range 1 (inc (count %)))
                             (map (fn [i] (symbol (str "%" i))) )
                             vec))
                     pre)
           pre (map zipmap args pre)]
       `(=> ~args ~pre ~post)))
  ([args pre post]
     (let [arglist (wrap-in-list-if vector? args)
           pre (wrap-in-list-if map? pre)
           f (gensym "f")]
       `(fn [~f]
          (fn ~@(map (partial gen-constrained-body f post) pre arglist))))))

(defmacro provide-contract [sym contract]
  (letfn [(normalize [expr]
            (match expr
              [pre '=> post] (list* `=> (map normalize [pre post]))
              :else expr))]
    (reset! current-var (resolve sym))
    `(alter-var-root (var ~sym) ~(normalize contract))))

(defmacro provide-contracts [& clauses]
  (cons `do
        (for [clause clauses]
          `(provide-contract ~@clause))))


(load "preds")
(load "curried") ; this line should be commented out during development
