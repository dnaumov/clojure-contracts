(ns contracts.core
  (:refer-clojure :exclude [and or not vector-of])
  (:require [clojure.core :as clj])
  (:use contracts.utils
        [clojure.core.match :only [match]]
        [clojure.walk :only [postwalk]]))

(def current-target (atom nil))

(defn humanize-checked-expr [expr]
  (let [s (match-s expr
            (deref (var x)) (name x)
            (deref x) (str "@" (name x))
            '%& "<rest-args>"
            :else (pr-str expr))]
    (if-not (.startsWith s "%")
      s
      (format "<%s arg>" (case s
                           ("%" "%1") "first"
                           "%2" "second"
                           "%3" "third"
                           (str (subs s 1) "th"))))))

(defn humanize-pred-expr
  "If expr is anonymous function, formats it's nicely, otherwise returns nil."
  [pred-expr checked-expr]
  (match-s pred-expr ((:or fn fn*) [arg] body)
    (->> body
         (postwalk #(if (= % arg) (symbol checked-expr) %))
         pr-str)))

(defn report [{:keys [type var pred expr value]}]
  (let [humanized-expr (if (= type :post)
                         "<result>"
                         (humanize-checked-expr expr))
        expecting (if-let [humanized-pred (humanize-pred-expr pred humanized-expr)]
                    humanized-pred
                    (format "%s is: %s" humanized-expr pred))]
    (format "%s failed for %s %n Expecting: %s %n Given: %s"
            (case type
              :pre "Precondition"
              :post "Postcondition"
              :invariant "Invariant")
            (clj/or var "<undefined>")
            expecting
            (pr-str value))))

;; TODO: rename
(defn gen-check* [{:keys [type cond return-val pred expr value]}]
  `(if ~cond
     ~return-val
     (throw (AssertionError.
             (report {:value ~value
                      :type ~type
                      :pred '~pred
                      :expr '~expr
                      :var ~(deref current-target)})))))

(defn gen-check [type exprs+preds]
  (->> (for [[expr pred] exprs+preds
             :let [[cond ret] (if (fn-contract-expr? pred)
                                [`(fn? ~expr) `(~pred ~expr)]
                                [`(~pred ~expr) expr])]]
         `['~expr ~(gen-check* {:cond cond
                                :return-val ret
                                :expr expr
                                :value expr
                                :type type
                                :pred pred})])
       (into {})))

(defn gen-constrained-body [f post pre args]
  (let [[pre-check-results result] (map gensym ["pre-check-results" "result"])
        [normal-args rest-args] (split-at-amp args)]
    `([~@args]
        (let [~pre-check-results ~(gen-check :pre pre)
              ;; contract can alter the values of args, so we rebind them
              ~@(mapcat (fn [arg] [arg `(get ~pre-check-results '~arg ~arg)])
                        (concat normal-args rest-args))
              ~result (apply ~f ~@normal-args ~(clj/or (first rest-args) []))]
          ~(-> (gen-check :post {result post}) first val)))))

(defn normalize-pre
  "Returns preconditions in the form ([pre ...] ...)"
  [expr]
  (cond
   (clj/and (list? expr) (every? vector? expr)) expr
   (vector? expr) (list expr)
   :else (list [expr])))

(defn build-numbered-args
  "Given a coll of preconditions, returns a vector of symbols prefixed
  by % (as those used in clojure's #(...))."
  [pre]
  (let [[normal-pre rest-pre] (split-at-amp pre)
        normal-args (map (fn [i] (symbol (str "%" i)))
                         (range 1 (inc (count normal-pre))))]
    (vec (cond
          (= 1 (count pre)) [(symbol "%")]
          rest-pre (concat normal-args (map symbol ["&" "%&"])) ; %& becomes rest__17578# if quote it directly
          :else normal-args))))

(defn build-pre-map [args pre]
  (zipmap (remove amp? args)
          (remove amp? pre)))

(defmacro =>
  ([post]
     `(=> [] ~post))
  ([pre post]
     (let [pre-list (normalize-pre pre)
           args-list (map build-numbered-args pre-list)
           pre-map (map build-pre-map args-list pre-list)]
       `(=> ~args-list ~pre-map ~post)))
  ([args pre post]
     (let [arglist (wrap-in-list-if vector? args)
           pre (wrap-in-list-if map? pre)
           f (gensym "f")]
       `(fn [~f]
          (fn ~@(map (partial gen-constrained-body f post) pre arglist))))))

(defn normalize-contract [expr]
  (match expr
    [pre '=> post] (list* `=> (map normalize-contract [pre post]))
    :else expr))

(defn gen-iref-contract [target pred]
  (let [newval (gensym "newval")]
    `(fn [~newval]
       ~(gen-check* {:type :invariant
                     :cond `(~pred ~newval)
                     :return-val true
                     :pred pred
                     :expr `(deref ~target)
                     :value newval}))))

(defprotocol Constrained
  (check-constraint [this]))

(defn apply-record-contract [f]
  (fn [& args]
    (let [result (apply f args)]
      (if (satisfies? Constrained result)
        (check-constraint result)
        result))))

(comment
  (doseq [v [#'assoc #'dissoc #'assoc-in #'update-in
             #'conj #'into #'merge #'merge-with]]
    (alter-var-root v apply-record-contract)))

(defn gen-constrain-record [class pred]
  (let [name (.getSimpleName class)
        this (gensym "this")
        [factory map-factory] (map #(symbol (str % name)) ["->" "map->"])]
    `(do (alter-var-root (var ~factory) apply-record-contract)
         (alter-var-root (var ~map-factory) apply-record-contract)
         (extend ~class
           Constrained
           {:check-constraint (fn [~this]
                                ~(gen-check* {:type :invariant
                                              :cond `(~pred ~this)
                                              :return-val this
                                              :pred pred
                                              :expr (symbol "<record>")
                                              :value this}))}))))

(defmacro provide-contract [target contract]
  (let [contract (normalize-contract contract)
        resolved-target (if (symbol? target)
                          (resolve target)
                          target)]
    (reset! current-target resolved-target)
    `(do ~(cond
           (fn-contract-expr? contract) `(alter-var-root (var ~target) ~contract)
           (class? resolved-target) (gen-constrain-record resolved-target contract)
           :else `(set-validator! ~target ~(gen-iref-contract target contract)))
         (reset! current-target nil))))

(defmacro provide-contracts [& clauses]
  (cons `do
        (for [clause clauses]
          `(provide-contract ~@clause))))


(load "preds")
;; (load "curried") ; this line should be commented out during development
