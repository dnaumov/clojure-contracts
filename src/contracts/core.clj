(ns contracts.core
  (:use [clojure.core.match :only [match]]))

(defn either
  ([val preds] ((apply some-fn preds) val))
  ([f val vals] (some (partial f val) vals)))


(defn report [value {:keys [type var pred]}]
  {:pre [(either = type [:pre :post])]}
  (let [type (case type
               :pre "Precondition"
               :post "Postcondition")]
    (format "%s failed for var %s %n Expecting: %s %n Given: %s"
            type var (pr-str pred) (pr-str value))))

(declare =>)
(defn combinator-expr? [expr]
  (and (seq? expr)
       (symbol? (first expr))
       (= (resolve (first expr)) #'=>)))

(def current-var (atom nil))

(defn gen-check [type exprs+preds]
  (into {}
        (for [[expr pred] exprs+preds]
          (if (combinator-expr? pred)
            `['~expr (~pred ~expr)]
            `['~expr (if (~pred ~expr)
                       ~expr
                       (throw (AssertionError. (report ~expr {:type ~type
                                                              :pred '~pred
                                                              :var ~(deref current-var)}))))]))))

(defmacro =>
  ([pre post]
     (let [pre (cond
                (combinator-expr? pre) (list [pre])
                (symbol? pre) (list [pre])
                (vector? pre) (list pre)
                (list? pre) pre)
           args (map #(vec (repeatedly (count %) (partial gensym "arg__")))
                     pre)
           pre (map zipmap args pre)]
       `(=> ~args ~pre ~post)))
  ([arglist pre post]
     (let [arglist (if (vector? arglist)
                     (list arglist)
                     arglist)
           pre (if (map? pre)
                 (list pre)
                 pre)
           f (gensym "f")
           pre-check-results (gensym "pre-check-results")
           result (gensym "result")]
       `(fn [~f]
          (fn ~@(-> (fn [args pre]
                      `([~@args]
                          (let [~pre-check-results ~(gen-check :pre pre)
                                ~@(mapcat (fn [arg] [arg `(get ~pre-check-results '~arg ~arg)]) ; contracts can alter the value of args, so we rebind them
                                          args)]
                            (let [~result (~f ~@args)
                                  ~result (-> ~(gen-check :post {result post}) first val)]
                              ~result))))
                    (map arglist pre)))))))

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

;; (=> number? pos?)
