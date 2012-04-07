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
       (= (resolve (first expr)) #'=>)))

(defn gen-check [type exprs+preds]
  (into {}
        (for [[expr pred] exprs+preds]
          (if (combinator-expr? pred)
            `['~expr (~pred ~expr)]
            `['~expr (if (~pred ~expr)
                       ~expr
                       (throw (AssertionError. (report ~expr {:type ~type :pred '~pred}))))]))))

(defmacro =>
  ([pre post]
     (let [pre (if-not (vector? pre) [pre] pre)
           args (map #(gensym (str "arg-" (inc %) "__"))
                     (range (count pre)))
           pre (zipmap args pre)]
       `(=> [~@args] ~pre ~post)))
  ([args pre post]
     (let [pre-check-results (gensym "pre-check-results")
           result (gensym "result")]
       `(fn [f#]
          (fn [~@args]
            (let [~pre-check-results ~(gen-check :pre pre)
                  ~@(mapcat (fn [arg] [arg `(get ~pre-check-results '~arg ~arg)]) ; contracts can alter the value of args, so we rebind them
                            args)]
              (let [~result (f# ~@args)
                    ~result (-> ~(gen-check :post {result post}) first val)]
                ~result)))))))

(defmacro provide-contract [sym contract]
  (letfn [(normalize [expr]
            (match expr
              [pre '=> post] (list* `=> (map normalize [pre post]))
              :else expr))]
    `(alter-var-root (var ~sym) ~(normalize contract))))

(defmacro provide-contracts [& clauses]
  (cons `do
        (for [clause clauses]
          `(provide-contract ~@clause))))
