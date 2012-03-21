(ns contracts.core
  (:use [clojure.string :only [] :as string]
        [clojure.core.match :only [match]]))

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

(defn combinator-expr? [expr]
  (and (seq? expr)
       (= (resolve (first expr)) #'=>)))

(defn gen-check [type expr]
  (if (combinator-expr? expr)
    expr
    (cons `or
          (for [[val pred] expr]
            `(when-not (~pred ~val)
               (report ~val {:type ~type :pred '~pred}))))))

(defmacro =>
  ([pre post]
     (let [pre (if-not (vector? pre) [pre] pre)
           args (map #(symbol (str "arg-" (inc %)))
                     (range (count pre)))
           pre (zipmap args pre)]
       `(=> [~@args] ~pre ~post)))
  ([args pre post]
     (let [result (gensym "result")]
       `(fn [f#]
          (fn [~@args]
            (if-let [pre-error# ~(gen-check :pre pre)]
              (throw (AssertionError. pre-error#))
              (let [~result (f# ~@args)]
                (if-let [post-error# ~(gen-check :post {result post})]
                  (throw (AssertionError. post-error#))
                  ~result))))))))

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
