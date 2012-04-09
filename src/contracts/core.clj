(ns contracts.core
  (:use [clojure.core.match :only [match]]))

(declare =>)
(def current-var (atom nil))

(defn report [value {:keys [type var pred]}]
  (let [type (case type
               :pre "Precondition"
               :post "Postcondition")]
    (format "%s failed for var %s %n Expecting: %s %n Given: %s"
            type var (pr-str pred) (pr-str value))))

(defn contract-expr? [expr]
  (and (seq? expr)
       (symbol? (first expr))
       (= (resolve (first expr)) #'=>)))

(defn gen-check [type exprs+preds]
  (->> (for [[expr pred] exprs+preds]
         (if (contract-expr? pred)
           `['~expr (~pred ~expr)]
           `['~expr (if (~pred ~expr)
                      ~expr
                      (throw (AssertionError.
                              (report ~expr {:type ~type
                                             :pred '~pred
                                             :var ~(deref current-var)}))))]))
       (into {})))

(defn wrap-in-list-if [pred x]
  (if (pred x)
    (list x)
    x))

(defn gen-constrained-body [f post pre args]
  (let [[pre-check-results result] (map gensym ["pre-check-results" "result"])
        [normal-args ampersand rest-args] (partition-by #{'&} args)]
    `([~@args]
        (let [~pre-check-results ~(gen-check :pre pre)
              ;; contract can alter the values of args, so we rebind them
              ~@(mapcat (fn [arg] [arg `(get ~pre-check-results '~arg ~arg)])
                        (concat normal-args rest-args))
              ~result (apply ~f ~@normal-args ~(or (first rest-args) []))]
          ~(-> (gen-check :post {result post}) first val)))))

(defmacro =>
  ([pre post]
     (let [pre (cond
                (and (list? pre) (every? vector? pre)) pre
                (vector? pre) (list pre) 
                :else (list [pre]))
           args (map #(vec (repeatedly (count %) (partial gensym "arg__")))
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
