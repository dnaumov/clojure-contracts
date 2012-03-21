(ns contracts.core
  (:use [clojure.string :only [] :as string]
        [clojure.core.match :only [match]]))

(defn either [f val & vals]
  (some (partial f val) vals))


(defn report [value {:keys [type var checker]}]
  {:pre [(either = type :pre :post)]}
  (let [type (case type
               :pre "Precondition"
               :post "Postcondition")]
    (format "%s failed for var %s %n Expecting: %s %n Given: %s"
            type var (pr-str checker) (pr-str value))))

(defn check [pred info]
  #(if (pred %)
     %
     (throw (AssertionError. (report % info)))))

(defn combinator-expr? [expr] 
  (and (seq? expr)
       (= (resolve (first expr)) #'=>)))

(defn gen-check [type expr]
  (if (combinator-expr? expr)
    expr
    `(check ~expr {:type ~type, :checker '~expr})
    
    ))

(defmacro =>
  [pre post]
  `(fn [f#]
     (comp ~(gen-check :post post)
           f#
           ~(gen-check :pre pre))))

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
