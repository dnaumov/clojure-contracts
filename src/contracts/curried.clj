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



