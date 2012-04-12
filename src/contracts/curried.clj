(in-ns 'contracts.core)

(defmacro ^:private defcurried
  [& names]
  (->> (for [name names
             :let [clj-var (ns-resolve 'clojure.core name)
                   doc (str "Curried version of clojure.core/" name)
                   args (->> clj-var meta :arglists (take 2))]]
         `(do (defn ~name
                ([a#] (partial ~clj-var a#))
                ([a# b#] (~clj-var a# b#)))
              (alter-meta! (var ~name) assoc :doc ~doc :arglists '~args)))
       (cons `do)))

(defcurried
  = == not= < > <= >=
  identical? instance? extends? satisfies? isa?
  contains? every? not-every? some not-any?)



