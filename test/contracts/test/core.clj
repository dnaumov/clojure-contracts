(ns contracts.test.core
  (:use [contracts.core :as c :exclude [=>]] :reload)
  (:use [midje.sweet]))

(fact "Simple contracts with explicit arguments"
  (let [inc' ((c/=> [x]   {x number?}       pos?) inc)
        *'   ((c/=> [x y] {x even?, y odd?} pos?) *)]
    (inc' 1) => 2
    (inc' "1") => (throws AssertionError #"Pre")
    (inc' -1) => (throws AssertionError #"Post")
    (*' 2 3) => 6
    (*' 2 2) => (throws AssertionError #"Pre" #"odd?")
    (*' 3 3) => (throws AssertionError #"Pre" #"even?")
    (*' 2 -3) => (throws AssertionError #"Post")))

(fact "Checking arbitrary expressions"
  (let [f (fn [x y] (- (+ x y)))
        f' ((c/=> [x y] {x number?, y number?, (+ x y) pos?} odd?) f)]
    (f' 1 2) => -3
    (f' -1 -2) => (throws AssertionError #"Pre" #"pos?")
    (f' "foo" 2) => (throws AssertionError #"Pre" #"number?")
    (f' 1 1) => (throws AssertionError #"Post" #"odd?")))

(fact "Simple constrainst on a single-argument function without args declaration"
  (let [inc' ((c/=> number? pos?) inc)]
    (inc' 1) => 2
    (inc' "1") => (throws AssertionError #"Pre")
    (inc' -1) => (throws AssertionError #"Post")))

(fact "Contracts for functions with several args"
  (let [*' ((c/=> [even? odd?] pos?) *)]
    (*' 2 3) => 6
    (*' 2 2) => (throws AssertionError #"Pre" #"odd?")
    (*' 3 3) => (throws AssertionError #"Pre" #"even?")
    (*' 2 -3) => (throws AssertionError #"Post")))

(future-facts "Contracts for higher-order functions"

  (fact "Function returning function"
    (let [f (fn [x] (fn [y] (+ x y)))
          f' ((c/=> number? (c/=> even? pos?)) f)]
      ((f' 1) 2) => 3
      ((f' "1") 2) => (throws AssertionError #"Pre" #"number?")
      ((f' 1) 3) => (throws AssertionError #"Pre" #"even?")
      ((f' 1) -10) => (throws AssertionError #"Post")))

  (fact "Function accepting function as an argument"
    (let [apply-c (c/=> (c/=> number? pos?) string?)
          f (apply-c (fn [f] (str (f 1))))
          g (apply-c (fn [f] (f "foo")))
          h (apply-c (fn [f] (f 1)))]
      (f inc) => "2"
      (f -) => (throws AssertionError #"Post" #"pos?")
      (g inc) => (throws AssertionError #"Pre" #"number?")
      (h inc) => (throws AssertionError #"Post" #"string?"))))



(future-fact "Different contracts for different arities"
             (let [f (fn
                       ([x] (dec x))
                       ([x y] (* x y)))
                   f' ((c/=> number? neg?
                             [even? odd?] pos?)
                       f)]
               (f' 0) => -1
               (f' "foo") => (throws AssertionError #"Pre" #"number?")
               (f' 1) => (throws AssertionError #"Post" #"neg?")
               ;; XXX: <copypasted>
               (f' 2 3) => 6
               (f' 2 2) => (throws AssertionError #"Pre" #"odd?")
               (f' 3 3) => (throws AssertionError #"Pre" #"even?")
               (f' 3 2) => (throws AssertionError #"Pre" #"even?" #"odd?")
               (f' 2 -3) => (throws AssertionError #"Post")
               ;; </copypasted>
               ))


#_(comment
    (c/=> {[number?] number?
           [even? odd?] pos?})
    ;; =>
    (fn [f]
      (fn
        ([x] (f x))
        ([x y] (f x y)))))



;; (defn h [x y] (* x y))
;; (def h' ((c/=> [number? number?] pos?) h))


#_(provide-contract g [x]
    [number? => (partial < x)])

#_(provide-contract h [x y]
    [[pos? pos?] => (partial < (+ x y))]
    (=> [pos? pos?] (partial < (+ x y))))


#_
(provide-contracts
 (f [number? => number?])
 (g [number? => [number? => number?]]) ; returns function

 (repeat [anything => seq?] ; multiple arities
         [[int? anything] => seq?]) ; more than 1 arg

 (repeat [n x] ; dependent contract
         [[int? anything] => (seq-of x)])

 (foo [x s] [[(and number? #(not= 0)) string?] => number? (partial not= 1)]))

#_
(def f' (fn [f]
          (combine f
                   (check number? {:type :pre, :var #'f, :checker 'number?})
                   (fn [g]
                     (combine g
                              (check number? {:type :pre, :var #'f, :checker 'number?})
                              (check number? {:type :post, :var #'f, :checker 'number?}))))))





#_(defn report [value {:keys [type var checker]}]
    {type (either = :pre :post) ; fn or vector of fns
     report string?}) ; post
