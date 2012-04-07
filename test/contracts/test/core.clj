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

(fact "Dependent contracts"
  (let [c (c/=> [a b] {} #(= % (* a b)))
        +' (c +)]
    (+' 0 0) => 0
    (+' 1 2) => (throws AssertionError #"Post")))

(fact "Simple constrainst on a single-argument function without args declaration"
  (let [inc' ((c/=> number? pos?) inc)]
    (inc' 1) => 2
    (inc' "1") => (throws AssertionError #"Pre")
    (inc' -1) => (throws AssertionError #"Post")))

(fact "Contracts for functions with several args (without explicit args declaration)"
  (let [*' ((c/=> [even? odd?] pos?) *)]
    (*' 2 3) => 6
    (*' 2 2) => (throws AssertionError #"Pre" #"odd?")
    (*' 3 3) => (throws AssertionError #"Pre" #"even?")
    (*' 2 -3) => (throws AssertionError #"Post")))

(facts "Contracts for higher-order functions"

  (fact "Function returning function"
    (let [f (fn [x] (fn [y] (+ x y)))
          f' ((c/=> number? (c/=> even? pos?)) f)]
      ((f' 1) 2) => 3
      ((f' "1") 2) => (throws AssertionError #"Pre" #"number?")
      ((f' 1) 3) => (throws AssertionError #"Pre" #"even?")
      ((f' 1) -10) => (throws AssertionError #"Post" #"pos?")))

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
