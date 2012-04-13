(ns contracts.test.core
  (:require [contracts.core :as c] :reload)
  (:use [midje.sweet]))

(fact "Simple contracts with explicit arguments"
  (let [inc' ((c/=> [x]   {x number?}       pos?) inc)
        *'   ((c/=> [x y] {x even?, y odd?} pos?) *)]
    (inc' 1) => 2
    (inc' "1") => (throws AssertionError #"Pre")
    (inc' -1) => (throws AssertionError #"Post")
    (*' 2 3) => 6
    (*' 2 2) => (throws AssertionError #"Pre" #"odd\?")
    (*' 3 3) => (throws AssertionError #"Pre" #"even\?")
    (*' 2 -3) => (throws AssertionError #"Post")))

(fact "Checking arbitrary expressions"
  (let [f (fn [x y] (- (+ x y)))
        f' ((c/=> [x y] {x number?, y number?, (+ x y) pos?} odd?) f)]
    (f' 1 2) => -3
    (f' -1 -2) => (throws AssertionError #"Pre" #"pos\?")
    (f' "foo" 2) => (throws AssertionError #"Pre" #"number\?")
    (f' 1 1) => (throws AssertionError #"Post" #"odd\?")))

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
    (*' 2 2) => (throws AssertionError #"Pre" #"odd\?")
    (*' 3 3) => (throws AssertionError #"Pre" #"even\?")
    (*' 2 -3) => (throws AssertionError #"Post")))

(facts "Contracts for higher-order functions"

  (fact "Function returning function"
    (let [f (fn [x] (fn [y] (+ x y)))
          f' ((c/=> number? (c/=> even? pos?)) f)]
      ((f' 1) 2) => 3
      ((f' "1") 2) => (throws AssertionError #"Pre" #"number\?")
      ((f' 1) 3) => (throws AssertionError #"Pre" #"even\?")
      ((f' 1) -10) => (throws AssertionError #"Post" #"pos\?")))

  (fact "Function accepting function as an argument"
    (let [apply-c (c/=> (c/=> number? pos?) string?)
          f (apply-c (fn [f] (str (f 1))))
          g (apply-c (fn [f] (f "foo")))
          h (apply-c (fn [f] (f 1)))]
      (f inc) => "2"
      (f -) => (throws AssertionError #"Post" #"pos\?")
      (f "not a fn") => (throws AssertionError #"Pre" #"\(c/=\> number\? pos\?\)")
      (g inc) => (throws AssertionError #"Pre" #"number\?")
      (h inc) => (throws AssertionError #"Post" #"string\?"))))

(letfn [(do-checks [f]
          (facts
            (f 10) => 9
            (f "foo") => (throws AssertionError #"Pre" #"number\?")
            (f 0) => (throws AssertionError #"Post" #"pos\?")
            (f 2 3) => 6
            (f 2 2) => (throws AssertionError #"Pre" #"odd\?")
            (f 3 3) => (throws AssertionError #"Pre" #"even\?")
            (f 2 -3) => (throws AssertionError #"Post")))]

  (fact "Contracts for multi-arity functions"
    (let [f (fn
              ([x] (dec x))
              ([x y] (* x y)))
          f' ((c/=> ([x] [x y])
                    ({x number?} {x even?, y odd?})
                    pos?)
              f)]
      (do-checks f')))

  (fact "Multi-arity contracts without explicit args declaration"
    (let [f (fn ([x] (dec x)) ([x y] (* x y)))
          f' ((c/=> ([number?] [even? odd?]) pos?) f)]
      (do-checks f'))))

(fact "Contracts for variadic functions"
  (let [f (fn [a b & more] (apply hash-map a b more))
        f' ((c/=> [a b & more] {more (comp even? count)} map?) f)
        g (fn [& numbers] (apply + numbers))
        g' ((c/=> [& xs] {xs (partial every? number?)} number?) +)]
    (f' :a 1) => {:a 1}
    (f' :a 1 :b 2) => {:a 1 :b 2}
    (f' :a 1 :foo) => (throws AssertionError #"Pre" #"even\?")
    (g' 1 2) => 3
    (g' 1 :foo) => (throws AssertionError #"Pre" #"every\? number\?")))

(fact "humanize-pred-expr"
  (let [f #(c/humanize-pred-expr % :x)]
    (f '(fn* [p1__17145#] (inc p1__17145#))) => '(inc :x)
    (f '(fn* [p1__19793#] (* p1__19793# p1__19793#))) => '(* :x :x)
    (f '(fn [x] (* x (+ 2 x)))) => '(* :x (+ 2 :x))
    (f '(+ 1 2)) => nil
    (f 'pred?) => nil))

(facts "Mention the expression under check in the error message"

  (fact "When args are declared"
    (let [f ((c/=> [x y] {(+ x y) even?} pos?) +)
          g ((c/=> [& args] {args (partial every? number?)} number?) +)]
      (f 2 4) => 6
      (f 2 3) => (throws AssertionError #"Pre" #"\(\+ x y\)" #"even\?" #"5")
      (g 1 2 3) => 6
      (g 1 "2" 3) => (throws AssertionError #"Pre" #"args" #"\(1 \"2\" 3\)")))

  (fact "When they are not"
    (let [f ((c/=> number? number?) inc)
          g ((c/=> [pos? neg?] number?) +)]
      (f 1) => 2
      (f :foo) => (throws AssertionError #"Pre" #"first arg" #"number\?" #":foo")
      (g 1 -1) => 0
      (g 1 1) => (throws AssertionError #"Pre" #"second arg")))

  (fact "When pred is anonymous function"
    (let [f ((c/=> #(= % 5) number?) inc)
          g ((c/=> [x y & more]
                   {x #(= % 1) more #(= (count %) 2)}
                   number?)
             +)]
      (f 5) => 6
      (f 6) => (throws AssertionError #"Pre" #"\(= <first arg> 5\)")
      (g 1 2 3 4) => 10
      (g 0 1 2 3) => (throws AssertionError #"Pre" #"\(= x 1\)")
      (g 1 2 3) => (throws AssertionError #"Pre" #"\(= \(count more\) 2\)")))

  (fact "In postconditions"
    (let [f ((c/=> number? even?) dec)
          g ((c/=> number? #(= % 5)) inc)]
      (f 3) => 2
      (f 2) => (throws AssertionError #"Post" #"<result>")
      (g 4) => 5
      (g 5) => (throws AssertionError #"Post" #"\(= <result> 5\)"))))


(defn constrained-inc [x] (inc x))
(defn constrained-dec [x] (dec x))

(c/provide-contracts
 (constrained-inc (c/=> number? number?))
 (constrained-dec [number? => number?]))

(fact "provide-contracts and error messages"
  (constrained-inc "foo") => (throws AssertionError #"#'contracts.test.core/constrained-inc")
  (constrained-dec "bar") => (throws AssertionError #"Pre" #"number\?"))
