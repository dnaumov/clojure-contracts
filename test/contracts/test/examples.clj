;; This tutorial will give you a brief introduction to contract
;; programming and overview of clojure-contracts facilities.


;;; # Contract programming

;; Before you start, please check the following links to get some
;; background about what the contract programming is and how can it be
;; useful:

;; - [Wikipedia
;; article](http://en.wikipedia.org/wiki/Design_by_contract) - 
;; provides some general informationon subject

;; - [PLT Racket's guide to
;; contracts](http://docs.racket-lang.org/guide/contracts.html) -
;; a good introduction to contract programming, but beware! Don't
;; dig into Racket-specific details toodeep!

;; In short, contracts provides the same level of in-code documentation
;; as type declarations do in static-typed languages (that's it, they
;; describe what arguments a function takes and what it returns). The
;; key difference between the two is that contracts are checked in
;; runtime, giving much greater freedom and flexibility for
;; programmer.

;; Another important aspect of contracts (which is especially relevant
;; in the context of Clojure) is that they make debugging much easier
;; by providing good error messages (yes, that's right,
;; clojure-contracts allows you to get rid of those mysterious
;; exceptions and creepy stacktraces!)

;; Once you've got the idea what this fuss is all about, return here and
;; continue reading.


;;; # Using clojure-contracts

;; We'll start with namespace declaration. Note that it's idiomatic to
;; alias `contracts.core` as `c`. Also, we'll use a testing framework
;; called [Midje](https://github.com/marick/Midje) in this tutorial to
;; make sure that all provided examples are correct and working with the
;; current version of clojure-contracts.

(ns contracts.test.examples
  (:use [contracts.core :only [provide-contracts] :as c]
        midje.sweet))


;; The main form you should learn about is `=>`. The following line
;; of code will create a very simple contract:

(c/=> number? number?)

;; It returns the contract which ensures that function accepts a
;; single argument, a number, and returns a number.

;; But what the contract actually is? How can we make something
;; useful with our newly created contract? Actually, the `=>`
;; returns a plain function, which accepts a single argument -
;; another function, to which the contract will be applied. So:

(let [contract (c/=> number? number?)]
  (def constrained-inc (contract inc)))

;; We can check now that our `constrained-inc` works as a normal
;; `inc` for correct inputs, but throws an error with meaningful
;; message when it's given a non-number:

(fact "NullPointerExceptions suck!"
  (constrained-inc 1) => 2
  (inc nil) => (throws NullPointerException)
  (constrained-inc nil) => (throws AssertionError
                                   #"Expecting: number?"
                                   #"Given: nil"))

;; Here, `fact` is the Midje's macro which groups together a bunch of
;; tests, and `throws` checks that exception of the given type with
;; message matching the regexps is thrown. Also, don't let those
;; arrows (`=>`) confuse you! In this example they come from Midje and
;; represents individual checks (think of `(is ...)` from
;; clojure.test). Remember that we're using `c/...` alias for
;; contracts.core.


;; So, `=>` can take two args: contracts for function's input and
;; output. Let's constrain another function. This time, it will take 2
;; arguments:

(fact "Maybe it's valid to add numbers with string in javascript, but
  certainly not in Clojure."
  (let [constrained-plus ((c/=> [number? number?] number?) +)]
    (constrained-plus 1 2) => 3
    (+ "1" 2) => (throws ClassCastException
                         "java.lang.String cannot be cast to java.lang.Number")
    (constrained-plus "1" 2) => (throws AssertionError
                                        #"Expecting: number?"
                                        #"Given: \"1\"")))

;; Notice that double opening parens - we created anonymous contract
;; and applied it to the function immediately. Syntax for several
;; arguments follows the principle of least astonishment - you just
;; put predicates for each arg in a vector.

;; This also means that in previous example you have used syntax sugar
;; - `(c/=> pred ...)` is exactly the same as `(c/=> [pred] ...)`. Why
;; not get rid of the extra pair of parens if you can, right?


;;; ## Introducing `provide-contracts`. Pre and post.

;; So far we've been creating another var (or local binding) for
;; constrained versions of each function. Making it for the real code
;; would be very awkward, since usually you'll want to provide contracts
;; for already existing function. Fortunately, it's exactly the job
;; `provide-contracts` does. Let's write some function to check how it
;; works.

(defn factorial [x]
  (reduce * (range x)))

(provide-contracts
 (factorial (c/=> (complement neg?) pos?)))

;; Now we're trying to execute it and...

(fact "What a shame!"
  (factorial 1) => (throws AssertionError
                           #"Postcondition failed for var #'contracts.test.examples/factorial"
                           #"Expecting: pos?"
                           #"Given: 0"))

;; Oops! Contract system complains that function have violated its
;; postcondition! Of course, the implementation of `factorial` is
;; wrong: `range` returns numbers starting from 0, when we need
;; sequence of numbers from 1 to x (inclusive). Let's rewrite it all:

(defn factorial [x]
  (reduce * (range 1 (inc x))))

(provide-contracts
 (factorial (c/=> (complement neg?) pos?)))

(fact "Lesson learned."
  (factorial 1) => 1
  (factorial 0) => 1
  (factorial -1) => (throws AssertionError
                            #"Expecting: \(complement neg\?\)"
                            #"Given: -1"))

;; *Morale*:  

;; - When a precondition is failed, it's a caller of the function to
;; blame.

;; - When a postcondition is failed, you blame the author of the
;; function.

