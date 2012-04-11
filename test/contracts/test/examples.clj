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
;; dig into Racket-specific details too deep!

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
  (:use [contracts.core :only [provide-contract provide-contracts] :as c]
        [contracts.preds :only [coll-of boolean?]]
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


;;; ## Introducing 'provide-contract'. Pre and post.

;; So far we've been creating another var (or local binding) for
;; constrained versions of each function. Making it for the real code
;; would be very awkward, since usually you'll want to provide contracts
;; for already existing function. Fortunately, it's exactly the job
;; `provide-contract` does. Let's write some function to check how it
;; works.

(defn factorial [x]
  (reduce * (range x)))

(provide-contract factorial
  (c/=> (complement neg?) pos?))

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

(provide-contract factorial
  (c/=> (complement neg?) pos?))

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


;;; ## More on arguments and functions.

;; It's common for clojure functions to have multiple arities. For
;; example, the following function returns the sum of the numbers in
;; given collection. It can also take an optional argument f, which
;; will be applied to every number before they get summed:

(defn sum
  ([numbers] (apply + numbers))
  ([f numbers] (apply + (map f numbers))))

;; This way, you can get a sum of either original collection or its
;; modified variant:

(fact
  (let [numbers [1.2 3.8 0.5]]
    (sum numbers) => 5.5
    (sum #(Math/round %) numbers) => 6))

;; All right, now let's put constraints on our function. The syntax
;; for multi-arity functions' contracts is pretty predictable:

(provide-contract sum
  (c/=> ([(coll-of number?)]
           [fn? (coll-of number?)])
        number?))

;; As you can see, we've wrapped preconditions for different arities
;; in a list. Just like `pre` is a  shortened form of `[pre]`, `[pre]`
;; itself is a shortand for `([pre])`. Also, note the `coll-of`
;; call. This higher-order function checks that the given argument is
;; a collection consisting of the items satisfying the given
;; predicate (in other words, the call above is rougly equivalent to
;; `(partial every? number?)`.

;; Let's make sure everything works properly:

(fact
  (sum [1 2 3]) => 6
  (sum [1 2 :boom]) => (throws AssertionError
                               #"Precondition failed for var #'contracts.test.examples/sum"
                               #"Expecting: \(coll-of number\?\)"
                               #"Given: \[1 2 :boom\]")
  (sum inc [1 2 3]) => 9
  (sum 1 [2 3]) => (throws AssertionError
                           #"Expecting: fn\?"
                           #"Given: 1"))

;; Seems fine... but is it so? Consider the following code:

(fact "No! Not this again!"
  (sum str [4 8 15 16 23 42]) => (throws ClassCastException
                                         #"java.lang.String cannot be cast"
                                         #"to java.lang.Number"))

;; We've passed incorrect function to `sum` and got not very
;; informative ClassCastException. It's easy to understand what the
;; problem is when you know the implementation of the `sum`, but
;; imagine that the hypotetical user of our function made a wrong
;; assumption about how it works: he\she thought that the function `f`
;; got applied to the *result* of the summation. In such situation,
;; the "java.lang.String cannot be cast to java.lang.Number" would
;; leave him completely confused. (Of course, this example is a little
;; contrived, but in real code things would be a thousand times more
;; complex, so a good error messages in such situations can save you
;; hours of debugging and reading documentaion.)

;; Fortunately, clojure-contracts provides you a feature called
;; *higher-order contract*. It allows you to specify the constraints
;; on a function which passed as an argument to (or returned by) your
;; function.

;; Let's correct our contract definition:

(provide-contract sum
  (c/=> ([(coll-of number?)]
           [(c/=> number? number?) (coll-of number?)])
        number?))

;; We've replaced `fn?` predicate with another `c/=>` call. That's
;; what higher-order contract is: here, in will ensure that the given
;; function takes a number and returns a number. Let's try it in
;; action:

(fact "Functional nirvana."
  (sum str [4 8 15 16 23 42]) => (throws AssertionError
                                         #"Postcondition failed"
                                         #"Expecting: number\?"
                                         #"Given: \"4\""))

;; That's much clearer: contract system tells the caller that his
;; function is expected to return a number, but returns "4" instead.


;;; ## Checking arbitrary expressions.

;; You already know pretty much about clojure-contracts, but there is
;; one omission in your knowledge: clojure's standart mechanism of pre
;; and post conditions doesn't limit you to checking predicates
;; against single arguments; it allows you to make an assertion on any
;; expression involving arguments and\or function's return value:

(defn foo [a b]
  {:pre [(not= (+ a b) 13)]}
  (comment ...))

(fact "Someone very superstitious has wrote this function."
  (foo 7 6) => (throws AssertionError))

;; Of course, it's possible to write this kind of contracts using the
;; `=>` form as well. The only requirement is that you have to
;; explicitly specify the arguments your function takes. Consider the
;; following example:

(defn harmonic-mean [x y]
  (/ 2 (+ (/ x) (/ y))))

;; As its name implies, this function returns the [harmonic
;; mean](http://en.wikipedia.org/wiki/Harmonic_mean) of two
;; numbers. If you have read the definiton carefully, you should
;; noticed that "x and y are numbers" is not the only requirement for
;; this function; another one is that one number must not be an
;; additive inverse of the other, i.e. x can not be equal -y, or we
;; will got an exception complaining that it's impossible to divide by
;; zero.

;; So, let's write down our conclusions:

(provide-contract harmonic-mean
  (c/=> [x y]
        {x number?, y number?, (+ x y) (complement zero?)}
        number?))

;; As you can see, this time slightly different syntax is used. First,
;; an additional argument representing an arguments to the function is
;; given to `=>`. Second, we use a map as a precondition form. Keys of
;; this map are expressions to check, and values are predicates which
;; will be checked against this expressions. See how it works:

(fact
  (harmonic-mean 10 30) => 15
  (harmonic-mean 10 -10) => (throws AssertionError
                                    #"Expecting: \(complement zero\?\)"
                                    #"Given: 0"))

;; One thing to note is that arguments' names in contract don't have
;; to be the same as in function declaration. Correlation between them
;; is established exclusively based on their order, but it makes
;; things clearer when the names are the same.


;; Another topic we haven't discussed yet is functions with variable
;; number of args. Currently, you have to explicitly define arguments
;; when providing contract for such function:

(fact
  (let [constrained-hash-map ((c/=> [& keys+vals]
                                    {keys+vals (comp even? count)}
                                    map?)
                              hash-map)]
    (constrained-hash-map :a 1 :b 2) => {:a 1 :b 2}
    (constrained-hash-map :a 1 :boo) => (throws AssertionError
                                                #"Expecting: \(comp even\? count\)"
                                                #"Given: \(:a 1 :boo\)")))


;;; ## Dependent contracts.

;; Elaborating on idea from the previous section, we can make the
;; postcondition to depend on the function's arguments. It allows us
;; to express in code statements like "return value must be greater
;; than sum of the arguments" or, for example, "the function returns
;; its argument unmodified if it satisfies certain
;; conditions". Contracts with such postconditions are called
;; *dependent contracts*. As an example, let's implement the function
;; that computes the [arithmetic
;; mean](http://en.wikipedia.org/wiki/Arithmetic_mean) of two numbers
;; and take into account the fact that it's always greater than or
;; equal to the [geometric
;; mean](http://en.wikipedia.org/wiki/Geometric_mean) of the same
;; numbers, or, in other words, that x+y divided by 2 is >= square
;; root of x*y (this fact is known as an [AM-GM
;; inequality](http://en.wikipedia.org/wiki/Inequality_of_arithmetic_and_geometric_means)).

;; Here is the function:

(defn arithmetic-mean [x y]
  (/ (+ x y) 2))

;; And the contract:

(provide-contract arithmetic-mean
  (c/=> [x y]
        {x (every-pred number? (complement neg?))
         y (every-pred number? (complement neg?))}
        (partial <= (Math/sqrt (* x y)))))

;; Of course, this contract will never be violated as long as valid
;; input is given to the function (which is ensured by preconditions)
;; and its implementation is correct:

(fact
  (arithmetic-mean 8 2) => 5
  (Math/sqrt (* 8 2)) => 4.0
  (arithmetic-mean -1 -2) => (throws AssertionError
                                     #"Expecting:"
                                     #"\(every-pred number\? \(complement neg\?\)\)"
                                     #"Given: -1"))

;; If you think about it, the features we've just examined allows us
;; to generate inputs for a function based on its preconditions, and
;; check that return values are correct as it's defined by
;; postconditions. This is called *generative-style testing*; for
;; further information on topic, check the following links:

;; - [QuickCheck](http://en.wikipedia.org/wiki/QuickCheck) - Haskell
;; library which enables this style of testing

;; - [test.generative](https://github.com/clojure/test.generative) -
;; Clojure library implementing the same ideas


;;; ## Protocols, multimethods and beyond.

