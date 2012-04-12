# Clojure-contracts

[Clojure-contracts](https://github.com/dnaumov/clojure-contracts) is a
library for
[contract programming](http://en.wikipedia.org/wiki/Design_by_contract)
in [Clojure](http://clojure.org).

**WARNING: this is a work in progress. It's not feature complete and
  may contain bugs.**


## Usage

See the **[tutorial](http://ubuntuone.com/0XjGCvBQviMz2qpaUUe5jW)**.

A quick example:

```
user> (defn foo [x] (+ x 5))
 
user> (foo nil) ;=>
 No message.
 [Thrown class java.lang.NullPointerException]
 
user> (provide-contract foo (c/=> number? number?))
 
user> (foo nil) ;=>
 Precondition failed for var #'user/foo 
 Expecting: number? 
 Given: nil
```

See the [tutorial](http://ubuntuone.com/0XjGCvBQviMz2qpaUUe5jW)
for details.


## Installation

Add `[clojure-contracts "0.0.1-SNAPSHOT"]` to your project's
dependecies. [Clojars page](http://clojars.org/clojure-contracts).


## License

Copyright (C) 2012 [Dmitri Naumov](https://github.com/dnaumov)

Distributed under the Eclipse Public License, the same as Clojure.
