## Random notes:

Functional programming:
* create programs composed of existing functions e.g joining raliway pieces
* possible in all languages but made easier in functional langauges
* challenge is to adapt functions with given inputs and outputs to other functions
* currying is useful for getting functions down to one argument so that it's easier to chain them
* adopting functional patterns mean that people know how to use it if they know the design pattern

Monoids:

create associative combining functions e.g. <>

Good for:
* can parallelise computation and combine later
* can store intermediate results in reduce
* identity helps with generalising when you have empty lists, missing data, initial value
* take object *map* to monoid and then use monoid to *reduce* result
* examples of GoF patterns that are monoids: composite pattern, null object pattern, composable commands

Functor
----------
Take a function one type and makes it work on another type so you can stay in that type e.g. fmap or <$>
reduces the need to switch between types which is an anti-pattern

Monad
-------

* allows you to compose functions that move between contexts (or types)
* chain with bind or >>=
* runs in series
* railway metaphor - provides an adapter so we can put together tracks with one input and two outputs

Applicative (Functor)
-----------------------
* Combine  `<*>`
* Monads are good for chaining effects in serial. Applicatives run in parallel
* e.g. 3 validation functions. You don't just want to return success or all failures (not just first.. that would be monad). Applicative runs all three and then combines the output.





