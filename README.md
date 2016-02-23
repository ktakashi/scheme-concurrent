Concurrent library for Scheme
=============================

This library is extracted from Sagittarius Scheme for portability. The
changes made on this library would be merged into Sagittarius itself
and vice versa.

Documentation
-------------

See [`(util concurrent)`](http://ktakashi.github.io/sagittarius-ref.html#concurrent)


Requirements
------------

This library requires following SRFIs:

- [SRFI 1: List Library](http://srfi.schemers.org/srfi-1/)
- [SRFI 18: Multithreading support](http://srfi.schemers.org/srfi-18/)
- [SRFI 39: Parameter objects](http://srfi.schemers.org/srfi-39/)
- [SRFI 117: Queues based on lists](http://srfi.schemers.org/srfi-117/)

For R7RS, this is required:

- [SRFI 99: ERR5RS Records](http://srfi.schemers.org/srfi-99/)

Tested implementations
----------------------

Unfortunately, not all R6RS implementations support SRFI-18. So only the
following implementations are currently supported:

- Sagittarius
- Guile 2.0.11

NOTE: on Guile, some of the tests may faile due to the bug of `thread-sleep!`

The following R7RS implementations are also tested:

- Chibi Scheme (0.7.3) : works partially
- Gauche (0.9.4)

Why should you use this?
------------------------

Using raw thread, mutex and condition variables often causes dead lock or
other problems. This library provides 4 components which reduce manual
operation of them and possibly decrease the possiblity of multi threading
related issues.

### Shared queue 
`(util concurrent shared-queue)`

Inter-thread communication sometimes required if you need write multi thread
programming. This component makes it easier instead of using mutex and
condition variables.

### Thread pool 
`(util concurrent thread-pool)`

Limiting number of threads might be required by your environment. This
component re-use managed threads.

### Future and executor
`(util concurrent future)` and  `(util concurrent executor)`

Future makes you to write asynchronouse program easier. Using simple future
is equivalent with making a thread, starting the thread and joining the 
thread. Combination of executor and future provides easier resource 
management.


TODO
----

- Supporting Racket.
- ~~Supporting R7RS implementations which supports SRFI-18~~.

Copyright and lincense
----------------------

Copyright 2016 Takashi Kato. Code released under the BSD-style license.
See [COPYING](COPYING).
