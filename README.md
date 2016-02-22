Concurrent library for Scheme
=============================

This library is extracted Sagittarius Scheme for portability. The changes
made on this library would be merged into Sagittarius itself and vice versa.

Documentation
-------------

See [`(util concurrent)`](http://ktakashi.github.io/sagittarius-ref.html#concurrent)


Requirements
------------

This library requires following SRFIs:

- [SRFI 18: Multithreading support](http://srfi.schemers.org/srfi-18/)
- [SRFI 19: Time Data Types and Procedures](http://srfi.schemers.org/srfi-19/)
- [SRFI 39: Parameter objects](http://srfi.schemers.org/srfi-39/)
- [SRFI 117: Queues based on lists](http://srfi.schemers.org/srfi-117/)


And currently only implemented on R6RS.

Tested implementations
----------------------

Unfortunately, not all R6RS implementations support SRFI-18. So only the
following implementations are currently supported:

- Sagittarius
- Guile 2.0.11

NOTE: on Guile, some of the tests may faile due to the bug of `thread-sleep!`

Why should you use this?
------------------------

### Shared queue
Inter-thread communication sometimes required if you need write multi thread
programming. This component makes it easier instead of using mutex and
condition variables.

### Thread pool
Limiting number of threads might be required by your environment. This
component re-use managed threads.

### Future and executor 
Future makes you to write asynchronouse program easier. Using simple future
is equivalent with making a thread, starting the thread and joining the 
thread. Combination of executor and future provides easier resource 
management.


TODO
----

- Supporting Racket.
- Supporting R7RS implementations which supports SRFI-18.

Copyright and lincense
----------------------

Copyright 2016 Takashi Kato. Code released under the BSD-style license.
See [COPYING](COPYING).
