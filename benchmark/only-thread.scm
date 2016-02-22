(import (rnrs) (only (srfi :1) iota) (srfi :18) (time))

(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(define (heavy-task n) (lambda () (fib n)))

(define counts (call-with-input-file "counts" read))

(define n** 
  (map (lambda (count)
	 (fold-left (lambda (acc o) (cons (+ (mod (length acc) 6) 20) acc))
		    '() (iota count)))
       counts))

(define (run count n*)
  (for-each thread-join!
	    (map (lambda (n) (thread-start! (make-thread (heavy-task n)))) n*)))

(for-each (lambda (count n*) (time (run count n*))) counts n**)
