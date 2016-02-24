(import (rnrs) (only (srfi :1) iota) (util concurrent) (time))

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
  (let ((e (make-thread-pool-executor 10)))
    (let loop ((n* n*) (f* '()))
      (cond ((null? n*) (for-each future-get f*) (shutdown-executor! e))
	    ((executor-available? e)
	     (let ((f (make-executor-future (heavy-task (car n*)))))
	       (execute-future! e f)
	       (loop (cdr n*) (cons f f*))))
	    (else 
	     (for-each future-get f*)
	     (loop n* '()))))))

(for-each (lambda (count n*) (time (run count n*))) counts n**)
