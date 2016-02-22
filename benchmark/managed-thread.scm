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
  (let loop ((i 0) (n* n*) (n 0) (t* '()))
    (cond ((= i count) (for-each thread-join! t*))
	  ((= n 10) (for-each thread-join! t*) (loop (+ i 1) (cdr n*) 0 '()))
	  (else (loop (+ i 1) (cdr n*) (+ n 1) 
		      (cons (thread-start! (make-thread (heavy-task (car n*))))
			    t*))))))

(for-each (lambda (count n*) (time (run count n*))) counts n**)


