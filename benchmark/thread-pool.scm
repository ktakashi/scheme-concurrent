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
  (let ((tp (make-thread-pool 10))
	(sq (make-shared-queue)))
    (let loop ((n* n*))
      (if (null? n*)
	  (thread-pool-release! tp)
	  (let ((p (heavy-task (car n*))))
	    (thread-pool-push-task! tp (lambda () (shared-queue-put! sq (p))))
	    (loop (cdr n*)))))))

(for-each (lambda (count n*) (time (run count n*))) counts n**)
