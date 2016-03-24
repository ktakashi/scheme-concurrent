(define (run count n*)
  (let ((e (make-thread-pool-executor 10)))
    (let loop ((n* n*) (f* '()))
      (cond ((null? n*) (for-each future-get f*) (shutdown-executor! e))
	    ((executor-available? e)
	     (let ((f (executor-submit! e (heavy-task (car n*)))))
	       (loop (cdr n*) (cons f f*))))
	    (else 
	     (for-each future-get f*)
	     (loop n* '()))))))
