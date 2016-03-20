(define (run count n*)
  (for-each thread-join!
	    (map (lambda (n) (thread-start! (make-thread (heavy-task n)))) n*)))
