(define (run n*)
  (for-each thread-join!
	    (map (lambda (n) (thread-start! (make-thread (heavy-task n)))) n*)))
