(define-library (test-lib)
  (export test-begin
	  test-end
	  test-assert
	  test-equal
	  test-error)
  (import (scheme base))
  ;; sigh...
  (cond-expand
   ;; Larceny 0.98 has a bug on test-error
   ;; So we can't use (srfi 64)
   ;; NB: for some reason it doesn't allow me to re-implement test-error
   ;;     like the following. so no help. 
   #;
   (larceny
    (import (except (srfi 64) test-error))
    (begin
      (define-syntax %test-error
	(syntax-rules ()
	  ((%test-error etype expr)
	   (let ((t etype))
	     (when (procedure? t)
	       (test-result-set! (test-runner-get) 'expected-error t))
	     (guard (ex (else
			 (test-result-set! (test-runner-get) 'actual-error ex)
			 (if (procedure? t) (t ex) #t)))
	       expr
	       #f)))))

      (define-syntax test-error
	(syntax-rules ()
	  ((test-error name etype expr)
	   (test-assert name (%test-error etype expr)))
	  ((test-error etype expr)
	   (test-assert (%test-error etype expr)))
	  ((test-error expr)
	   (test-assert (%test-error #t expr)))))))

   ((and (not larceny) (library (srfi 64))) (import (srfi 64)))
   ((library (chibi test))
    (import (rename (except (chibi test) test-equal)
		    (test-error %test-error))
	    (scheme base))
    (begin
      (define-syntax test-error
	(syntax-rules ()
	  ((_ expr) (test-error 'expr test? expr))
	  ((_ name expr) (test-error name test? expr))
	  ((_ name test expr) (%test-error name expr))))
      (define-syntax test-equal
	(syntax-rules ()
	  ((_ name expect expr)
	   (test name expect expr))
	  ((_ expect expr)
	   (test-equal 'expr expect expr))))))
   ((library (gauche test))
    (import (scheme base)
	    (rename (gauche test)
		    (test-error %test-error)
		    (test-start test-begin)
		    (test-section test-group)))
    (begin
      (define-syntax test-equal
	(syntax-rules ()
	  ((_ expect expr)
	   (test-equal 'expr expect expr))
	  ((_ name expect expr)
	   (test* name expect expr))))
      (define-syntax test-assert
	(syntax-rules ()
	  ((_ expr)
	   (test-assert 'expr expr))
	  ((_ name expr)
	   (test* name #t (and expr #t)))))
      (define (test-error-compare? e r) (test-error? r))
      (define-syntax test-error
	(syntax-rules ()
	  ((_ expr)
	   (test-error 'expr condition? expr))
	  ((_ name expr)
	   (test-error name condition? expr))
	  ((_ name test expr)
	   (test* name 'dummy expr test-error-compare?))))))
   (else
    (import (scheme base) (scheme write))
    (begin
      ;; TODO better report handling
      (define passed 0)
      (define failed 0)
      (define (test-begin . o) #f)
      (define (test-end . o)
	(display "Passed: ") (display passed) (newline)
	(display "Failed: ") (display failed) (newline)
	(display "Total: ") (display (+ passed failed)) (newline))
      (define (inc-passed!) (set! passed (+ passed 1)))
      (define (inc-failed!) (set! failed (+ failed 1)))
      (define-syntax test-assert
	(syntax-rules ()
	  ((_ expr) (test-assert 'expr expr))
	  ((_ name expr)
	   (guard (e (else 
		      (inc-failed!)
		      (display "FAIL: ") (write name) (newline)))
	     (and expr
		  (inc-passed!)
		  (display "PASS: ") (write name) (newline))))))
      (define-syntax test-error
	(syntax-rules ()
	  ((_ expr) (test-assert 'expr expr))
	  ((_ name expr)
	   (guard (e (else 
		      (inc-passed!)
		      (display "PASS: ") (write name) (newline)))
	     expr
	     (inc-failed!)
	     (display "FAIL: ") (write name) (newline)))))
      (define-syntax test-equal
	(syntax-rules ()
	  ((_ expected expr) (test-equal 'expr expected expr))
	  ((_ name expected expr)
	   (let ((res expr))
	     (cond ((equal? res expected)
		    (inc-passed!)
		    (display "PASS: ") (write name) (newline))
		   (else
		    (inc-failed!)
		    (display "FAIL: ") (display name) (newline)
		    (display "   expected ") (write expected) (newline)
		    (display "   but got ")  (write res) (newline))))))))))

   )