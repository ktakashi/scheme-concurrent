(import (rnrs) 
	(util file)
	(srfi :26)
	(pp))

(define *prelude*
  '(
    (import (rnrs) (only (srfi :1) iota) 
	    (except (srfi :18) raise with-exception-handler)
	    (util concurrent) (time))
    
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
	   counts)))
  )

(define *prologue*
  '(
    (for-each (lambda (n*) (time (run n*))) n**))
  )

(define (dump out exprs)
  (for-each (lambda (e) (pp e out)) exprs))

(define (convert source out)
  (display ";; -*- mode: scheme -*-" out) (newline out)
  (dump out *prelude*)
  (dump out source)
  (dump out *prologue*))

(define (main args)
  (let ((source (file->sexp-list (cadr args)))
	(out (caddr args)))
    (when (file-exists? out) (delete-file out))
    (call-with-output-file out (cut convert source <>))))
  
