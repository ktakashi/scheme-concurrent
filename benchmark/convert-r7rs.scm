(import (rnrs)
	(util file)
	(srfi :26)
	(match)
	(pp))

;; simple one is enough
(define fold-left%
  '(define (fold-left proc seed lst1)
     (let loop ((lis lst1) (knil seed))
       (if (null? lis) knil (loop (cdr lis) (proc knil (car lis)))))))

(define (->r7rs-import imports)
  (define (normalize import)
    (map (lambda (part)
	   (cond ((equal? part '(rnrs)) '(scheme base))
		 ((pair? part) (normalize part))
		 ;; must only be for srfi thing
		 ((keyword? part) (string->number (keyword->string part)))
		 (else
		  (let ((s (symbol->string part)))
		    (if (char=? (string-ref s 0) #\:)
			(string->number (string-copy s 1))
			part)))))
	 import))
  (let loop ((imports imports) (r '()))
    (cond ((null? imports) (reverse! r))
	  ((equal? (car imports) '(rnrs))
	   (loop (cdr imports) 
		 (cons '(scheme file) 
		       (cons '(scheme read)
			     (cons '(scheme base) r)))))
	  ((equal? (car imports) '(rnrs mutable-pairs))
	   (loop (cdr imports) r))
	  (else (loop (cdr imports) (cons (normalize (car imports)) r))))))

(define (convert source out)
  (define convert-it
    (match-lambda
     (('import specs ...)
      (pp `(import ,@(->r7rs-import specs)) out)
      (pp fold-left% out)
      (pp '(define mod modulo) out))
     (this (pp this out))))
  (display ";; -*- mode: scheme -*-" out) (newline out)
  (for-each convert-it source))

;; called from Makefile
;; convert-r7rs.scm in out
(define (main args) 
  (let ((source (file->sexp-list (cadr args)))
	(out (caddr args)))
    (when (file-exists? out) (delete-file out))
    (call-with-output-file out (cut convert source <>))))

