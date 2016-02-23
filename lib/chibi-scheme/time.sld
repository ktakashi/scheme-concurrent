;; -*- mode:scheme -*-
(define-library (time)
  (export time)
  (import (chibi) (rename (chibi time) (time chibi:time)))
  (begin
    ;; Chibi 0.7.3 has a bug on time macro (missing quote on first expr)
    ;; it's fixed on the repository but we are using 0.7.3.
    (define-syntax time
      (syntax-rules ()
        ((time expr)
         (chibi:time (call-with-output-string 
		       (lambda (out) (write 'expr out))) expr))))
    )
)
