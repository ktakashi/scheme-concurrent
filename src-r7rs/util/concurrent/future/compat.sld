;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; r7rs/util/concurrent/future/compat.sld - Compatible layer for R7RS
;;;  
;;;   Copyright (c) 2016  Takashi Kato  <ktakashi@ymail.com>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  

(define-library (util concurrent future compat)
  (export <future> future?
	  <simple-future> make-simple-future simple-future?
	  
	  future-thunk future-thunk-set!
	  future-result future-result-set!
	  future-state future-state-set!
	  future-canceller future-canceller-set!

	  &future-terminated make-future-terminated future-terminated?
	  terminated-future
	  condition make-message-condition make-who-condition
	  make-irritants-condition
	  record-constructor record-constructor-descriptor
	  )
  (import (except (scheme base) define-record-type)
	  (srfi 18)
	  (util concurrent shared-queue))
  (cond-expand
   ((library (srfi 99)) (import (srfi 99)))
   (else (error '(util concurrent) "SRFI-99 is required")))
  (begin
    (define-record-type <future> 
      %make-future 
      future?
      (thunk future-thunk future-thunk-set!)
      (result future-result future-result-set!)
      (state future-state future-state-set!)
      (canceller future-canceller future-canceller-set!))

    (define-record-type (<simple-future> <future>)
      %make-simple-future
      simple-future?)
    (define (simple-invoke thunk f q)
      (lambda ()
	(guard (e (else (future-canceller-set! f #t)
			(future-state-set! f 'finished)
			(shared-queue-put! q e)))
	  (let ((r (thunk)))
	    (future-state-set! f 'finished)
	    (shared-queue-put! q r)))))
    (define (make-simple-future thunk)
      (let ((q (make-shared-queue)))
	(let ((f (%make-simple-future thunk q 'created #f)))
	  (thread-start! (make-thread (simple-invoke thunk f q)))
	  f)))
    (define record-constructor-descriptor values)
    ;; this isn't a good one...
    (define (not-started dummy)
      (error "executor-future: Future is not started yet" dummy))
    (define (record-constructor rtd)
      (let ((rcd (rtd-constructor rtd)))
	(lambda (thunk)
	  (rcd thunk not-started 'created #f))))
    )
  (cond-expand
   ;; Seems this would step on Gauche's bug...
   #;((library (srfi 35)) 
    (import (rename (srfi 35) (condition %condition)))
    (begin 
      (define-condition-type &future-terminated &error
	future-terminated?
	(future terminated-future))
      (define (make-future-terminated f)
	(%condition (&future-terminated (future f))))

      (define-condition-type &irritants &condition
	irritants-condition?
	(irritants &irritants-irritants))
      (define (make-irritants-condition . irr)
	(%condition (&irritants-irritants (irritants irr))))
      (define (make-message-condition msg)
	(%condition (&message (message msg))))
      (define (make-who-condition who) (%condition (&who (who who))))
      (define condition make-compound-condition)
      ))
   (else 
    (begin
      ;; yuck!
      (define-syntax &future-terminated (syntax-rules ()))
      (define (make-future-terminated f) (vector f))
      (define (future-terminated? c) #f)
      (define (terminated-future c) #f)
      (define (condition . args) args)
      (define (make-irritants-condition . irr) (vector irr))
      (define (make-message-condition msg) (vector msg))
      (define (make-who-condition who) (vector who)))
    ))
)