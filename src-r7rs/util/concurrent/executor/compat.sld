;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; r7rs/util/concurrent/executor/compat.sld - Compatible layer for R7RS
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

(define-library (util concurrent executor compat)
  (export <executor-future> make-executor-future executor-future?

	  <executor> executor? executor-state executor-state-set!
	  <thread-pool-executor>
	  make-thread-pool-executor thread-pool-executor?
	  executor-pool-ids 
	  executor-pool 
	  executor-rejected-handler
	  executor-mutex

	  <fork-join-executor> make-fork-join-executor fork-join-executor?

	  abort-rejected-handler default-rejected-handler

	  &rejected-execution-error
	  &duplicate-executor-registration
	  make-rejected-execution-error rejected-execution-error?
	  rejected-future rejected-executor
	  make-duplicate-executor-registration
	  duplicate-executor-registration? duplicate-executor-rtd
	  ;; rather required ...
	  condition make-message-condition make-who-condition
	  record-type-descriptor record-predicate

	  cadar caddr cadddr assertion-violation
	  )
  (import (except (scheme base) define-record-type)
	  (scheme cxr)
	  (srfi 18)
	  (srfi 117)
	  (util concurrent shared-queue)
	  (util concurrent future)
	  (util concurrent thread-pool))
  (cond-expand
   ((library (srfi 99)) (import (srfi 99)))
   (else (error '(util concurrent) "SRFI-99 is required")))
  (begin
    (define-record-type (<executor-future> <future>)
      %make-executor-future
      executor-future?)
    (define (not-started dummy)
      (error "executor-future: Future is not started yet" dummy))
    (define make-executor-future
      (lambda (thunk) (%make-executor-future thunk not-started 'created #f)))
      
   (define-record-type <executor> %%make-executor executor?
     (state executor-state executor-state-set!))
   ;; (define %make-executor (lambda () (%%make-executor 'running)))
   (define-record-type (<thread-pool-executor> <executor>)
     %make-thread-pool-executor
     thread-pool-executor?
     (pool-ids executor-pool-ids)
     (pool executor-pool)
     (rejected-handler executor-rejected-handler)
     (mutex executor-mutex))
   (define (abort-rejected-handler future executor)
     (error "executor: Failed to add task" future executor))
   (define default-rejected-handler abort-rejected-handler)
   (define make-thread-pool-executor
     (lambda (max-pool-size . rest)
       (%make-thread-pool-executor 'running
				   (make-list-queue '())
				   (make-thread-pool max-pool-size)
				   (if (null? rest)
				       default-rejected-handler
				       (car rest))
				   (make-mutex))))
    (define-record-type (<fork-join-executor> <executor>)
      %make-fork-join-executor
      fork-join-executor?)
    (define make-fork-join-executor 
      (lambda () (%make-fork-join-executor 'running)))
    ;; Hope this works on all implementation of SRFI-99 and SRFI-131
    (define record-type-descriptor values)
    (define record-predicate rtd-predicate)

    (define (assertion-violation who msg . irr) (apply error msg irr))
    )
  (cond-expand
   ;; Seems this would step on Gauche's bug...
   #;((library (srfi 35)) 
    (import (rename (srfi 35) (condition %condition)))
    (begin 
      (define-condition-type &rejected-execution-error &error
	rejected-execution-error?
	(future rejected-future)
	(executor rejected-executor))
      (define (make-rejected-execution-error f e)
	(%condition (&rejected-execution-error (future f) (executor e))))

      (define-condition-type &duplicate-executor-registration &error
	duplicate-executor-registration?
	(rtd duplicate-executor-rtd))
      (define (make-duplicate-executor-registration rtd)
	(%condition (&duplicate-executor-registration (rtd rtd))))
      (define (make-message-condition msg)
	(%condition (&message (message msg))))
      (define (make-who-condition who) (%condition (&who (who who))))
      (define condition make-compound-condition)
      ))
   (else 
    (begin
      ;; yuck!
      (define-syntax &rejected-execution-error (syntax-rules ()))
      (define-syntax &duplicate-executor-registration (syntax-rules ()))
      (define (make-rejected-execution-error f e) (vector f e))
      (define (rejected-execution-error? c) #f)
      (define (rejected-future c) #f)
      (define (rejected-executor c) #f)
      (define (make-duplicate-executor-registration r) (vector r))
      (define (duplicate-executor-registration? c) #f)
      (define (duplicate-executor-rtd c) #f)
      (define (condition . args) args)
      (define (make-message-condition msg) (vector msg))
      (define (make-who-condition who) (vector who)))
    ))

  )
