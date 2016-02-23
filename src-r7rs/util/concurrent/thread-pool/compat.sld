;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; r7rs/util/concurrent/thread-pool/compat.sld - Compatible layer for R7RS
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

(define-library (util concurrent thread-pool compat)
  (export <thread-pool> make-thread-pool thread-pool?
	  <thread-pool>-threads
	  <thread-pool>-queues
	  <thread-pool>-idlings
	  <thread-pool>-error-handler

	  default-error-handler 
	  *thread-pool-current-thread-id*
	  make-executor

	  for-all
	  ;; this doesn't work on Gauche 0.9.4
	  ;; (rename every for-all)
	  )
  (import (except (scheme base) define-record-type)
	  (rename (srfi 1) (every for-all))
	  (srfi 18)
	  (util concurrent shared-queue))
  (cond-expand
   ((library (srfi 131)) (import (srfi 131)))
   ((library (srfi 99)) (import (srfi 99)))
   (else (error '(util concurrent) "SRFI-99 or SRFI-131 is required")))
  (begin
    (define-record-type <thread-pool> %make-thread-pool thread-pool?
      (threads <thread-pool>-threads)
      (queues  <thread-pool>-queues)
      (idlings <thread-pool>-idlings)
      (error-handler <thread-pool>-error-handler))

    (define (default-error-handler e) #f)

    (define *thread-pool-current-thread-id* (make-parameter #f))

    (define (make-executor idlings i queue error-handler)
      (lambda ()
	(*thread-pool-current-thread-id* i)
	(let loop ()
	  (shared-queue-put! idlings i)
	  (let loop2 ((task (shared-queue-get! queue)))
	    (when task
	      (guard (e (else (error-handler e))) (task))
	      (if (shared-queue-empty? queue)
                  (loop)
                  (loop2 (shared-queue-get! queue))))))))
    (define make-thread-pool
      (lambda (n . maybe-error-handler)
	(let* ((threads (make-vector n))
	       (queues (make-vector n))
	       (idlings (make-shared-queue))
	       (error-handler
		(if (null? maybe-error-handler)
		    default-error-handler
		    (car maybe-error-handler)))
	       (tp (%make-thread-pool threads queues idlings error-handler)))
	  (do ((i 0 (+ i 1)))
	      ((= i n)
	       (let loop ()
		 (if (= n (shared-queue-size idlings))
                     tp
                     (begin
                       (thread-yield!)
                       (thread-sleep! 0.1)
                       (loop)))))
	    (let ((q (make-shared-queue)))
	      (vector-set! queues i q)
	      (vector-set! threads i
			   (thread-start!
			    (make-thread
			     (make-executor idlings i q error-handler)))))))))
    )
)
