;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; r7rs/util/concurrent/shared-queue/compat.sld - Compatible layer for R7RS
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

(define-library (util concurrent shared-queue compat)
  (export <shared-queue> make-shared-queue shared-queue?
	  shared-queue-head shared-queue-head-set!
	  shared-queue-tail shared-queue-tail-set!
	  shared-queue-size shared-queue-size-set!
	  shared-queue-max-length
	  ;; internal use only
	  %w %w-set!
	  %lock
	  %read-cv
	  %write-cv

	  <shared-priority-queue> make-shared-priority-queue 
	  shared-priority-queue?
	  %spq-es %spq-es-set!
	  shared-priority-queue-size %spq-size-set!
	  shared-priority-queue-compare
	  %spq-w %spq-w-set!
	  %spq-lock
	  %spq-cv

	  (rename quotient div)
	  find
	  )
  (import (except (scheme base) define-record-type)
	  (srfi 1)
	  (srfi 18))
  (cond-expand
   ((library (srfi 131)) (import (srfi 131)))
   ((library (srfi 99)) (import (srfi 99)))
   (else (error '(util concurrent) "SRFI-99 or SRFI-131 is required")))
  (begin
    (define-record-type <shared-queue> %make-shared-queue shared-queue?
      (head shared-queue-head shared-queue-head-set!)
      (tail shared-queue-tail shared-queue-tail-set!)
      (size shared-queue-size shared-queue-size-set!)
      (max-length shared-queue-max-length)
      (w %w %w-set!)
      (lock %lock)
      (read-cv %read-cv)
      (write-cv %write-cv))

    (define make-shared-queue
      (lambda maybe-max
	(let ((max-length (if (pair? maybe-max) (car maybe-max) -1)))
	  (unless (integer? max-length)
	    (error "make-shared-queue: max-length must be an integer"
		   max-length))
	  (%make-shared-queue '() '() 0 max-length 0
			      (make-mutex)
			      (make-condition-variable)
			      (make-condition-variable)))))

    (define-record-type <shared-priority-queue>
      %make-shared-priority-queue
      shared-priority-queue?
      (elements %spq-es %spq-es-set!)
      (size shared-priority-queue-size %spq-size-set!)
      (compare shared-priority-queue-compare)
      (w %spq-w %spq-w-set!)
      (lock %spq-lock)
      (cv %spq-cv))

    (define make-shared-priority-queue
      (lambda (compare . maybe-capacity)
	(let ((capacity (if (pair? maybe-capacity)
			    (car maybe-capacity)
			    10)))
	  (unless (integer? capacity)
	    (error "make-shared-priority-queue: capacity must be an integer"
		   capacity))
	  (%make-shared-priority-queue (make-vector capacity)
				       0
				       compare
				       0
				       (make-mutex)
				       (make-condition-variable)))))
    )
  )