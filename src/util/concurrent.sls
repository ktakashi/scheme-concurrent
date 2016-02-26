;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; util/concurrent.sls - Thread pool
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

#!r6rs
(library (util concurrent)
    (export shared-queue? make-shared-queue <shared-queue>
	    shared-queue-empty? shared-queue-size
	    shared-queue-max-length
	    shared-queue-overflows?
	    shared-queue-put! shared-queue-get!
	    shared-queue-clear!
	    shared-queue-find
	    shared-queue-locked?

	    shared-priority-queue? make-shared-priority-queue
	    <shared-priority-queue>
	    shared-priority-queue-empty? shared-priority-queue-size
	    shared-priority-queue-max-length
	    shared-priority-queue-overflows?
	    shared-priority-queue-put! shared-priority-queue-get!
	    shared-priority-queue-remove!
	    shared-priority-queue-clear!
	    shared-priority-queue-locked?

	    ;; thread-pool
	    make-thread-pool thread-pool? <thread-pool>
	    thread-pool-size
	    thread-pool-idling-count
	    thread-pool-idling?
	    thread-pool-push-task!
	    thread-pool-wait-all!
	    thread-pool-release!
	    
	    thread-pool-thread-terminate!
	    thread-pool-thread
	    thread-pool-thread-id
	    thread-pool-thread-task-running?
	    thread-pool-current-thread-id

	    ;; future
	    <future> future?
	    future class
	    future-get future-cancel
	    future-done? future-cancelled?

	    &future-terminated future-terminated? terminated-future
	    future-state
	    <simple-future> make-simple-future simple-future?

	    ;; executor
	    <executor> executor?
	    executor-state
	    executor-available?
	    shutdown-executor!
	    execute-future!

	    <thread-pool-executor> make-thread-pool-executor 
	    thread-pool-executor?
	    thread-pool-executor-pool-size
	    thread-pool-executor-max-pool-size
	    thread-pool-executor-available?
	    thread-pool-executor-execute-future!
	    thread-pool-executor-shutdown!

	    abort-rejected-handler
	    terminate-oldest-handler
	    wait-finishing-handler
	    push-future-handler
	    
	    &rejected-execution-error rejected-execution-error?
	    rejected-future rejected-executor

	    <fork-join-executor> make-fork-join-executor
	    fork-join-executor?
	    fork-join-executor-available?
	    fork-join-executor-execute-future!
	    fork-join-executor-shutdown!

	    duplicate-executor-registration?
	    duplicate-executor-rtd

	    <executor-future> make-executor-future executor-future?

	    register-executor-methods)
    (import (only (rnrs) define-syntax identifier-syntax)
	    (util concurrent shared-queue)
	    (util concurrent thread-pool)
	    (rename (util concurrent future) (future future:future))
	    (util concurrent executor))
;; for Guile workaround
(define-syntax future (identifier-syntax future:future))
)
