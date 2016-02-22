;; -*- mode:scheme; coding: utf-8; -*-
;; Automatically generated. DON'T EDIT!
(define-library
  (util concurrent executor)
  (export
    <executor>
    executor?
    executor-state
    executor-available?
    shutdown-executor!
    execute-future!
    <thread-pool-executor>
    make-thread-pool-executor
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
    &rejected-execution-error
    rejected-execution-error?
    rejected-future
    rejected-executor
    <fork-join-executor>
    make-fork-join-executor
    fork-join-executor?
    fork-join-executor-available?
    fork-join-executor-execute-future!
    fork-join-executor-shutdown!
    duplicate-executor-registration?
    duplicate-executor-rtd
    <executor-future>
    make-executor-future
    executor-future?
    register-executor-methods)
  (import
    (except (scheme base) define-record-type)
    (only (srfi 1) remove!)
    (except (srfi 18) raise with-exception-handler)
    (srfi 117)
    (util concurrent shared-queue)
    (util concurrent future)
    (util concurrent thread-pool)
    (util concurrent executor compat))
  (begin
    (define (terminate-oldest-handler future executor)
      (define (get-oldest executor)
        (with-atomic
          executor
          (let ((l (executor-pool-ids executor)))
            (and (not (list-queue-empty? l))
                 (list-queue-remove-front! l)))))
      (let ((oldest (get-oldest executor)))
        (and oldest
             (with-atomic
               executor
               (thread-pool-thread-terminate!
                 (executor-pool executor)
                 (car oldest)))
             (cleanup executor (cdr oldest) 'terminated)))
      (execute-future! executor future))
    (define (wait-finishing-handler wait-retry)
      (lambda (future executor)
        (thread-sleep! 0.1)
        (let loop ((count 0))
          (cond ((executor-available? executor)
                 (execute-future! executor future))
                ((= count wait-retry)
                 (abort-rejected-handler future executor))
                (else (thread-sleep! 0.5) (loop (+ count 1)))))))
    (define (thread-pool-executor-pool-size executor)
      (length
        (list-queue-list (executor-pool-ids executor))))
    (define (thread-pool-executor-max-pool-size executor)
      (thread-pool-size (executor-pool executor)))
    (define mutex-lock-recursively! mutex-lock!)
    (define mutex-unlock-recursively! mutex-unlock!)
    (define-syntax
      with-atomic
      (syntax-rules
        ()
        ((_ executor expr ...)
         (dynamic-wind
           (lambda ()
             (mutex-lock-recursively!
               (executor-mutex executor)))
           (lambda () expr ...)
           (lambda ()
             (mutex-unlock-recursively!
               (executor-mutex executor)))))))
    (define (cleanup executor future state)
      (define (remove-from-queue! proc queue)
        (list-queue-set-list!
          queue
          (remove! proc (list-queue-list queue))))
      (with-atomic
        executor
        (remove-from-queue!
          (lambda (o) (eq? (cdr o) future))
          (executor-pool-ids executor))
        (future-state-set! future state)))
    (define (thread-pool-executor-available? executor)
      (< (thread-pool-executor-pool-size executor)
         (thread-pool-executor-max-pool-size executor)))
    (define (thread-pool-executor-shutdown! executor)
      (define (executor-shutdown executor)
        (with-atomic
          executor
          (let ((state (executor-state executor)))
            (guard (e (else (executor-state-set! executor state)
                            (raise e)))
                   (when (eq? state 'running)
                         (executor-state-set! executor 'shutdown))
                   (thread-pool-release!
                     (executor-pool executor)
                     'terminate)
                   (list-queue-remove-all!
                     (executor-pool-ids executor))))))
      (for-each
        (lambda (future) (future-cancel (cdr future)))
        (executor-shutdown executor)))
    (define (thread-pool-executor-execute-future!
             executor
             future)
      (or (with-atomic
            executor
            (let ((pool-size
                    (thread-pool-executor-pool-size executor))
                  (max-pool-size
                    (thread-pool-executor-max-pool-size executor)))
              (and (< pool-size max-pool-size)
                   (eq? (executor-state executor) 'running)
                   (push-future-handler future executor))))
          (let ((reject-handler
                  (executor-rejected-handler executor)))
            (reject-handler future executor))))
    (define (push-future-handler future executor)
      (define (canceller future)
        (cleanup executor future 'terminated))
      (define (task-invoker thunk)
        (lambda ()
          (let ((q (future-result future)))
            (guard (e (else (future-canceller-set! future #t)
                            (cleanup executor future 'finished)
                            (shared-queue-put! q e)))
                   (let ((r (thunk)))
                     (cleanup executor future 'finished)
                     (shared-queue-put! q r))))))
      (define (add-future future executor)
        (future-result-set! future (make-shared-queue))
        (let* ((thunk (future-thunk future))
               (id (thread-pool-push-task!
                     (executor-pool executor)
                     (task-invoker thunk))))
          (future-canceller-set! future canceller)
          (list-queue-add-back!
            (executor-pool-ids executor)
            (cons id future))
          executor))
      (unless
        (eq? (executor-state executor) 'running)
        (assertion-violation
          'push-future-handler
          "executor is not running"
          executor))
      (if (eq? (mutex-state (executor-mutex executor))
               (current-thread))
        (add-future future executor)
        (with-atomic
          executor
          (add-future future executor))))
    (define (fork-join-executor-available? e)
      (unless
        (fork-join-executor? e)
        (assertion-violation
          'fork-join-executor-available?
          "not a fork-join-executor"
          e))
      #t)
    (define (fork-join-executor-execute-future! e f)
      (define (task-invoker thunk)
        (lambda ()
          (let ((q (future-result f)))
            (guard (e (else (future-canceller-set! f #t)
                            (future-state-set! f 'finished)
                            (shared-queue-put! q e)))
                   (let ((r (thunk)))
                     (future-state-set! f 'finished)
                     (shared-queue-put! q r))))))
      (unless
        (fork-join-executor? e)
        (assertion-violation
          'fork-join-executor-available?
          "not a fork-join-executor"
          e))
      (thread-start!
        (make-thread (task-invoker (future-thunk f))))
      f)
    (define (fork-join-executor-shutdown! e)
      (unless
        (fork-join-executor? e)
        (assertion-violation
          'fork-join-executor-available?
          "not a fork-join-executor"
          e))
      (executor-state-set! e 'shutdown))
    (define *registered-executors* '())
    (define *register-lock* (make-mutex))
    (define (%register-executor-methods
             rtd
             pred
             available?
             execute
             shutdown)
      (mutex-lock! *register-lock*)
      (when (assq rtd *registered-executors*)
            (mutex-unlock! *register-lock*)
            (raise (condition
                     (make-duplicate-executor-registration rtd)
                     (make-who-condition 'register-executor-methods)
                     (make-message-condition
                       "specified predicate is already registered"))))
      (set! *registered-executors*
        (cons (list rtd pred available? execute shutdown)
              *registered-executors*))
      (mutex-unlock! *register-lock*))
    (define-syntax
      register-executor-methods
      (syntax-rules
        ()
        ((_ type available? execute shutdown)
         (define dummy
           (let ((rtd (record-type-descriptor type)))
             (%register-executor-methods
               rtd
               (record-predicate rtd)
               available?
               execute
               shutdown))))))
    (define-syntax
      invoke-method
      (syntax-rules
        ()
        ((_ who pos fallback e args ...)
         (let loop ((methods *registered-executors*))
           (cond ((null? methods)
                  (if (executor? e)
                    fallback
                    (assertion-violation 'who "not an executor" e)))
                 (((cadar methods) e)
                  ((pos (cdar methods)) e args ...))
                 (else (loop (cdr methods))))))))
    (define (not-supported e)
      (error #f "given executor is not supported" e))
    (define (executor-available? e)
      (invoke-method executor-available? cadr #f e))
    (define (execute-future! e f)
      (invoke-method
        execute-future!
        caddr
        (not-supported e)
        e
        f))
    (define (shutdown-executor! e)
      (invoke-method
        shutdown-executor!
        cadddr
        (not-supported e)
        e))
    (register-executor-methods
      <thread-pool-executor>
      thread-pool-executor-available?
      thread-pool-executor-execute-future!
      thread-pool-executor-shutdown!)
    (register-executor-methods
      <fork-join-executor>
      fork-join-executor-available?
      fork-join-executor-execute-future!
      fork-join-executor-shutdown!)))
