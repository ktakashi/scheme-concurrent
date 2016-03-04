;; -*- mode:scheme; coding: utf-8; -*-
;; Automatically generated. DON'T EDIT!
(define-library
  (util concurrent future)
  (export
    <future>
    future?
    future
    class
    future-get
    future-cancel
    future-done?
    future-cancelled?
    &future-terminated
    future-terminated?
    terminated-future
    future-state
    future-state-set!
    future-thunk
    future-result
    future-result-set!
    future-canceller
    future-canceller-set!
    <simple-future>
    make-simple-future
    simple-future?
    make-shared-box
    shared-box?
    shared-box-put!
    shared-box-get!)
  (import
    (except (scheme base) define-record-type)
    (except (srfi 18) raise with-exception-handler)
    (util concurrent future compat))
  (begin
    (define-syntax class (syntax-rules ()))
    (define-syntax
      future
      (syntax-rules
        (class)
        ((_ (class cls) expr ...)
         ((record-constructor
            (record-constructor-descriptor cls))
          (lambda () expr ...)))
        ((_ expr ...)
         (make-simple-future (lambda () expr ...)))))
    (define (future-get future . opt)
      (define (finish r)
        (future-state-set! future 'done)
        (if (eqv? (future-canceller future) #t)
          (raise r)
          r))
      (when (eq? (future-state future) 'terminated)
            (raise-future-terminated future))
      (let ((state (future-state future)))
        (let ((r (future-result future)))
          (finish
            (cond ((and (not (eq? state 'done)) (shared-box? r))
                   (future-result-set!
                     future
                     (apply shared-box-get! r opt))
                   (future-result future))
                  ((and (not (eq? state 'done)) (procedure? r))
                   (future-result-set! future (apply r future opt))
                   (future-result future))
                  (else r))))))
    (define (future-cancel future)
      (unless
        (eq? (future-state future) 'done)
        (future-state-set! future 'terminated))
      (let ((c (future-canceller future)))
        (when (procedure? c)
              (c future)
              (future-canceller-set! future #f))))
    (define (future-done? future)
      (eq? (future-state future) 'done))
    (define (future-cancelled? future)
      (eq? (future-state future) 'terminated))))
