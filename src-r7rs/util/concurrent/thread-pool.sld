;; -*- mode:scheme; coding: utf-8; -*-
;; Automatically generated. DON'T EDIT!
(define-library
  (util concurrent thread-pool)
  (export
    make-thread-pool
    thread-pool?
    <thread-pool>
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
    thread-pool-current-thread-id)
  (import
    (except (scheme base) define-record-type)
    (except (srfi 18) raise with-exception-handler)
    (srfi 39)
    (util concurrent shared-queue)
    (util concurrent thread-pool compat))
  (begin
    (define (thread-pool-current-thread-id)
      (*thread-pool-current-thread-id*))
    (define (thread-pool-thread tp id)
      (vector-ref (<thread-pool>-threads tp) id))
    (define (thread-pool-thread-id tp thread)
      (define threads (<thread-pool>-threads tp))
      (define size (vector-length threads))
      (let loop ((i 0))
        (cond ((= i size)
               (error 'thread-pool-thread-id
                      "not a managed thread"
                      thread))
              ((eq? (vector-ref threads i) thread) i)
              (else (loop (+ i 1))))))
    (define (thread-pool-size tp)
      (vector-length (<thread-pool>-threads tp)))
    (define (thread-pool-idling-count tp)
      (shared-queue-size (<thread-pool>-idlings tp)))
    (define (thread-pool-idling? tp)
      (not (zero? (thread-pool-idling-count tp))))
    (define (thread-pool-push-task! tp task . opt)
      (define (default-handler n)
        (if (negative? n) 0 n))
      (define (find-available tp add-to-back?)
        (let* ((threads (<thread-pool>-threads tp))
               (queue (<thread-pool>-queues tp))
               (size (vector-length threads)))
          (let loop ((i 0) (maybe -1) (qsize +inf.0))
            (if (= i size)
              (add-to-back? maybe)
              (let ((t (vector-ref threads i))
                    (s (shared-queue-size (vector-ref queue i))))
                (cond ((and (add-to-back? i) (< s qsize))
                       (loop (+ i 1) i s))
                      (else (loop (+ i 1) maybe qsize))))))))
      (let ((where (or (and (thread-pool-idling? tp)
                            (shared-queue-get! (<thread-pool>-idlings tp)))
                       (find-available
                         tp
                         (if (null? opt) default-handler (car opt))))))
        (shared-queue-put!
          (vector-ref (<thread-pool>-queues tp) where)
          task)
        where))
    (define (thread-pool-wait-all! tp)
      (define (wait-queue tp)
        (define queus
          (vector->list (<thread-pool>-queues tp)))
        (let loop ()
          (unless
            (for-all shared-queue-empty? queus)
            (thread-yield!)
            (thread-sleep! 0.1)
            (loop))))
      (define (wait-threads tp)
        (define size (thread-pool-size tp))
        (let loop ()
          (unless
            (= (thread-pool-idling-count tp) size)
            (thread-yield!)
            (thread-sleep! 0.1)
            (loop))))
      (wait-queue tp)
      (wait-threads tp))
    (define (thread-pool-release! tp . opt)
      (let ((type (if (null? opt) 'join (car opt))))
        (vector-for-each
          (lambda (q) (shared-queue-put! q #f))
          (<thread-pool>-queues tp))
        (vector-for-each
          (lambda (t)
            (case type
              ((terminate) (thread-terminate! t))
              (else (thread-join! t))))
          (<thread-pool>-threads tp))))
    (define (thread-pool-thread-terminate! tp id)
      (define threads (<thread-pool>-threads tp))
      (define queues (<thread-pool>-queues tp))
      (define idlings (<thread-pool>-idlings tp))
      (let ((t (vector-ref threads id))
            (q (vector-ref queues id))
            (nq (make-shared-queue)))
        (shared-queue-clear! q)
        (shared-queue-locked? idlings #t)
        (thread-terminate! t)
        (vector-set! queues id nq)
        (vector-set!
          threads
          id
          (thread-start!
            (make-thread
              (make-executor
                idlings
                id
                nq
                (<thread-pool>-error-handler tp)))))))
    (define (thread-pool-thread-task-running? tp id)
      (not (shared-queue-find (<thread-pool>-idlings tp) id)))))
