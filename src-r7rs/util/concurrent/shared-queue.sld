;; -*- mode:scheme; coding: utf-8; -*-
;; Automatically generated. DON'T EDIT!
(define-library
  (util concurrent shared-queue)
  (export
    shared-queue?
    make-shared-queue
    <shared-queue>
    shared-queue-empty?
    shared-queue-size
    shared-queue-max-length
    shared-queue-overflows?
    shared-queue-put!
    shared-queue-get!
    shared-queue-clear!
    shared-queue-find
    shared-queue-locked?
    shared-priority-queue?
    make-shared-priority-queue
    <shared-priority-queue>
    shared-priority-queue-empty?
    shared-priority-queue-size
    shared-priority-queue-max-length
    shared-priority-queue-overflows?
    shared-priority-queue-put!
    shared-priority-queue-get!
    shared-priority-queue-remove!
    shared-priority-queue-clear!
    shared-priority-queue-locked?)
  (import
    (except (scheme base) define-record-type)
    (srfi 18)
    (util concurrent shared-queue compat))
  (begin
    (define (shared-queue-empty? sq)
      (null? (shared-queue-head sq)))
    (define (shared-queue-get! sq . maybe-timeout)
      (let ((timeout
              (if (pair? maybe-timeout) (car maybe-timeout) #f))
            (timeout-value
              (if (and (pair? maybe-timeout)
                       (pair? (cdr maybe-timeout)))
                (cadr maybe-timeout)
                #f)))
        (mutex-lock! (%lock sq))
        (%w-set! sq (+ (%w sq) 1))
        (condition-variable-broadcast! (%write-cv sq))
        (let loop ()
          (cond ((shared-queue-empty? sq)
                 (cond ((mutex-unlock! (%lock sq) (%read-cv sq) timeout)
                        (mutex-lock! (%lock sq))
                        (loop))
                       (else (%w-set! sq (- (%w sq) 1)) timeout-value)))
                (else
                 (%w-set! sq (- (%w sq) 1))
                 (let ((head (shared-queue-head sq)))
                   (shared-queue-head-set! sq (cdr head))
                   (when (null? (cdr head))
                         (shared-queue-tail-set! sq '()))
                   (shared-queue-size-set!
                     sq
                     (- (shared-queue-size sq) 1))
                   (mutex-unlock! (%lock sq))
                   (car head)))))))
    (define (shared-queue-put! sq obj . maybe-timeout)
      (let ((timeout
              (if (pair? maybe-timeout) (car maybe-timeout) #f))
            (timeout-value
              (if (and (pair? maybe-timeout)
                       (pair? (cdr maybe-timeout)))
                (cadr maybe-timeout)
                #f)))
        (mutex-lock! (%lock sq))
        (let loop ()
          (cond ((if (zero? (shared-queue-max-length sq))
                   (zero? (%w sq))
                   (shared-queue-overflows? sq 1))
                 (cond ((mutex-unlock! (%lock sq) (%write-cv sq) timeout)
                        (mutex-lock! (%lock sq))
                        (loop))
                       (else timeout-value)))
                (else
                 (let ((new (list obj)) (tail (shared-queue-tail sq)))
                   (shared-queue-tail-set! sq new)
                   (if (pair? tail)
                     (set-cdr! tail new)
                     (shared-queue-head-set! sq new))
                   (shared-queue-size-set!
                     sq
                     (+ (shared-queue-size sq) 1))
                   (when (> (%w sq) 0)
                         (condition-variable-broadcast! (%read-cv sq)))
                   (mutex-unlock! (%lock sq))
                   obj))))))
    (define (shared-queue-overflows? sq count)
      (and (>= (shared-queue-max-length sq) 0)
           (> (+ count (shared-queue-size sq))
              (shared-queue-max-length sq))))
    (define (shared-queue-clear! sq)
      (mutex-lock! (%lock sq))
      (shared-queue-size-set! sq 0)
      (shared-queue-head-set! sq '())
      (shared-queue-tail-set! sq '())
      (mutex-unlock! (%lock sq)))
    (define (shared-queue-find sq proc)
      (dynamic-wind
        (lambda () (mutex-lock! (%lock sq)))
        (lambda () (find proc (shared-queue-head sq)))
        (lambda () (mutex-unlock! (%lock sq)))))
    (define (shared-queue-locked? sq . maybe-wait?)
      (let ((wait? (if (null? maybe-wait?) #f (car maybe-wait?))))
        (and (thread? (mutex-state (%lock sq)))
             (if wait?
               (begin
                 (mutex-lock! (%lock sq))
                 (mutex-unlock! (%lock sq))
                 #f)
               #t))))
    (define (shared-priority-queue-empty? spq)
      (zero? (shared-priority-queue-size spq)))
    (define (shared-priority-queue-put!
             spq
             o
             .
             maybe-timeout)
      (define size (shared-priority-queue-size spq))
      (define (grow! spq min-capacity)
        (define old (%spq-es spq))
        (let* ((capacity
                 (+ size (if (< size 64) (+ size 2) (div size 2))))
               (new (make-vector capacity)))
          (do ((i 0 (+ i 1)))
              ((= i size))
            (vector-set! new i (vector-ref old i)))
          (%spq-es-set! spq new)))
      (define timeout
        (if (pair? maybe-timeout) (car maybe-timeout) #f))
      (define timeout-value
        (if (and (pair? maybe-timeout)
                 (pair? (cdr maybe-timeout)))
          (cadr maybe-timeout)
          #f))
      (mutex-lock! (%spq-lock spq))
      (let loop ()
        (cond ((if (zero? (shared-priority-queue-max-length spq))
                 (zero? (%spq-w spq))
                 (shared-priority-queue-overflows? spq 1))
               (cond ((mutex-unlock!
                        (%spq-lock spq)
                        (%spq-wcv spq)
                        timeout)
                      (mutex-lock! (%spq-lock spq))
                      (loop))
                     (else timeout-value)))
              (else
               (when (>= size (vector-length (%spq-es spq)))
                     (grow! spq (+ size 1)))
               (if (zero? size)
                 (vector-set! (%spq-es spq) 0 o)
                 (shift-up spq size o))
               (%spq-size-set! spq (+ size 1))
               (when (> (%spq-w spq) 0)
                     (condition-variable-broadcast! (%spq-cv spq)))
               (mutex-unlock! (%spq-lock spq))
               o))))
    (define (shared-priority-queue-get! spq . maybe-timeout)
      (let ((timeout
              (if (pair? maybe-timeout) (car maybe-timeout) #f))
            (timeout-value
              (if (and (pair? maybe-timeout)
                       (pair? (cdr maybe-timeout)))
                (cadr maybe-timeout)
                #f)))
        (mutex-lock! (%spq-lock spq))
        (%spq-w-set! spq (+ (%spq-w spq) 1))
        (condition-variable-broadcast! (%spq-wcv spq))
        (let loop ()
          (cond ((shared-priority-queue-empty? spq)
                 (cond ((mutex-unlock!
                          (%spq-lock spq)
                          (%spq-cv spq)
                          timeout)
                        (mutex-lock! (%spq-lock spq))
                        (loop))
                       (else
                        (%spq-w-set! spq (- (%spq-w spq) 1))
                        timeout-value)))
                (else
                 (%spq-w-set! spq (- (%spq-w spq) 1))
                 (%spq-size-set!
                   spq
                   (- (shared-priority-queue-size spq) 1))
                 (let* ((s (shared-priority-queue-size spq))
                        (es (%spq-es spq))
                        (r (vector-ref es 0))
                        (x (vector-ref es s)))
                   (vector-set! es s #f)
                   (unless (zero? s) (shift-down spq 0 x))
                   (mutex-unlock! (%spq-lock spq))
                   r))))))
    (define (shared-priority-queue-remove! spq o)
      (define cmp (shared-priority-queue-compare spq))
      (define es (%spq-es spq))
      (define (find)
        (define len (shared-priority-queue-size spq))
        (let loop ((i 0))
          (cond ((= i len) -1)
                ((zero? (cmp o (vector-ref es i))) i)
                (else (loop (+ i 1))))))
      (define (remove-at spq index)
        (%spq-size-set!
          spq
          (- (shared-priority-queue-size spq) 1))
        (let ((s (shared-priority-queue-size spq)))
          (if (= s index)
            (vector-set! es s #f)
            (let ((moved (vector-ref es s)))
              (vector-set! es s #f)
              (shift-down spq index moved)
              (when (eq? (vector-ref es index) moved)
                    (shift-up spq index moved)))))
        (mutex-unlock! (%spq-lock spq)))
      (mutex-lock! (%spq-lock spq))
      (let ((index (find)))
        (if (>= index 0)
          (begin (remove-at spq index) #t)
          (begin (mutex-unlock! (%spq-lock spq)) #f))))
    (define (shift-up spq index o)
      (define cmp (shared-priority-queue-compare spq))
      (define es (%spq-es spq))
      (let loop ((k index))
        (if (> k 0)
          (let* ((parent (div (- k 1) 2))
                 (e (vector-ref es parent)))
            (if (>= (cmp o e) 0)
              (vector-set! es k o)
              (begin (vector-set! es k e) (loop parent))))
          (vector-set! es k o))))
    (define (shift-down spq k x)
      (define cmp (shared-priority-queue-compare spq))
      (define es (%spq-es spq))
      (define size (shared-priority-queue-size spq))
      (define half (div size 2))
      (let loop ((k k))
        (if (< k half)
          (let* ((child (+ (* k 2) 1))
                 (o (vector-ref es child))
                 (right (+ child 1)))
            (let-values
              (((o child)
                (if (and (< right size)
                         (> (cmp o (vector-ref es right)) 0))
                  (values (vector-ref es right) right)
                  (values o child))))
              (if (<= (cmp x o) 0)
                (vector-set! es k x)
                (begin (vector-set! es k o) (loop child)))))
          (vector-set! es k x))))
    (define (shared-priority-queue-overflows? spq count)
      (and (>= (shared-priority-queue-max-length spq) 0)
           (> (+ count (shared-priority-queue-size spq))
              (shared-priority-queue-max-length spq))))
    (define (shared-priority-queue-clear! spq)
      (mutex-lock! (%spq-lock spq))
      (do ((len (shared-priority-queue-size spq))
           (es (%spq-es spq))
           (i 0 (+ i 1)))
          ((= i len)
           (%spq-size-set! spq 0)
           (mutex-unlock! (%spq-lock spq)))
        (vector-set! es i #f)))
    (define (shared-priority-queue-locked? sq . maybe-wait?)
      (let ((wait? (if (null? maybe-wait?) #f (car maybe-wait?))))
        (and (thread? (mutex-state (%spq-lock sq)))
             (if wait?
               (begin
                 (mutex-lock! (%spq-lock sq))
                 (mutex-unlock! (%spq-lock sq))
                 #f)
               #t))))))
