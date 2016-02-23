;; very unfortunate
(define-module srfi.18 (extend gauche.threads))
(select-module srfi.18)
;; seems 0.9.4 doesn't export current-thread from gauche.threads
(export current-thread)
