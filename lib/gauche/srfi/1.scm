;; seems Gauche 0.9.4 doesn't export append!, list-copy, every and last-pair 
;; from srfi-1 (probaby lot more...)
;; so workaround it.
(define-module srfi.1 (extend srfi-1))
(select-module srfi.1)
(export append! last-pair list-copy every reverse!)
