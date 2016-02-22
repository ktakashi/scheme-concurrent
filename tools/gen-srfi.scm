#!read-macro=sagittarius/regex
(import (rnrs)
	(sagittarius)
	(sagittarius regex)
	(match)
	(pp)
	(getopt)
	(util file))

(define (get-output-file&library name i)
  (case i
    ((guile)
     (let ((lib (match name
		  (('srfi name)
		   (let ((n (format "srfi-~a"
				    (string-copy (symbol->string name) 1))))
		     `(srfi ,(string->symbol n)))))))
       (values (build-path* "guile" (format "srfi/~a.sls" (cadr lib))) lib)))
    (else (error 'get-output-file&library "not supported"))))

(define (create-directory-of-file file)
  (let-values (((path base ext) (decompose-path file)))
    (create-directory* path)))

(define (main args)
  (with-args (cdr args)
      ((i (#\i "implementation") #t (usage))
       (d (#\d "dir") #t (usage)))
    (let ((files (glob (build-path* d "srfi" "*.sls")))
	  (implementation (string->symbol i)))
      (for-each (lambda (file)
		  (let ((library (call-with-input-file file read)))
		    (match library
		      (('library name 
			   ('export exports ...)
			   ('import imports ...)
			 body ...)
		       (let-values (((ofile lib) 
				     (get-output-file&library name 
							      implementation)))
			 (let ((ofile (build-path* d ofile)))
			   (when (file-exists? ofile) (delete-file ofile))
			   (create-directory-of-file ofile)
			   (call-with-output-file ofile
			     (lambda (out)
			       (display 
				";; Automatically generated. DON'T EDIT!" out)
			       (newline out)
			       (pp `(library ,lib
					(export ,@exports)
					(import ,@imports)
				      ,@body)
				   out)))))))))
		files))))
