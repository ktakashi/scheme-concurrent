all: sagittarius guile

sagittarius:
	sash -L ../src only-thread.scm
	sash -L ../src  managed-thread.scm
	sash -L ../src  thread-pool.scm
	@echo done

guile:
	guile -q -L ../lib/guile -x .sls only-thread.scm
	guile -q -L ../lib/guile -x .sls managed-thread.scm
	guile -q -L ../src -L ../lib/guile -x .sls thread-pool.scm
	@echo done
