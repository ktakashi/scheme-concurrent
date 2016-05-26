# executor doesn't work on chibi properly...
#FILES= only-thread.scm managed-thread.scm thread-pool.scm # executor.scm
FILES= executor.scm
R6RS_FILES= $(FILES:.scm=.sps)
R7RS_FILES= $(FILES:.scm=.scm7)


all: sagittarius guile gauche chibi

sagittarius: $(R6RS_FILES)
	@for f in $(R6RS_FILES); do echo $$f; sash -L ../src $$f; done
	@echo done

guile: $(R6RS_FILES)
	@for f in $(R6RS_FILES); do \
	  echo $$f; \
	  guile -q -L ../src -L ../lib/guile -x .sls $$f; \
	done
	@echo done

gauche: $(R7RS_FILES)
	@for f in $(R7RS_FILES); do \
	  echo $$f; \
	  gosh -r7 -e'(set! *load-suffixes* (cons ".sld" *load-suffixes*))' \
	        -I../src-r7rs -I../lib/gauche -I../lib/r7rs $$f; \
	done
	@echo done

chibi:  $(R7RS_FILES)
	@for f in $(R7RS_FILES); do \
	  echo $$f; \
	  chibi-scheme -I../src-r7rs -I../lib/r7rs -I../lib/chibi-scheme $$f; \
	done
	@echo done

%.sps: %.scm
	sash convert-r6rs.scm $< $@

%.scm7: %.sps
	sash convert-r7rs.scm $< $@
