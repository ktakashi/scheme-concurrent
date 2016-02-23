FILES= only-thread.scm managed-thread.scm thread-pool.scm
R7RS_FILES= $(FILES:.scm=.scm7)


all: sagittarius guile gauche chibi

sagittarius:
	@for f in $(FILES); do edho $$f; sash -L ../src $$f; done
	@echo done

guile:
	@for f in $(FILES); do \
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
	  chibi-scheme -I../src-r7rs -I../lib/r7rs -I../lib/chibi $$f; \
	done
	@echo done

%.scm7: %.scm
	sash convert-r7rs.scm $< $@
