all: sagittarius guile gauche chibi

sagittarius: sagittarius-r6rs sagittarius-r7rs

sagittarius-r6rs:
	sagittarius -r6 -Lsrc tests/concurrent.scm

sagittarius-r7rs:
	sagittarius -r7 -Lsrc-r7rs -Llib/r7rs-test tests/concurrent-r7rs.scm

guile:
	guile -L src -L lib/guile -x .sls tests/concurrent.scm

gauche:
	gosh -r7 -e '(set! *load-suffixes* (cons ".sld" *load-suffixes*))' \
	     -Isrc-r7rs -Ilib/r7rs -Ilib/gauche -Ilib/r7rs-test \
	     tests/concurrent-r7rs.scm

chibi:
	chibi-scheme -Isrc-r7rs -Ilib/r7rs-test -Ilib/r7rs \
	     tests/concurrent-r7rs.scm
