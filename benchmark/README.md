This directory contains benchmarks of fork-join, manual thread management
and thread-pool. Each script executes configured numbers of tasks (`fib`).
By default it's 100, 1000 and 10000. See `counts` file.

One of the iteration result (environment: Ubuntu 14.04,
Intel® Core™ i5-2520M CPU @ 2.50GHz × 4)

Of Sagittarius:

```
only-thread.scm

;;  (run count n*)
;;  0.179345 real    0.669790 user    0.000000 sys

;;  (run count n*)
;;  1.814083 real    6.944284 user    0.036162 sys

;;  (run count n*)
;;  19.771697 real    72.585444 user    0.534883 sys
managed-thread.scm

;;  (run count n*)
;;  0.217348 real    0.626586 user    0.004360 sys

;;  (run count n*)
;;  2.313352 real    6.100025 user    0.062993 sys

;;  (run count n*)
;;  25.991337 real    61.319317 user    0.494629 sys
thread-pool.scm

;;  (run count n*)
;;  0.296410 real    0.690976 user    0.000000 sys

;;  (run count n*)
;;  1.908583 real    6.595167 user    0.015673 sys

;;  (run count n*)
;;  18.062681 real    67.95119 user    0.035911 sys
executor.scm

;;  (run count n*)
;;  0.321831 real    0.651005 user    0.004029 sys

;;  (run count n*)
;;  2.188450 real    6.584105 user    0.003298 sys

;;  (run count n*)
;;  21.440668 real    65.512636 user    0.123994 sys
done
```

Of Guile:

```
only-thread.scm
clock utime stime cutime cstime gctime
 0.29  0.90  0.02   0.00   0.00   0.13
clock utime stime cutime cstime gctime
 3.22  8.77  0.09   0.00   0.00   0.13
clock utime stime cutime cstime gctime
28.38 87.95  3.10   0.00   0.00   0.31
 
managed-thread.scm
clock utime stime cutime cstime gctime
 0.27  0.69  0.02   0.00   0.00   0.10
clock utime stime cutime cstime gctime
 2.70  6.56  0.13   0.00   0.00   0.29
clock utime stime cutime cstime gctime
24.82 66.39  1.02   0.00   0.00   0.42
 
thread-pool.scm
clock utime stime cutime cstime gctime
 0.24  0.84  0.00   0.00   0.00   0.04
clock utime stime cutime cstime gctime
 2.56  8.11  0.01   0.00   0.00   0.04
clock utime stime cutime cstime gctime
21.33 81.66  0.11   0.00   0.00   1.10

executor.scm
clock utime stime cutime cstime gctime
 0.25  0.75  0.01   0.00   0.00   0.04
clock utime stime cutime cstime gctime
 2.44  7.38  0.02   0.00   0.00   0.23
clock utime stime cutime cstime gctime
24.58 74.28  0.53   0.00   0.00   3.11
done
```

Of Gauche:

```
only-thread.scm7
;(time (run count n*))
; real   0.190
; user   0.690
; sys    0.000
;(time (run count n*))
; real   1.780
; user   6.820
; sys    0.030
;(time (run count n*))
; real  18.129
; user  67.780
; sys    0.470

managed-thread.scm7
;(time (run count n*))
; real   0.231
; user   0.610
; sys    0.000
;(time (run count n*))
; real   2.286
; user   6.170
; sys    0.050
;(time (run count n*))
; real  22.487
; user  62.030
; sys    0.460

thread-pool.scm7
;(time (run count n*))
; real   0.192
; user   0.680
; sys    0.000
;(time (run count n*))
; real   1.794
; user   6.920
; sys    0.000
;(time (run count n*))
; real  17.867
; user  69.960
; sys    0.010

executor.scm7
;(time (run count n*))
; real   0.219
; user   0.660
; sys    0.000
;(time (run count n*))
; real   2.217
; user   6.580
; sys    0.010
;(time (run count n*))
; real  21.584
; user  65.600
; sys    0.120
done
```

Of Chibi:

```
only-thread.scm7
(run count n*): 545 ms
(run count n*): 5820 ms
(run count n*): 64945 ms

managed-thread.scm7
(run count n*): 438 ms
(run count n*): 4487 ms
(run count n*): 44951 ms

thread-pool.scm7
(run count n*): 610 ms
(run count n*): 5307 ms
(run count n*): 53893 ms

executor.scm7
(run count n*): 572 ms
;... didn't stop. not sure which side of bug
```


Depending on the implementations but using thread pool on heavy load of 
iteration makes better performance (max 60%). Manual thread management
seems have least user time. According to the Guile result, thread-pool
has the largest GC time.
