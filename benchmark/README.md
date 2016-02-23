This directory contains benchmarks of fork-join, manual thread management
and thread-pool. Each script executes configured numbers of tasks (`fib`).
By default it's 100, 1000 and 10000. See `counts` file.

One of the iteration result (environment: Ubuntu 14.04,
Intel® Core™ i5-2520M CPU @ 2.50GHz × 4)

Of Sagittarius:

```
sash -L ../src only-thread.scm

;;  (run count n*)
;;  0.187311 real    0.666758 user    0.016081 sys

;;  (run count n*)
;;  1.930832 real    6.880919 user    0.057010 sys

;;  (run count n*)
;;  19.668944 real    71.378263 user    0.535720 sys
sash -L ../src  managed-thread.scm

;;  (run count n*)
;;  0.205822 real    0.634320 user    0.004150 sys

;;  (run count n*)
;;  2.224107 real    6.178853 user    0.084120 sys

;;  (run count n*)
;;  22.646924 real    62.801109 user    0.451154 sys
sash -L ../src  thread-pool.scm

;;  (run count n*)
;;  0.321009 real    0.682420 user    8.4e-400 sys

;;  (run count n*)
;;  1.918472 real    6.752759 user    7.52e-40 sys

;;  (run count n*)
;;  18.048768 real    67.398971 user    0.023797 sys
done
```

Of Guile:

```
guile -q -L ../lib/guile -x .sls only-thread.scm
clock utime stime cutime cstime gctime
 0.25  0.79  0.04   0.00   0.00   0.10
clock utime stime cutime cstime gctime
 2.34  7.65  0.18   0.00   0.00   0.10
clock utime stime cutime cstime gctime
 32.03 76.19 12.11   0.00   0.00   0.33

guile -q -L ../lib/guile -x .sls managed-thread.scm
clock utime stime cutime cstime gctime
 0.27  0.77  0.03   0.00   0.00   0.11
clock utime stime cutime cstime gctime
 2.72  7.27  0.15   0.00   0.00   0.12
clock utime stime cutime cstime gctime
 27.15 72.56  0.90   0.00   0.00   0.55

guile -q -L ../src -L ../lib/guile -x .sls thread-pool.scm
clock utime stime cutime cstime gctime
 0.20  0.73  0.00   0.00   0.00   0.01
clock utime stime cutime cstime gctime
 2.02  7.35  0.00   0.00   0.00   0.07
clock utime stime cutime cstime gctime
 19.96 74.06  0.14   0.00   0.00   0.92
done
```

Of Gauche:

```
only-thread.scm7
;(time (run count n*))
; real   0.191
; user   0.670
; sys    0.010
;(time (run count n*))
; real   1.914
; user   6.840
; sys    0.060
;(time (run count n*))
; real  18.636
; user  68.540
; sys    0.580
managed-thread.scm7
;(time (run count n*))
; real   0.228
; user   0.650
; sys    0.000
;(time (run count n*))
; real   2.291
; user   6.220
; sys    0.050
;(time (run count n*))
; real  22.466
; user  61.200
; sys    0.530
thread-pool.scm7
;(time (run count n*))
; real   0.187
; user   0.650
; sys    0.010
;(time (run count n*))
; real   1.804
; user   6.770
; sys    0.000
;(time (run count n*))
; real  17.881
; user  67.240
; sys    0.040
done
```

Of Chibi:

```
only-thread.scm7
(run count n*): 440 ms
(run count n*): 4674 ms
(run count n*): 53061 ms
managed-thread.scm7
(run count n*): 488 ms
(run count n*): 5069 ms
(run count n*): 54634 ms
thread-pool.scm7
(run count n*): 574 ms
(run count n*): 5028 ms
(run count n*): 49338 ms
done
```


Depending on the implementations but using thread pool on heavy load of 
iteration makes better performance (max 60%). Manual thread management
seems have least user time. According to the Guile result, thread-pool
has the largest GC time.
