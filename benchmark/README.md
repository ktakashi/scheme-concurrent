This directory contains benchmarks of fork-join, manual thread management
and thread-pool. Each script executes configured numbers of tasks (`fib`).
By default it's 100, 1000 and 10000. See `counts` file.

One of the iteration result:

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

Of Guile

```
guile -q -L ../lib/guile -x .sls only-thread.scm
clock utime stime cutime cstime gctime
 0.27  0.84  0.02   0.00   0.00   0.10
clock utime stime cutime cstime gctime
 2.38  7.72  0.18   0.00   0.00   0.14
clock utime stime cutime cstime gctime
 25.88 77.36  3.28   0.00   0.00   0.36
guile -q -L ../lib/guile -x .sls managed-thread.scm
clock utime stime cutime cstime gctime
 0.28  0.74  0.01   0.00   0.00   0.09
clock utime stime cutime cstime gctime
 2.58  6.59  0.11   0.00   0.00   0.31
clock utime stime cutime cstime gctime
 25.13 66.25  0.96   0.00   0.00   0.46
guile -q -L ../src -L ../lib/guile -x .sls thread-pool.scm
clock utime stime cutime cstime gctime
 0.22  0.81  0.00   0.00   0.00   0.03
clock utime stime cutime cstime gctime
 2.09  8.02  0.06   0.00   0.00   0.26
clock utime stime cutime cstime gctime
 21.75 83.25  0.60   0.00   0.00   4.59
done
```

Depending on the implementations but using thread pool on heavy load of 
iteration makes better performance (max 20%). Manual thread management
seems have least user time. According to the Guile result, thread-pool
has the largest GC time.
