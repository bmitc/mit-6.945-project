;;; Dummy procedure.
(define (do-something-else) (display "Did something else.\n"))

;;; Computes the nth Fibonacci number, slowly
(define (slow-fibonacci n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (slow-fibonacci (- n 1))
		 (slow-fibonacci (- n 2))))))

;;; Wraps the regular procedure slow-fibonacci as an asynchronous process
(define (async-slow-fibonacci n)
  (async (slow-fibonacci n)))

;;; This mimics the F# example.
(await-async (start-async (async-slow-fibonacci 20))) ; -> 6765

;;; This mimics the LabVIEW example.
(define started-process (start-async (async-slow-fibonacci 32))) ; -> Returns immediately
(do-something-else) ; -> Prints "Did something else." immediately after the above executes.
(display (await-async started-process)) ; -> Prints "2178309" once the computation is done.