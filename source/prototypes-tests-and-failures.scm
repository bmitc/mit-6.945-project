#|===================================================
       Async expression macro failures
=====================================================|#

#|
This documents some of my tests and failures regarding the asynchronous expression
macros. It contains the bare minimum of helper functions so that the asyncrhonous expressions
can be defined and created in an identical way to the robust and working code.
|#


#|===================================================
       Dependencies
=====================================================|#

;;; Threads: src/runtime/thread.scm
;;; Thread queues: src/runtime/thread-queue.scm
;;; Promises: https://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Promises.html
;;; Macros: https://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Macros.html


#|===================================================
       Helper functions
=====================================================|#

;;; Sleeps the current thread for the given amount of milliseconds.
(define (sleep milliseconds)
  (sleep-current-thread milliseconds))

;;; Takes a thread queue and an item and enqueues the item to the thread queue.
;;; This naming scheme is conflicting with some other threaded-queue procedures
;;; and common usage of stacks, but it is short and convenient.
(define (push! queue item)
  (thread-queue/queue! queue item))

;;; Takes a thread queue and dequeues an item. This blocks the current thread until an
;;; an item can be dequeued. Will block indefinitely if thre is no item to dequeue.
(define (pop! queue)
  (thread-queue/dequeue! queue))

;;; Creates a thunk for the given body. Call it like (thunk body).
(define-syntax thunk
  (syntax-rules ()
    ((_ x ...)
     (lambda () x ...))))

#| Test:
(pp (syntax (thunk (* 2 2) (* 4 4)) (the-environment))) ; ->
(lambda ()
  (* 2 2)
  (* 4 4))
|#

;;; Short-hand notation for creating a new thread with a default continuation.
(define (thread thunk)
  (create-thread #f thunk))


#|===================================================
       Tests for how syntax-rules works
=====================================================|#

(define-syntax syntax-test-1
  (syntax-rules ()
    ((_ x ...)
     (+ x ...))))

(define-syntax syntax-test-2
  (syntax-rules ()
    ((_ x ...)
     (let ((result (map 1+ (list x ...))))
       (if (= 1 (length result))
	   (car result)
	   result)))))

(define-syntax syntax-test-3
  (syntax-rules ()
    ((_ x ...)
     (list x ...))))

(define-syntax syntax-test-3
  (syntax-rules ()
    ((_ x ...)
     '(x ...))))


#|===================================================
       Async/Await
=====================================================|#

;;; Defines a record structure for an async-expression.
;;; This is mainly used as a wait to hide the underlying implementation
;;; of the async-expression and for pretty printing at the REPL.
(define-record-type <async-expression>
  (make-async-expression promise-thread                     ;; Constructor
			 result-value-queue
			 symbolic-expression)
  async-expression?                                         ;; Predicate
  (promise-thread get-promise-thread)                       ;; Accessors
  (result-value-queue get-result-value-queue)
  (symbolic-expression get-original-expression))

;;; Helper function to help pretty print async-expressions.
(define (get-async-parts async-expression)
  (list 'Expression: (get-original-expression async-expression)))

;;; Pretty printer for async-expressions that shows the async-expression type
;;; and the expression the async-expression was created from.
(define-print-method async-expression?
  (standard-print-method "<async-expression>" get-async-parts))

;;; Defines a macro that takes the arguments to async and returns an asynchronous process.
;;; This macro is used so that the arguments are not evaluated. Call like (async body).
(define-syntax original-async
  (syntax-rules ()
    ((_ x)
     (let ((queue (make-thread-queue 1))) ;; Create a single-element queue for the return value.
       (make-async-expression
	(delay (thread (thunk (push! queue x)        ;; Delayed evaluations.
			      stop-current-thread))) ;; Thread can be restarted.
	queue
	(quote x))))))

#| Notes:
This macro definition of original-async only takes in a single argument and returns a single
async-expression. To build a list of async-expressions, one must do so manullay, like:
(list (async (display 1) (display 2)))
|#

;;; Works for multiple arguments to async but doesn't handle lexical binding properly.
;;; See below for an example of this.
(define-syntax new-async-1
  (syntax-rules ()
    ((_ x ...)
     (let* ((env (interaction-environment))
	    (result (map (lambda (expr) (async-symbolic expr env)) '(x ...))))
       (cond ((empty? result) (error "async requires at least one argument."))
	     ((= 1 (length result)) (car result))
	     (else result))))))

;;; The helper function that gets mapped across a symbolic list of arguments.
(define (async-symbolic quoted-expression env)
  (let ((queue (make-thread-queue 1)))
    (make-async-expression
     (delay (thread (thunk (push! queue (eval quoted-expression env))
			   stop-current-thread)))
     queue
     quoted-expression)))


(define-syntax new-async-2
  (syntax-rules ()
    ((_ x ...)
     (let* ((quoted-arguments '(x ...))
	    (symbolic-list (map (lambda (y) (list 'sub-async y))
				quoted-arguments)))
       (eval (cons 'list symbolic-list)
	     (interaction-environment))))))

(define-syntax sub-async
  (syntax-rules ()
    ((_ x)
     (let ((queue (make-thread-queue 1))) ;; Create a single-element queue for the return value.
       (make-async-expression
	(delay (thread (lambda () (push! queue x)        ;; Delayed evaluations.
			          stop-current-thread))) ;; Thread can be restarted.
	queue
	(quote x))))))

#| Tests:
(map await-async (map start-async (new-async-2 (* 2 3) (* 4 5)))) ; -> (6 20)
|#

(define-syntax sub-async-2
  (syntax-rules ()
    ((_ x)
     (let ((queue (make-thread-queue 1)) ;; Create a single-element queue for the return value.
	   (y (delay x)))
       (make-async-expression
	(delay (thread (lambda () (push! queue (force y)) ;; Delayed evaluations.
			          stop-current-thread)))  ;; Thread can be restarted.
	queue
	(quote x))))))

(define-syntax new-async-3
  (syntax-rules ()
    ((_ x ...)
     (let* ((quoted-arguments '(x ...))
	    (symbolic-list (map (lambda (y) (list 'sub-async-2 y))
				quoted-arguments)))
       (eval (cons 'list symbolic-list)
	     (interaction-environment))))))

#| Test to see what is returned by symbolic-list:
(define-syntax async-tests
  (syntax-rules ()
    ((_ x ...)
     (let* ((quoted-arguments '(x ...))
	    (symbolic-list (map (lambda (y) (list 'sub-async y))
				quoted-arguments)))
       symbolic-list))))
|#

;;; Defines a record structure for an awaitable, which is returned after starting an
;;; async-expression. This is mainly used as a wait to hide the underlying implementation
;;; of the awaitable, which is a queue, and for pretty printing at the REPL.
(define-record-type <awaitable>
  (make-awaitable thread-queue)    ;; Constructor
  awaitable?                       ;; Predicate
  (thread-queue get-thread-queue)) ;; Accessors

;;; Start the given asynchronous expression. This process must have been created using
;;; the async procedure (macro).
(define (start-async async-expression)
  (if (async-expression? async-expression)
      (let ((promise (get-promise-thread async-expression)) ;; Get the async-expression parts
	    (queue (get-result-value-queue async-expression)))
	(force promise)         ;; Force the async-expression's promise, starts the thread
	(make-awaitable queue)) ;; Return the awaitable which is used to get the resulting value
      (error "The given expression is not an async-expression.")))

;;; Waits for a started asynchronous process to finish and the returns its value.
;;; This blocks the current thread indefinitly until the process finishes.
;;; We allow the result to be requested multiple times if the process is finished.
(define (await-async awaitable)
  (cond ((async-expression? awaitable) (pop! (get-result-value-queue awaitable)))
	((awaitable? awaitable) (pop! (get-thread-queue awaitable)))
	(else (error "The expression passed in is not an async-expression or awaitable."))))


#|===================================================
       Lexical binding problems with new-async-<num>
=====================================================|#

#| All three implementations above of new-sync have this issue with lexical bindings.
We'll just use new-async-1 to demonstrate success for simple async-expressions but then
failure for parameterized async-expressions.
|#

#| Successes:
(await-async (start-async (new-async-1 (* 2 3)))) ; -> 6
(map await-async (map start-async (new-async-1 (* 2 3) (* 4 5)))) ; -> (6 20)
|#

(define (tester x) (* 2 x))
(define (async-tester this-variable) (new-async-1 (tester this-variable)))

#| Failure:
(start-async (async-tester 3))
;The thread #[thread 31] signalled an error #[condition 32 "unbound-variable"]:
Unbound variable: this-variable
|#

#| Notes: I could not find out a way to get these macros to properly expand within
the local environment such that lexical bindings were available. Even upon inspecting the
call stack with the debugger showed that only top-level bindings were available.

This debugging is done by:
error> (debug)
debug> C
|#


#|===================================================
       Original implementions of some mapped async
       macros for lists of async-expressions.
=====================================================|#

;;; Define map for asynchronous expressions.
;;; This takes a list of procedure-calls and returns a list of asynchronous expressions
;;; such that async is "applied" to each member of the list.
;;; For example, (map-async (list (display 1) (display 2))) returns the list
;;; (list #[async-expression] #[async-expression]) and does not evaluate (display 1) or
;;; (display 2) until start-async is called upon each element.
(define-syntax map-async
  (syntax-rules ()
    ((_ list-of-procedure-calls)
     (let ((symbolic-list (map (lambda (x) (list 'async x))      ;; Wrap each element with async
			       (cdr 'list-of-procedure-calls)))) ;; Drops the 'list
       (eval (cons 'list symbolic-list)                          ;; Put back the 'list
	     (interaction-environment))))))

#| Notes:
This macro is fairly simple when looking at it at a high level. What it does is simply takes
a list such as
(list (display 1) (display 2))
and then evaluates this to
(list (async (display 1))
      (async (display 2)))
Thus, it returns a list of asynchronous expressions. See the tests below for a break down
of how this macro processes the incoming list procedure calls.
|#

#| Tests:
(cdr '(list (test 1) (test 2))) ; -> ((test 1) (test 2))
--------------------------------------------------------
(map (lambda (x) (list 'async x))
		       (cdr '(list (test 1) (test 2))))
; -> ((async (test 1)) (async (test 2)))
--------------------------------------------------------
(cons 'list (map (lambda (x) (list 'async x))
		       (cdr '(list (test 1) (test 2)))))
; -> (list (async (test 1)) (async (test 2)))
--------------------------------------------------------
(eval (cons 'list (map (lambda (x) (list 'async x))
		       (cdr '(list (test 1) (test 2)))))
      (the-environment))
; -> (#[async-expression 84] #[async-expression 85])

(map start-async (map-async (list (display 1) (display 2)))) ; -> 12 (printed to the console)
|#

;;; Helper macro such that a list of procedure-calls can individually be wrapped as an
;;; asynchronous expression and then started, each procedure-call being evaluated in its own
;;; asynchronous process.
(define-syntax start-async-parallel
  (syntax-rules ()
    ((_ x)
     (map start-async (map-async x)))))

#| Usage:
(define (test n) (* 2 n))
(map await-async (start-async-parallel (list (test 1) (test 2)))) ; -> (2 4)
(map await-async (start-async-parallel '((test 1) (test 2))))

This should display 2 immediately then 3 after one second and then 1 after another second.
(start-async-parallel
 (list (begin (sleep 2000) (display 1))
       (display 2)
       (begin (sleep 1000) (display 3))))
This shows how processes can be launched asynchronously all at once.
|#
