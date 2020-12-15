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

;;; Waits the given time and then evaluates the thunk.
(define (wait-then-compute time-to-wait-ms thunk)
  (begin (sleep time-to-wait-ms)
	 (thunk)))

;;; Waits the given time and then displays the given expression.
(define (wait-then-display time-to-wait-ms thing-to-display)
  (wait-then-compute time-to-wait-ms
		     (thunk (display thing-to-display))))

;;; Helper definition for the empty list. This is for clarity when returning it.
(define empty-list ())

;;; An identity procedure.
(define identity (lambda (x) x))

;;; Tests for the empty list.
(define empty? null?)

;;; Slow Fibonacci procedure
(define (slow-fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (slow-fib (- n 1))
		 (slow-fib (- n 2))))))


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
(define-syntax async
  (syntax-rules ()
    ((_ x)
     (let ((queue (make-thread-queue 1))) ;; Create a single-element queue for the return value.
       (make-async-expression
	(delay (thread (thunk (push! queue x)        ;; Delayed evaluations.
			      stop-current-thread))) ;; Thread can be restarted.
	queue
	(quote x))))))

#| Notes:
Let's say async is called with (async procedure-call). We first create a queue that will serve
as the bucket to put the result of the asynchronous process in. Eventually, when the asynchronous
expression is started is started, we will evaluate the procedure and then place its value into
this queue. But with (async procedure), at the time of its creation we need to delay evaluation
of the procedure call, which is x in the macro, the pushing of the returned value onto the
queue, and the creation of the thread. Since thunk doesn't evaulate it's arguments, are good
there. But then to delay the thread creation, we wrap thread with delay, which creates a
promise.
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

#| Notes:
As explained above, when the asynchronous process is created, it is created using delayed
evaluations. Now, when the process is requested to be started, we much force the promise inside
the asynchronous process. This creates a thread, which is passed a thunk that will evaluate
the procedure wrapped by the asynchronous expression, and then put its value on the internal
single-element queue.
|#

;;; Waits for a started asynchronous process to finish and the returns its value.
;;; This blocks the current thread indefinitly until the process finishes.
;;; We allow the result to be requested multiple times if the process is finished.
(define (await-async awaitable)
  (cond ((async-expression? awaitable) (pop! (get-result-value-queue awaitable)))
	((awaitable? awaitable) (pop! (get-thread-queue awaitable)))
	(else (error "The expression passed in is not an async-expression or awaitable."))))

;;; Start an asynchronous expression, wait for the asynchronous process to finish, and then
;;; return the result. This blocks the current thread indefinitely until the process finishes.
;;; This should be equivalent to running (procedure-call) if the asynchronous expression was
;;; created with (async procedure-call).
(define (start-async-synchronously async-expression)
  (await-async (start-async async-expression)))


#|===================================================
       Lambda Actors
=====================================================|#

;;; Returns a simple lambda actor as a list that contains an async-expression as the first
;;; element and a thread-queue as the second element. Start the async-expression to start the
;;; actor and then push an input value to the thread-queue to send a value message to the
;;; simple lambda actor.
(define (simple-lambda-actor procedure)
  (let ((inbox (make-thread-queue)))     ;; Make a thread-queue as an inbox 
    (define (lambda-loop)
      (let ((input (pop! inbox)))        ;; Block the thread until a message arrives
	(apply procedure (list input))   ;; Apply the procedure to the message value
	(lambda-loop)))                  ;; Return back to waiting for a new message
    (list (async (lambda-loop)) inbox))) ;; Return (list async-expression thread-queue)

#| Example:
(define test-sla (simple-lambda-actor (lambda (input) (display (* 2 input)))))
(start-async (car test-sla))
(push! (cadr test-sla) 3.5)
|#

;;; Sends the input as a message to the lambda-actor.
(define (send-input lambda-actor input)
  (push! (cadr lambda-actor) input)) ;; Push the input onto the lambda-actor's thread-queue

;;; Starts the lambda-actor. Once started, it will be listening to messages.
(define (start-lambda-actor lambda-actor)
  (start-async (car lambda-actor))) ;; Start the lambda-actor's async-expression

;;; Takes a procedure and output-actors and returns a lambda-actor as a list that contains an
;;; async-expression as the first element and a thread-queue as the second element. Use the
;;; procedures above to start it and send it messages. The output-actors is a list of other
;;; lambda-actors that the lambda-actor sends its output to. This is useful for linking up
;;; lambda-actors.
(define (lambda-actor procedure output-actors)
  (let ((inbox (make-thread-queue))
	(outboxes
	 (if (list? output-actors)
	     (map (lambda (actor) (cadr actor)) output-actors)
	     #f)))
    (define (lambda-loop)
      (let* ((input (pop! inbox))
	     (value (apply procedure (list input))))
	(if outboxes
	    (map (lambda (outbox) (push! outbox value)) outboxes))
	    ;(send-input output-actor value))
	(lambda-loop)))
    (list (async (lambda-loop)) inbox)))

#| Example:
(define lambda-a (lambda-actor (lambda (x) (display (* x x))) #f))
(define lambda-b (lambda-actor (lambda (x) (display (* 2 x))) #f))
(define add-1 (lambda-actor (lambda (x) (+ x 1)) (list lambda-a lambda-b)))
(map (lambda (actor) (start-lambda-actor actor)) (list lambda-a lambda-b add-1))
(send-input add-1 3)
|#


#|===================================================
       General Actors
=====================================================|#

;;; Actor record, primarily for initiating actor creation and pretty printing.
(define-record-type <actor>
  (make-actor initial-state message-processors message-queue)
  actor?
  (initial-state      actor:get-initial-state)
  (message-processors actor:get-message-processors)
  (message-queue      actor:get-message-queue)
  (address            actor:get-address actor:set-address!))

;;; Actor address record, almost exclusively for pretty printing.
(define-record-type <actor-address>
  (make-actor-address queue)
  actor-address?
  (queue actor-address:get-queue))

;;; Checks if the given list is a list of actor addresses or not.
(define (actor-addresses? lst)
  (and (list? lst)
       (boolean/and (map actor-address? lst))))

;;; message-procedure = (lambda (state message-value) ...) -> state

;;; Given the message name and message procedure, which is a procedure that takes in the state
;;; and a message value and returns a new state, returns a message processor that an actor
;;; can be given for message processing.
(define (create-message-processor name message-procedure)
  (list name message-procedure 'aync)) ;; The general message type is asynchronous.

;;; Unimplemented in the actor loop. Would need to augment the message-procedure type to be
;;; a procedure that takes in the state and a message value and returns a new state and also
;;; the output of the message intended to be used in a synchronous manner. For example,
;;; request the state of an actor might be fitting for a synchronous message.
(define (create-synchronous-message-processor name message-procedure)
  (list name message-procedure 'sync))

;;; Given the incoming message's name and the actor's list of message processors, return the
;;; appropriate message processor, if there is one. Otherwise, return the symbol
;;; 'no-message-processor.
(define (get-message-processor-procedure message-name message-processors)
  (let ((message-processor (assq message-name message-processors)))
    (if message-processor
	(cadr message-processor)
	'no-message-processor)))

;;; Defines a message global to all actors that stops the actor process. This message will stop
;;; the actor, will return the actor's state as the result of the asynchronous process, and thus
;;; ends the asynchronous process that ran the actor.
(define (stop-message? message)
  (equal? 'stop-message (car message)))

;;; Defines a message global to all actors that provides a way for another process to send an
;;; actor a message and wait for a response.
(define (synchronous-message? message)
  (and (equal? 'synchronous-message (car message))
       #t))

#| Note:
There is no thread-queue predicate that I can find, and there is no apparent way to tell if a
given value is a thread-queue. It would be great to have something like this:
(thread-queue? (cadr message))
The #t is in place of this if in the future this predicate could be defined.
|#

;;; This procedure defines the actual actor process. This is what is launched as an
;;; asynchronous process. The actor-loop processes messages as they arive and then recursively
;;; evaluates to wait for the next message. This procedure handles messages global to all actors,
;;; such as the stop message.
(define (actor-loop state message-processors inbox)
  (let* ((message (pop! inbox))                            ;; Wait for a message to arrive.
	 (message-name (car message))                      ;; Get the message's name.
	 (message-value (cadr message))                    ;; Get the message's value.
         (is-synchronous? (equal? 'sync (caddr message)))) ;; Check synchronous flag.
    (cond ((stop-message? message) state) ;; Check for the stop message, which stops the actor.
	  (else (let ((procedure (get-message-processor-procedure message-name
								  message-processors)))
		  (if (equal? 'no-message-processor procedure)    ;; Check for a msg processor
		      (actor-loop state message-processors inbox) ;; Ignore the message
		      (actor-loop (procedure state message-value) ;; Process the message and
				  message-processors              ;;   update the state
				  inbox)))))))

;;; Returns an actor given the initial-state and message-processors. The returned actor will
;;; need to be started to be able to start processing messages sent to it. It will only process
;;; messages defined by the message-processors. It ignores all other messages.
(define (create-actor initial-state message-processors)
  (make-actor initial-state
	      message-processors
	      (make-thread-queue)))

;;; Launches an actor and returns the actor's address that is used to send messages to the actor.
(define (launch-actor actor)
  (let* ((async-expression (async (actor-loop (actor:get-initial-state      actor)
					      (actor:get-message-processors actor)
					      (actor:get-message-queue      actor))))
	 (awaitable (start-async async-expression)))
    (make-actor-address (actor:get-message-queue actor)))) ;; Return the actor's address.

;;; Checks whether the actor-address is an actor or an actor-address. If so, returns the
;;; actor-address value. Otherwsie, returns an error.
(define (check-for-address actor-address)
  (cond ((actor? actor-address) (make-actor-address (actor:get-message-queue actor-address)))
	((actor-address? actor-address) actor-address)
	(else (error "The given actor-address is not an actor nor an actor-address type."))))

;;; Sends a message to an actor at the given actor-address. The message name and value dictates
;;; how the message is processed. This method will also accept an actor itself for the
;;; actor-address argument.
(define (send-message actor-address message-name message-value)
  (let ((address (check-for-address actor-address)))
    (push! (actor-address:get-queue address)
	   (list message-name message-value 'async))))

;;; Sends the stop message to the given actor address or actor. This message will be
;;; processed in turn.
(define (send-stop-message actor-address)
  (send-message actor-address 'stop-message empty-list))

;;; Sends the stop message to the given actor address or actor such that the actor processes
;;; this message before any other message, even if the messages are already in its message
;;; queue.
(define (send-stop-message-now actor-address)
  (let ((address (check-for-address actor-address)))
    (thread-queue/push! (actor-address:get-queue address)  ;; Puts the message at the head of
			(list 'stop-message empty-list)))) ;; the actor's message queue.

;;; Sends a message to an actor at the given actor-address and then waits for the processed
;;; message's result.
;;; NOT IMPLEMENTED
(define (send-synchronous-message actor-address message)
  empty)


#|===================================================
       Example Actors
=====================================================|#

;;; Displays two times the message value sent.
(define test-message
  (create-message-processor 'double
			    (lambda (state message-value)
			      (begin (display (* 2 message-value))
				     empty-list))))
(define test-actor (create-actor '() (list test-message)))
(define test-address (launch-actor test-actor))

;;; Displays the state of the actor, which is just a count, and then increments the state by 1.
(define increment-counter
  (create-message-processor 'increment
			    (lambda (state message-value)
			      (begin (display state)
				     (+ state 1)))))
(define counter (create-actor 0 (list increment-counter)))
(define counter-address (launch-actor counter))

#| Tests for actor-addresses?, which is defined further above, but these test actors provide
convenient test actors:
(actor-addresses? test-address) ; -> #f
(actor-addresses? (list test-address counter-address)) ; -> #t
|#


#|===================================================
       General Lambda Actors
=====================================================|#

;;; This will be the state of a lambda-actor implemented using the general actor framework.
(define-record-type <lambda-actor-state>
  (make-lambda-actor procedure output-actors)
  lambda-actor?
  (procedure     lambda-actor:get-procedure)
  (output-actors lambda-actor:get-output-actors))

;;; Defines 'input message. Sends a value for the lambda actor's procedure to operate on.
(define input-processor
  (create-message-processor
   'input
   (lambda (state message-value)
     (let* ((procedure (lambda-actor:get-procedure state))
	    (output-actors (lambda-actor:get-output-actors state))
	    (output-value (procedure message-value)))
       (if (actor-addresses? output-actors)
	   (map (lambda (output-actor)
		  (send-message output-actor 'input output-value))
		output-actors))
       state))))

;;; Defines 'lambda message. Sends a procedure to update the lambda actor's procedure.
(define lambda-processor
  (create-message-processor
   'lambda
   (lambda (state message-value)
     (if (procedure? message-value)
	 (make-lambda-actor message-value
			    (lambda-actor:get-output-actors state))
	 state))))

;;; Creates a lambda actor to compute the given procedure and then send it to the given
;;; output-actors.
(define (create-lambda-actor procedure output-actors)
  (create-actor (make-lambda-actor procedure
				   output-actors)
		(list input-processor lambda-processor)))

;;; Lambda actor to display its 'input message.
(define display-actor
  (create-lambda-actor display empty-list))

;;; Lambda actor that increments its 'input message by one and sends it to the display-actor.
(define increment-actor
  (create-lambda-actor 1+ (list display-actor)))

;;; Lambda actor that doubles its 'input message and sends it to the display-actor.
(define double-actor
  (create-lambda-actor (lambda (x) (* 2 x))
		       (list display-actor)))

;;; Lambda actor that routes its input to the increment-actor and double-actor.
(define input-actor
  (create-lambda-actor identity
		       (list increment-actor double-actor)))

;;; Launches the lambda actors and returns their addresses in a list.
(define lambda-addresses
  (map launch-actor (list input-actor
			  double-actor
			  increment-actor
			  display-actor)))

;;; Gets the address of the input-actor so that we can start the computation.
(define input-address (car lambda-addresses))
