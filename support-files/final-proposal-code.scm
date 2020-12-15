(define example-process ...)
(define async-example-process (async example-process)) ; wraps a process as an async process
(define example-processes (map async (list ...)))      ; wraps multiple processes
(start-async async-example-process)                    ; starts & doesn’t wait for completion
(define result (wait-async async-example-process))     ; starts and waits for async process to
                                                       ; return a value
(define result-list
  (wait-async (start-parallel example-processes))) ; starts multiple async processes in
                                                   ; parallel and waits for return values and
                                                   ; puts them in a list named result-list

; Creates an actor but does not start the actor process
(define (create-actor name initial-state message-processors) ...)

; Starts the actor process and returns an address for the actor
(define (start-actor actor) ...)

; Sends a message to an actor via address or name. Does not return a processed value.
(define (send-message message actor-address-or-name) ...)

; Batch sends multiple messages to a single actor via address or name
(define (send-messages messages actor-addresses-or-name) ...)

; Sends a synchronous message, waits for the message to be processed, and returns the result
(define (send-sync-message actor-address-or-name message) ...)

; Stops a running actor after it processes the messages received before the stop message
(define (stop-actor actor-address-or-name) ...)