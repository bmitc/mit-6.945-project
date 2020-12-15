(load "concurrency")

(send-message input-address 'input 3)

(send-message double-actor 'input 3)

(send-message increment-actor 'input 3)

(send-message increment-actor 'lambda (lambda (x) (+ x 2)))

(send-message increment-actor 'input 3)
