(load "concurrency")

(slow-fib 31)

(define a (async (slow-fib 31)))
a

(define b (start-async a))
b

(await-async b)
