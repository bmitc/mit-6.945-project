(:type Number => Number)
(define (square x) (* x x))

(:type List of Integers => Number)
(define (sum-list lst) (reduce + 0 lst))

(:type (Anything => Boolean) -> List of Anything => List of Anything)
(define (filter predicate lst) ...)

(:type List of String|Number => String)
(define (create-email-address list-of-strings-and-numbers) ...)

(:type Number -> Number => Number
  (define (multiply x y) (* x y)))

(define actor1 (actor 'Test
                      )

(define centerX 50)
(define centerY 50)
(define radius 50)
(define speed (hz (/ 1 10)))
(define disk1 (disk radius 
                    'fill "white"
                    'boundary "black"))
(define disk2 (disk (/ radius 2)
                    'fill "black"
                    'boundary "black"))

(setup
  (size 100 100)
  (background (grayscale 204))
  (place-by-center disk1 centerX centerY))

(draw
  (move-along-path disk2 (boundary disk1) speed))

(define (dataflow-process x)
  (define incoming-terminal (new-terminal x))
  (define outgoing-terminal-a (new-terminal))
  (define outgoing-terminal-b (new-terminal))
  (branch incoming-terminal => a b)
  (dataflow ((process-a a) => outgoing-terminal-a)
            ((process-b b) => outgoing-terminal-b)))

(define example-process ...)
(define example-processes (list ...))
(define async-example-process (async example-process))
(start-async async-example-process) ;; starts and does not wait for completion
(define result (wait-async async-example-process)) ;; starts and waits for async process to return a value
(define result-list
  (wait-async (start-parallel example-processes))) ;; starts multiple async processes in parallel and waits for
                                                   ;; the return values and puts them in a list named result-list

(define omega (+ (* (literal-function f) (1-form x)) (* (literal-function g) (1-form y))))
; -> omega = (+ (* (literal-function f) 'dx) (* (literal-function f) 'dy)
(define domega (d omega))
; -> (* (- (partial (literal-function g) x) (partial (literal-function f) y))
;       (wedge 'dx 'dy))