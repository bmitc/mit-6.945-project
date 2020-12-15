# MIT 6.945 Project: async/await and actors implemented in MIT/GNU Scheme

## Introduction

I took [Prof. Gerald Jay Sussman's](http://groups.csail.mit.edu/mac/users/gjs/gjs.html) [MIT 6.945 Large-scale Symbolic Systems (aka Adventures in Advanced Symbolic Programming)](https://groups.csail.mit.edu/mac/users/gjs/6.945/) in spring 2019.

As part of the course, we were to propose and complete a project, implementing it in Scheme, utilizing the philosophy of the course, which was:

> Concepts and techniques for the design and implementation of large software systems that can be adapted to uses not anticipated by the designer.

My [draft proposal](/reports-and-presentations/draft-proposal.pdf) contains some interesting ideas, such as:

1. **Concurrency Substrate**: async/await and actors

2. **Dataflow Language**: implementing dataflow processes like those found in LabVIEW or VHDL

3. **Teachable Actors**: teaching actors new behavior via new message processors

4. **2D Graphics the Way I Think**: generate 2D graphics and animation declaratively

5. **Scheme Type System**: adding types to Scheme without them getting in the way

6. **Differential Forms Library**: build a library to handle differential form calculations

In the end, I chose to implement async/await and actors in MIT/GNU Scheme, as I felt it was one that could be built off of to complete some of the other projects, most of which needed concurrency.

## Code

The code for the project is found in [concurrency.scm](/source/concurrency.scm). The only dependency is [MIT/GNU Scheme](https://www.gnu.org/software/mit-scheme/). The code is well documented and organized, so take a look! I think it is a relatively nice example of adding a useful feature like async/await to a programming language _without_ editing the compiler or interpretor. In this case, async/await was built using a simple macro and thread-safe queues. Once you have async/await that executes on threads, actors are easily added to the system.

A small preview is below, which is the definition of the actor loop process that listens via blocking, which is intentional since every actor runs in its own thread, for new messages and then processes them.

```scheme
;;; This procedure defines the actual actor process. This is what is launched as an
;;; asynchronous process. The actor-loop processes messages as they arive and then recursively
;;; evaluates to wait for the next message. This procedure handles messages global to all actors,
;;; such as the stop message.
(define (actor-loop state message-processors inbox)
  (let* ((message (pop! inbox))                                   ;; Wait for a message to arrive.
	 (message-name (car message))                             ;; Get the message's name.
	 (message-value (cadr message))                           ;; Get the message's value.
         (is-synchronous? (equal? 'sync (caddr message))))        ;; Check synchronous flag.
    (cond ((stop-message? message) state)                         ;; Check for the stop message, which stops the actor.
	  (else (let ((procedure (get-message-processor-procedure message-name
								  message-processors)))
		  (if (equal? 'no-message-processor procedure)    ;; Check for a msg processor
		      (actor-loop state message-processors inbox) ;; Ignore the message
		      (actor-loop (procedure state message-value) ;; Process the message and
				  message-processors              ;;   update the state
				  inbox)))))))
```

## Presentation

The [presentation](/reports-and-presentations/project-presentation.pdf) copied below gives a good high-level overview of the work.

![slide-01](/images/slide-01.png)
![slide-02](/images/slide-02.png)
![slide-03](/images/slide-03.png)
![slide-04](/images/slide-04.png)
![slide-05](/images/slide-05.png)
![slide-06](/images/slide-06.png)
![slide-07](/images/slide-07.png)
![slide-08](/images/slide-08.png)
![slide-09](/images/slide-09.png)
![slide-10](/images/slide-10.png)
![slide-11](/images/slide-11.png)
![slide-12](/images/slide-12.png)
![slide-13](/images/slide-13.png)
![slide-14](/images/slide-14.png)
