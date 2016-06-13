#lang racket/base
(provide register-new-session!         
         find-session
         current-session
         session-user
         session-receiver
         session-id)

(require srfi/2 racket/function "make-next.rkt")

(struct session (id user receiver))

(define current-session (make-parameter #f))

(define (find-session session-number)
  (let loop ([sessions sessions])
    (cond
      [(null? sessions) #f]
      [(and-let* ([session (custodian-box-value (car sessions))]
                  (= (session-id session) session-number))
         session) => identity]
      [else (loop (cdr sessions))])))

(define sessions null)

(define (register-new-session! user receiver)
  (define next! (make-next! 0))
  (define new-session (session (next!) user receiver))
  (set! sessions (cons (make-custodian-box (current-custodian) new-session) sessions))
  new-session)

