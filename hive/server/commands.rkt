#lang racket/base

(provide register-command!
         register-std-command!
         do-command
         do-std-command
         list-command)

(require hive/common/serialize racket/function racket/list srfi/26)

(module+ test (require rackunit))

(define commands (make-hash))
(define std-commands (make-hash))

(module+ test
  (define (test-func a1 a2)
    (list a1 a2))
  (register-command! 'test test-func)
  (check-eq? (hash-ref commands 'test) test-func))

(define (register-command! id func)  
  (hash-set! commands id func))

(module+ test  
  (register-std-command! 'test test-func)
  (check-eq? (hash-ref std-commands 'test) test-func))

(define (register-std-command! id func)
  (hash-set! std-commands id func))

(module+ test
  (check-equal? (do-command 'test '(2 3)) '(2 3)))

(define (do-command id args)
  (log-debug "~a: ~a ~a" 'do-command id args)
  (apply (hash-ref commands
                   id 
                   (λ () (raise-argument-error 'do-command "id of command registered with register-command!" id)))
         args))

(module+ test
  (check-equal? (do-std-command 'test '(2 3)) '(2 3)))

(define (do-std-command id args)
  (log-debug "~a: ~a ~a" 'do-std-command id args)
  (apply (hash-ref std-commands
                   id 
                   (λ () (raise-argument-error 'do-std-command "id of command registered with register-std-command!" id)))
         args))

;;; standard commands

(module+ test
  (register-command! 'test (list-command (λ () '((1 2) (3 4)))
                                         (λ (x) (list (car x) (cadr x)))))
  (check-equal? (do-command 'test '(0 4)) '((1 2) (3 4)))
  (check-equal? (do-command 'test '(0 1)) '((1 2)))
  (check-equal? (do-command 'test '(1 1)) '((3 4))))

(define (take* list0 n0)
  (if (zero? n0)
      list0
      (let loop ([list list0] [n n0])
        (cond [(zero? n) '()]
              [(pair? list) (cons (car list) (loop (cdr list) (sub1 n)))]
              [else '()]))))

(define (drop* list0 n0)
  (let loop ([list list0] [n n0])
    (cond [(zero? n) list]
          [(pair? list) (loop (cdr list) (sub1 n))]
          [else list])))

(define ((list-command items-proc [prepare (λ (x) x)]) skip limit)
  (map (cut serialize <> prepare)
       (take* (drop* (items-proc) skip) limit)))

(module+ test)