#lang racket/base

(provide serve register-command! register-std-command!)
(require racket/function
         racket/match
         racket/tcp
         racket/port
         hive/common/read-write
         "server/commands.rkt"
         "server/users.rkt"
         "server/session.rkt")

;; got authenticated
;; if new session => handle process-commands
;;    else => handle broadcast-events in current session
(define (handle in out)
  (define auth-result (auth (read/timeout in)))
  (with-handlers ([(const #t) (compose displayln exn->list)])
    (dynamic-wind
     void
     (λ () (cond 
             [(user? auth-result)
              (define user auth-result)
              (define receiver (thread (λ ()
                                         (let loop ()
                                           (with-handlers ([(const #t)
                                                            (λ (x) (custodian-shutdown-all
                                                                    (current-custodian)))])
                                             (write/flush (thread-receive) out)
                                             (loop))))))
              (thread (λ ()
                        (let loop ()
                          (when (thread-send receiver 'keepalive #f)
                            (sleep 10)
                            (loop)))))
              (define session (register-new-session! user receiver))
              (write/flush 'ok out)
              (parameterize ([current-user user]
                             [current-role (user-role user)]
                             [current-session session])
                (get-commands-loop in receiver))]
             [else (write/flush auth-result out)]))
     (λ () (custodian-shutdown-all (current-custodian))))))

(define (process-command cmd)
  (with-handlers ([(λ (x) #t) exn->list])
    (match cmd
      [(list-rest kind cmd rest)
       (case kind
         [(command) (do-command cmd rest)]
         [(std-command) (do-std-command cmd rest)]
         [else (raise-argument-error 'process-command 
                                     "command or std-command"
                                     kind)])]
      [_ (raise-argument-error 'process-command 
                                  "list (kind cmd ...)"
                                  cmd)])))

(define (get-commands-loop in receiver)
  (define cmd (read/timeout in))
  (unless (eof-object? cmd)
    (match cmd
      ['keepalive #t]
      [(list-rest id cmd)
       (thread (λ ()
                 (thread-send receiver
                              (cons id (process-command cmd)) #f)))]
      [_ (thread-send receiver 'bad-syntax #f)])
    (get-commands-loop in receiver)))

(define (exn->list exn)
  (match (struct->vector exn)
    [(vector type msg _ ...) (list 'error type msg)]))

(module+ test
  (require rackunit)
  (define (test-func a1 a2)
    (list a1 a2))
  (register-command! 'test test-func)
  (define (make-receiver out)
    (thread (λ ()
              (let loop ()
                (define data (thread-receive))
                (when data
                  (write/flush data out)
                  (loop))))))
  (check-equal? (call-with-input-string "(1 command test 5 6)"
                                        (λ (in)
                                          (call-with-output-string
                                           (λ (out)
                                             (define receiver (make-receiver out))
                                             (get-commands-loop in receiver)
                                             (sleep 0.1)
                                             (thread-send receiver #f)))))
                "(1 5 6)\n")
  (check-equal? (call-with-input-string "(2 command no-command 5 6)"
                                        (λ (in)
                                          (call-with-output-string
                                           (λ (out)
                                             (define receiver (make-receiver out))
                                             (get-commands-loop in receiver)
                                             (sleep 0.1)
                                             (thread-send receiver #f)))))
                "(2 error struct:exn:fail:contract \"do-command: contract violation\\n  expected: id of command registered with register-command!\\n  given: 'no-command\")\n"))

(define (accept-and-handle listener)
  (parameterize ([current-custodian (make-custodian)])
    (define-values (in out) (tcp-accept listener))
    (thread (λ () (handle in out)))))

(define (serve [port-no 1313])
  (define cust (make-custodian)) 
  (parameterize ([current-custodian cust]) 
    (define listener (tcp-listen port-no 5 #t))
    (define (loop) 
      (accept-and-handle listener) 
      (loop))
    (thread loop))
  (lambda ()
    (custodian-shutdown-all cust)))