#lang racket
(provide (struct-out user) users find-by-name add-user! current-user current-role auth set-online!)

(require hive/common/users
         hive/common/serialize
         "commands.rkt"
         "file-store.rkt"         
         "session.rkt"
         "catalog.rkt"
         srfi/2)

(define current-user (make-parameter #f))
(define current-role (make-parameter #f))

(define on-null-users 
  (make-parameter
   (λ (username password)
     (define new-user (user (next!) username password 'admin #t))
     (add-user! new-user)
     (on-change new-user)
     new-user)))

(define on-no-such-user 
  (make-parameter
   (λ (username password)
     (define new-user (user (next!) username password 'user #t))
     (add-user! new-user)
     (on-change new-user)
     new-user)))

(define-catalog users next! user)
(for ([u (in-list users)])
  (set-user-online! u #f))

(define (add-user! new-user) 
  (set! users (cons new-user users))
  (store-save! new-user)
  (on-change new-user))

(define (find-by-name a-name)
  (for/first ([u (in-list users)]
              #:when (string=? a-name (user-name u)))
    u))

(define (without-password x)
  (struct-copy user x [password ""]))

(register-std-command! '(users list)
                       (list-command (λ () users)
                                     without-password))

(register-std-command! '(users save!)
                       (λ (id name password role)                        
                         (or (for/first ([user users]
                                         #:when (= (object-id user) id))
                               (when (or (eq? (current-role) 'admin)
                                         (and (eq? (current-user) user)
                                              (eq? role 'user)))
                                 (set-user-name! user name)
                                 (set-user-password! user password)
                                 (set-user-role! user role)
                                 (on-change user)
                                 (store-save! user)
                                 'ok))
                             'failed)))

(define users-monitor null)

(define (on-change user)
  (define info `(#f user ,@(serialize user without-password)))
  (define (send-change receiver)
    (thread-send receiver info #f))
  (map send-change users-monitor))

(define (set-online! user value)
  (set-user-online! user value)
  (on-change user))

(register-std-command! '(users monitor)
                       (λ ()
                         (define receiver (session-receiver (current-session)))
                         (unless (memq receiver users-monitor)
                           (set! users-monitor
                                 (cons receiver users-monitor)))
                         'ok))

(define main-custodian (current-custodian))

(define (auth user-pass)
  (match user-pass
    [(list username password) 
     (define found-user (find-by-name username))
     (cond
       [found-user
        (cond
          [(string=? password (user-password found-user))
           (set-online! found-user #t)
           (define user-box (make-custodian-box (current-custodian) user))
           ;; when current thread dies, main-custodian still live
           (parameterize ([current-custodian main-custodian])
             (thread (λ ()
                       (sync user-box)
                       (set-online! found-user #f))))
           found-user]
          [else 'bad-password])]
       [(null? users)
        ((on-null-users) username password)]
       [else
        ((on-no-such-user) username password)])]
    ;[(list username password session)
    ; (and-let* ([found-user (find-by-name username)]
    ;            (string=? password (user-password found-user))
    ;            [found-session (find-session session)]
    ;            (eq? found-user (session-user found-session)))
    ;   session)]
    [else 'bad-auth-syntax]))