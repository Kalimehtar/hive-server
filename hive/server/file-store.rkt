#lang racket

(require hive/common/serialize)

(provide store-read store-save!)

(define work-directory (build-path (current-directory) "data"))

(define (store-read name constructor [deref identity])
  (unless (directory-exists? work-directory)
    (make-directory work-directory))
  (define directory (build-path work-directory name))
  (unless (directory-exists? directory)
    (make-directory directory))
  (for/list ([f (in-directory directory)])
    (apply constructor (deserialize (call-with-input-file f read) deref))))

(define (store-save! data)
  (call-with-atomic-output-file (build-path work-directory
                                            (symbol->string (struct-name data))
                                            (number->string (object-id data)))
    (λ (out _) (write (serialize data) out))))