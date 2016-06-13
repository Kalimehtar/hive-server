#lang racket/base
(provide define-catalog)

(require hive/common/serialize "file-store.rkt")

(define catalogs (make-hash))

(define (deref data)
  (define catalog (hash-ref catalogs (ref-typename data)))
  (find-by-ref (ref-id data) catalog))

(define-syntax-rule (define-catalog name next! constructor)
  (begin
    (define name (sort
                   (store-read
                      (symbol->string 'constructor) constructor deref)
                   >
                   #:key object-id))
    (define next! (make-next! (foldl max 0 (map object-id name))))
    (hash-set! catalogs 'constructor name)))

(define ((make-next! val))
  (set! val (add1 val))
  val)

