#lang racket/base
(provide make-next!)

(define ((make-next! val))
  (set! val (add1 val))
  val)