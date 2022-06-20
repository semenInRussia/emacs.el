#lang racket
(provide dig-pow)

(define digits
  (compose
   (curry map (compose string->number string))
   string->list
   number->string))

(define (dig-pow n pow)
  )
