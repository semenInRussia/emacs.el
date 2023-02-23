#lang racket

;; here `a` has price 8 per mark
;; and `b` has price 15 per mark

(define (can-decompress? n)
  (for*/or ([a (in-range 0 n)]
            [b (in-range 0 n)]
            #:when (= (+ (* a 8) (* b 15)) n))
    #t))

(module+ main
  (for ([n (in-range 20 100)]
        #:unless (can-decompress? n))
    (printf "~s~n" n)))
