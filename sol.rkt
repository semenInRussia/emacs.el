#lang racket

(require math/number-theory)

(module+ main
  (for* ([i (in-naturals)])
    (when (prime? i)
      (let ((p (- (expt i 7) 21)))
        (when (prime? p)
          (println p)
          (newline))))))
