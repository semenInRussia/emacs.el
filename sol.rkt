#lang racket/base

(require
 net/http-easy
 html-parsing
 threading
 sxml)

(define url "https://www.myip.com")

(define response (get url #:stream? #t))

(define extract-info-div-elements
  (sxpath '(// (div (@ class (equal? "info_2"))))))

(~>>
 response
 response-output
 html->xexp
 extract-info-div-elements
 ((select-kids string?))
 car)

