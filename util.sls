#!r6rs

; Exposing my Perl roots a bit here.

(library (sistim util)
  (export say
          slurp
          percentage)
  (import (rnrs))

  (define (say msg)
    (display msg)
    (newline))

  (define (slurp file)
    (call-with-input-file file get-string-all))

  (define (percentage a b)
    (inexact (* (/ a b) 100))))

