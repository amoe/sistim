#!r6rs

; Exposing my Perl roots a bit here.

(library (sistim util)
  (export say
          say*
          utter
          slurp
          percentage
          get-line-bytevector
          pyg
          alias
          file->value
          ->boolean
          ->string
          apply-thunk
          possibly-transform
          begin0
          amap
          plus-or-minus
          port->lines
          file->lines
          replicate
          random-real-in-range
          warn
          sub1
          add1)
  (import (rnrs)
          (only (srfi :1) unfold list-tabulate)
          (srfi :27)
          (srfi :38)
          (srfi :48))

  (define *line-feed* #x0a)
  (define *carriage-return* #x0d)

  ; randomized below

  (define (plus-or-minus n)
    (exact (floor (* (- (* (random-real*) 2) 1) n))))

  (define (random-real-in-range r)
    (* (random-real*) r))

  (define (say msg)
    (display msg)
    (newline))

  (define (say* template . rest)
    (display (apply format (cons template rest)))
    (newline))

  (define (warn msg)
    (let ((p (current-error-port)))
      (display "warning: " p)
      (display msg p)
      (newline p)))

  ; very cute.  sorry.
  (define (utter msg)
    (format #t "~y" msg))

  (define (slurp file)
    (call-with-input-file file get-string-all))

  (define (percentage a b)
    (inexact (* (/ a b) 100)))

  ; read CRLF
  (define (get-line-bytevector binary-input-port)
    (u8-list->bytevector
     (let loop ()
       (let ((byte (get-u8 binary-input-port)))
         (cond
          ((or (= byte *line-feed*)
               (eof-object? byte))
           '())
          ((= byte *carriage-return*)
           (get-u8 binary-input-port)    ; skip the next byte, assumed LF
           '())
          (else
           (cons byte (loop))))))))

  (define (pyg key alist)
    (cond
      ((assoc key alist) => cdr)
      (else
       (error 'pyg "key not found" key))))

  ; substitute symbols in a list by looking them up in an alist
  (define (alias a k)
    (cond
     ((assq k a) => cdr)
     (else k)))

  (define (file->value filename)
    (call-with-input-file filename read))

  (define (->boolean value)
    (if value #t #f))

  (define (->string value)
    (format "~a" value))

  ; slightly silly, but can help with some comprehensions
  (define (apply-thunk proc) (proc))

  ; weird
  (define (possibly-transform pred? f x) (if pred? (f x) x))

  ; stolen from Nausicaa, which was in turn stolen from the R6RS appendix A.
  (define-syntax begin0
    (syntax-rules ()
      ((_ ?expr0 ?expr ...)
       (call-with-values
           (lambda () ?expr0)
         (lambda args
           ?expr ...
           (apply values args))))))

  ; mapping alists to two-argument functions
  (define (amap f l)
    (map (lambda (pair) (f (car pair) (cdr pair))) l))

  (define (port->lines port)
    (unfold eof-object? values (lambda (seed) (get-line port)) (get-line port)))

  (define (file->lines path)
    (call-with-input-file path port->lines))

  ; very slow
  (define (replicate l n)
    (let ((len (length l)))
      (list-tabulate (* len n)
                     (lambda (i) (list-ref l (mod i len))))))

  (define (sub1 n) (- n 1))
  (define (add1 n) (- n 1))

  (define random-source (make-random-source))
  (define random-real* (random-source-make-reals random-source))
  (random-source-randomize! random-source)
)
