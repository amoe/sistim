#!r6rs

; A generalized condition formatter to use in exception handlers.

(library (sistim condition-formatter)
  (export format-condition)
  (import (rnrs)
          (only (srfi :13) string-join)
          (srfi :48))

  ; All conditions have a type which is implemented by this mapping table.
  ; Conditions can optionally have the following information:
  ;   * Message
  ;   * Irritants
  ;   * Who
  ;   * Syntax-violation-form
  ;   * Syntax-violation-subform

  (define (identity x) x)

  (define (condition->subform con)
    (if (syntax-violation? con)
        (let ((subform (syntax-violation-subform con)))
          (if subform
              (format "~s" subform)
              #f))
        #f))

  (define (condition->form con)
    (if (syntax-violation? con)
        (format "~s" (syntax-violation-form con))
        #f))

  (define (condition->irritants con)
    (if (irritants-condition? con)
        (string-join
         (map (lambda (i) (format "~s" i))
              (condition-irritants con))
         ", ")
        #f))

  (define (condition->who con)
    (if (who-condition? con)
        (format "~s" (condition-who con))
        #f))

  (define (condition->type con)
    (cond
     ((error? con)    "error")
     ((assertion-violation? con)  "assertion violation")
     ((implementation-restriction-violation? con)
      "implementation restriction violation")
     ((lexical-violation? con)
      "lexical syntax violation")
     ((syntax-violation? con) "syntax violation")
     ((undefined-violation? con) "unbound identifier")
     ((violation? con)  "violation")
     ((warning? con)  "warning")
     ((serious-condition? con)  "serious")
     ((condition? con)  "condition")
     (else
      (assertion-violation 'condition->string "impossible"))))

  (define (condition->message con)
    (if (message-condition? con) (condition-message con) #f))

  (define (format-condition con)
    (when (not (condition? con))
          (assertion-violation 'condition->string
                               "invalid argument type, must be condition" con))
    (let ((msg (condition->message con))
          (type (condition->type con))
          (who (condition->who con))
          (irritants (condition->irritants con))
          (form (condition->form con))
          (subform (condition->subform con)))
      (string-join (filter identity
                           (list type who msg irritants form subform))
                   ": "))))

;(let ((con (guard (ex (#t ex)) (file-size-in-bytes "/wha"))))
;  (display (condition->string con))
;  (newline))
