;;; compare.em -*- Lisp -*-
;;; Euscheme code Copyright (c) 1994 Russell Bradford

(defmodule compare

  (import (root))

  (define %equal? equal?)

  (define-generic (equal? a b))

  ;; not as in definition, as it says eql
  (define-method (equal? (a <object>) (b <object>))
    (%equal? a b))

  (define %< <)

  (define-generic (binary< a b))

  (define-method (binary< (a <number>) (b <number>))
    (%< a b))

  (define-method (binary< (a <char>) (b <char>))
    (char<? a b))

  (define-method (binary< (a <string>) (b <string>))
    (string<? a b))

  (define (< arg . args)
    (cond ((null? args) #t)
	  ((null? (cdr args)) (binary< arg (car args)))
	  ((binary< arg (car args)) (apply < args))
	  (t #f)))

  (define (> arg . args)
    (cond ((null? args) #t)
          ((null? (cdr args)) (binary< (car args) arg))
          ((binary< (car args) arg) (apply > args))
          (t #f)))

  (define %= =)

  (define-generic (binary= a b))

  (define-method (binary= (a <number>) (b <number>))
    (%= a b))

  (define-method (binary= (a <char>) (b <char>))
    (char=? a b))

  (define-method (binary= (a <string>) (b <string>))
    (string=? a b))

  (define (= arg . args)
    (cond ((null? args) #t)
          ((null? (cdr args)) (binary= arg (car args)))
          ((binary= arg (car args))
           (apply = (car args) (cdr args)))
          (t #f)))

  (define (<= arg . args)
    (cond ((null? args) #t)
          ((null? (cdr args)) (or (binary< arg (car args))
				  (binary= arg (car args))))
          ((or (binary< arg (car args))
	       (binary= arg (car args))) (apply <= args))
          (t #f)))

  (define (>= arg . args)
    (cond ((null? args) #t)
          ((null? (cdr args)) (or (binary< (car args) arg)
                                  (binary= arg (car args))))
          ((or (binary< (car args) arg)
               (binary= arg (car args))) (apply <= args))
          (t #f)))

  (define (max arg . args)
    (cond ((null? args) arg)
	  ((null? (cdr args))
	   (if (binary< arg (car args))
	       (car args)
	       arg))
	  (t (apply max (max arg (car args)) (cdr args)))))

  (define (min arg . args)
    (cond ((null? args) arg)
          ((null? (cdr args))
           (if (binary< arg (car args))
               arg
	       (car args)))
          (t (apply min (min arg (car args)) (cdr args)))))

  (export equal? binary< binary= < = > <= >= max min)

  (define %assoc assoc)

  (define (assoc obj list . comp)
    (assoc-loop obj list (if (null? comp) eqv? (car comp))))

  (define (assoc-loop obj list comp)
    (cond ((atom? list) ())
          ((comp obj (caar list)) (car list))
          (t (assoc-loop obj (cdr list) comp))))

  (export assoc)

)
