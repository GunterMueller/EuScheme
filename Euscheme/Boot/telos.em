;;; telos.em -*- Lisp -*-
;;; Euscheme code Copyright (c) 1994 Russell Bradford

(defmodule telos

  (import (root telosint))

  (define (make cl . inits)
;    (print (list 'make cl inits))
    (let ((maker (table-ref builtin-make-table cl)))
      (if (null? maker)
	  (initialize (allocate cl inits) inits)
	  (maker inits))))

  (define-generic (initialize cl inits))

  (define-method (initialize (obj <object>) inits)
    (initialize-object obj inits))

  (define-method (initialize (cl <class>) inits)
    (call-next-method)
    (initialize-class cl inits))

  ;; class-hierarchy
  (define (class-hierarchy . top)
    (hierarchy (if (null? top) <object> (car top)) 0)
    #t)

  (define (hierarchy cl depth)
    (if (class-abstract? cl)
	(%display "A  ")
        (%display "   "))
    (indent depth)
    (%display cl)
    (newline)
    (for-each
     (lambda (c)
       (hierarchy c (+ depth 2)))
     (reverse-list (class-subclasses cl))))

  (define (indent n)
    (while (> n 0)
      (princ " ")
      (setq n (- n 1))))

  ; generic printing
  (define-generic (generic-display obj port))

  (define-method (generic-display (obj <object>) port)
    (%display obj port)
    obj)

  (define-method (generic-display (obj <null>) port)
    (%display "()" port)
    obj)

  (define-method (generic-display (obj <list>) port)
    (write-list obj port generic-display))

  (define-method (generic-display (obj <vector>) port)
    (write-vector obj port generic-display))

  (define (display obj . port)
    (if (null? port)
	(generic-display obj (current-output-port))
	(generic-display obj (car port))))

  (define %print print)

  (define (print obj . port)
    (let ((p (if (null? port)
		 (current-output-port)
		 (car port))))
      (generic-display obj p)
      (newline p)
      obj))

  (define %write write)

  (define-generic (generic-write obj port))

  (define-method (generic-write (obj <object>) port)
    (%write obj port)
    obj)

  (define-method (generic-write (obj <null>) port)
    (%display "()" port)
    obj)

  (define-method (generic-write (obj <list>) port)
    (write-list obj port generic-write))

  (define-method (generic-write (obj <vector>) port)
    (write-vector obj port generic-write))
	   
  (define (write obj . port)
    (if (null? port)
        (generic-write obj (current-output-port))
        (generic-write obj (car port))))

  ;; a feeble attempt at stopping infinite loops
  (define current-print-depth 0)
  (define (inc-pr-depth n)
    (setq current-print-depth (+ current-print-depth n)))

  ;; maintain tail recursion in write-list1
  (define (write-list obj port gfun)
    (cond ((and (print-depth)
		(>= current-print-depth (print-depth)))
	   (%display "(...)" port))
	  ((list? obj)
	   (%display "(" port)
	   (inc-pr-depth 1)
	   (gfun (car obj) port)
	   (write-list1 (cdr obj) port gfun 1)
	   (inc-pr-depth -1))
	  (t (%write obj port)))	; new subclass of <list>
    obj)
	  

  (define (write-list1 obj port gfun current-print-breadth)
    (cond ((null? obj)
	   (%display ")" port))
	  ((atom? obj)
	   (%display " . " port)
	   (gfun obj port)
	   (%display ")" port))
	  ((and (print-breadth)
		(>= current-print-breadth (print-breadth)))
	   (%display " ...)" port))
	  (else
	   (%display " " port)
	   (gfun (car obj) port)
	   (write-list1 (cdr obj) port gfun (+ current-print-breadth 1)))))

  (define (write-vector obj port gfun)
    (cond ((and (print-depth)
		(>= current-print-depth (print-depth)))
	   (%display "#(...)" port))
	  ((vector? obj)
	   (let ((length (vector-length obj)))
	     (if (= length 0)
		 (%display "#()" port)
	       (begin
		(%display "#(" port)
		(inc-pr-depth 1)
		(gfun (vector-ref obj 0) port)
		(write-vector1 obj port 1 length gfun)
		(inc-pr-depth -1)))))
	  (t (%write obj port)))	; new subclass of <vector>
    obj)

  (define (write-vector1 obj port index length gfun)
    (if (= index length)
	(%display ")" port)
	(begin
	 (%display " " port)
	 (gfun (vector-ref obj index) port)
	 (write-vector1 obj port (+ index 1) length gfun))))

  (define-generic (wait thread timeout))

  (export

   ; classes
   <object>
     <class>
       <simple-class>
     <list>
       <cons>
       <null>
     <number>
       <integer>
         <fpi>
       <float>
         <double-float>
     <symbol>
       <keyword>
     <string>
       <simple-string>
     <port>
       <input-port>
       <output-port>
       <i/o-port>
     <vector>
       <simple-vector>
     <char>
       <simple-char>
     <promise>
     <env>
     <code>
     <module>
     <table>
       <hash-table>
     <function>
       <simple-function>
         <closure>
	 <subr>
       <continuation>
       <generic>
         <simple-generic>
     <xsubr>
     <csubr>
     <method>
       <simple-method>
     <slot>
       <local-slot>
     <structure>

   generic-display
   generic-write
   wait

   ; specials
   defclass
   generic-lambda
   call-next-method
   next-method?

   ; functions
   make
   allocate
   initialize

   ; debugging
   describe
   class-hierarchy

   current-print-depth
  )

  ;; from telosint, while developing
  (export

   class-of
   class-name
   class-superclasses
   class-precedence-list
   class-slots
   class-keywords
;   set-class-keywords!
   class-subclasses
   class-instance-size
   class-abstract?
   class?
   subclass?
   generic-name
   generic-args
   generic-methods
   generic-cache1
   generic-cache2
   make-generic
   make-method
   method-generic
   method-function
   method-domain
   add-method
   slot-name
   slot-keyword
   slot-default
;   set-slot-default!
   slot-required-p
;   set-slot-required-p!
;   find-slot-index
;   initialize-object
;   initialize-class

  )

)
