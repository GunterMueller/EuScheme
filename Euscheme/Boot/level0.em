;;; level0.em -*- Lisp -*-
;;; Euscheme code Copyright (c) 1994 Russell Bradford

(defmodule level0

  (import
   ((rename
     ((begin progn)
      (thread? threadp)
      (lock? lockp)
      (generic-display generic-prin)
      (display prin)
      (next-method? next-method-p)
      (null? null)
      (atom? atom)
      (list? listp)
      (number? numberp)
      (boolean? booleanp)
      (pair? consp)
      (symbol? symbolp)
      (keyword? keywordp)
      (complex? complexp)
      (real? floatp)
      (real? double-float-p)
      (rational? rationalp)
      (integer? integerp)
      (char? charp)
      (string? stringp)
      (vector? vectorp)
      (procedure? functionp)
      (port? portp)
      (input-port? input-port-p)
      (output-port? output-port-p)
      (object? objectp)
      (eof-object? eof-object-p)
      (default-object? default-object-p)
      (eq? eq)
      (eqv? eql)
      (equal? equal)
      (zero? zerop)
      (positive? positivep)
      (negative? negativep)
      (odd? oddp)
      (even? evenp)
      (backtrace? backtracep)
      (condition? conditionp)
      (class? classp)
      (subclass? subclassp)
      (char-ready? char-ready-p)
      (class-abstract? class-abstract-p))
      
     (except (defmodule)
      root
      thread
      telos
      telosint
      setter
      convert
      condcl
      arith
      compare
      macros
      collect
      copy
      format))))

  (export

   ;; specials
   quote
   lambda
   delay
   let
   let*
;;   letrec
;;   define
   setq
   if
   cond
   progn				; begin
;;   sequence
   and
   or
   while
;;   access
;;   defmodule
   export
   expose
   enter-module
   !>
   reenter-module
   !>>
;;   import
   call-next-method
   next-method-p			; next-method?
   defclass

   ;; root
   ;; cf xsftab.c for the following

   ;; functions that call eval or apply
   apply
;;   call-with-current-continuation
;;   call/cc
   map-list
;;   for-each
;;   call-with-input-file
;;   call-with-output-file
   load
   load-noisily
   force
;;   initialize-object

   ;; list functions
   cons
   car
   cdr
   caar
   cadr
   cdar
   cddr
   caaar
   caadr
   cadar
   caddr
   cdaar
   cdadr
   cddar
   cdddr
   caaaar
   caaadr
   caadar
   caaddr
   cadaar
   cadadr
   caddar
   cadddr
   cdaaar
   cdaadr
   cdadar
   cdaddr
   cddaar
   cddadr
   cdddar
   cddddr
   list
   list*
   append
;;   reverse-list
   last-pair
   length
;;   member
   memv
   memq
;;   assoc
   assv
   assq
   list-ref
   list-tail

   ;; destructive list functions
;;   set-car!
;;   set-cdr!


   ;; symbol functions
   bound?
   symbol-value
;;   set-symbol-value!
   symbol-plist
;;   set-symbol-plist!
   gensym
   get
   put

   ;; environment functions
;;   the-environment
;;   procedure-environment
;;   environment?
;;   environment-bindings
;;   environment-parent

   ;; vector functions
   vector
   make-vector
   vector-length
   vector-ref
;;   vector-set!

   ;; array functions
   make-array
   array-ref
;;   array-set!

   ;; conversion functions
;;   symbol->string
;;   string->symbol
;;   vector->list
;;   list->vector
;;   string->list
;;   list->string
;;   char->integer
;;   integer->char

   ;; predicates
   null
   atom
   listp
   numberp
   booleanp
   consp				; pair?
   symbolp
   keywordp
   complexp
   floatp				; realp
   double-float-p			; realp
   rationalp
   integerp
   charp
   stringp
   vectorp
   functionp
   portp
   input-port-p
   output-port-p
   objectp
   eof-object-p
   default-object-p
   eq
   eql
;;   equal

   ;; arithmetic functions
   zerop				; zero?
   positivep				; positive?
   negativep				; negative?
   oddp					; odd?
   evenp				; even?
   exact?
   inexact?
   truncate
   floor
   ceiling
   round
;;   add1
;;   sub1
;;   abs
;;   gcd
   random
;;   +
;;   -
;;   *
;;   /
   quotient
   remainder
;;   min
;;   max
   sin
   cos
   tan
   asin
   acos
   atan
   exp
   sqrt
;;   expt
   log

   ;; bitwise logical functions
   logand
   logior
   logxor
   lognot

   ;; numeric comparison functions
;;   <
;;   <=
;;   =
;;   >=
;;   >

   ;; string functions
   make-string
   string-length
   string-null?
   string-append
   string-ref
;;   string-set!
   substring
;;   string<?
;;   string<=?
;;   string=?
;;   string>=?
;;   string>?
;;   string-ci<?
;;   string-ci<=?
;;   string-ci=?
;;   string-ci>=?
;;   string-ci>?

   ;; character functions
;;   char<?
;;   char<=?
;;   char=?
;;   char>=?
;;   char>?
;;   char-ci<?
;;   char-ci<=?
;;   char-ci=?
;;   char-ci>=?
;;   char-ci>?

   ;; i/o functions
   read
   read-char
   read-byte
   read-short
   read-long
   write
   write-char
   write-byte
   write-short
   write-long
   prin					; display
   print
   newline
   char-ready-p				; char-ready?
   peek-char

   ;; print control functions
   print-breadth
   print-depth

   ;; file i/o functions
   open-input-file
   open-output-file
   open-append-file
   open-update-file
   close-port
   close-input-port
   close-output-port
   get-file-position
;;   set-file-position!
   unlink
   current-input-port
   current-output-port

   ; utility functions
   transcript-on
   transcript-off
   getarg
   prompt?
   exit
   compile
   decompile
   gc
   save
   restore
;;   reset
;;   xserror
;;   default-handler

   ;; debugging functions
   trace-on
   trace-off

   ;; module functions
   module-symbols
   module-exports
   symbol-module
   current-module
   module-list
   unintern

   ;; telos
   allocate
   describe
   classp
   subclassp

   ;; tables
   make-table
   table-ref
;;   table-set!
   table-comparator
   table-delete
   table-length
   table-keys
   table-values
   table-fill
;;   set-table-fill!
   table-clear

   ;; plus some others
   binary
   text
   not
   prin1
   princ
   t
   nil
   eval					; no guarantees this one will work
   else
   system
   getenv
   putenv
   tmpfile
   current-time
   ticks-per-second
   backtrace
   backtracep				; backtrace?

   ;; thread
   <thread>
   <simple-thread>
   make-thread
   threadp				; thread?
   thread-reschedule
   current-thread
   thread-kill
   thread-queue
   current-thread
   thread-start
   thread-value
   thread-state
   <thread-condition>
   <thread-error>
   <thread-already-started>

   <lock>
   <simple-lock>
   make-lock
   lockp				; lock?
   lock
   unlock
   <lock-condition>
   <lock-error>

   wait
   <wait-condition>
   <wait-error>

   let/cc
   with-handler
   unwind-protect
   <wrong-condition-class>
   signal
   error
   cerror

   ;; telos
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
;;   <env>
;;   <code>
;;   <module>
   <table>
   <hash-table>
   <function>
   <simple-function>
;;   <closure>
   <subr>
   <continuation>
   <generic>
   <simple-generic>
;;   <xsubr>
;;   <csubr>
   <method>
   <simple-method>
   <slot>
   <local-slot>
   <structure>

   generic-prin				; generic-display
   generic-write
   wait

   ;; defclass
   ;; call-next-method
   ;; call-next-method
   ;; next-method?

   make
;;   allocate
   initialize
;;   describe
   class-hierarchy

   ;; setter
   setter

   ;; converter
   converter
   convert
   <conversion-condition>
   <no-converter> 

   ;; condcl
   defcondition
   conditionp				; condition?
   condition-message
   condition-value
   <condition>
   <telos-condition>
   <telos-error>
   <telos-general-error>
   <telos-bad-ref>
   <no-applicable-method>
   <no-next-method>
   <incompatible-method-domain>
   <arithmetic-condition>
   <arithmetic-error>
   <error>
   <general-error>
   <bad-type>
   <unbound-error>
   <compilation-error>
   <macro-error>
   <syntax-error>
   <user-interrupt>

   ;; arith
   binary+
   binary-
   unary-
   binary*
   binary/
   unary/
   binary%
   binary-gcd
   gcd
   abs
   +
   -
   *
   /
   %
   pow

   ;; compare
   equal				; equal?
   binary<
   binary=
   <
   =
   >
   <=
   >=
   max
   min
   assoc

   ;; macros
   defmacro
   quasiquote
   unquote
   unquote-splicing
   symbol-macro
   macroexpand
   macroexpand1
;   syntax
;   dprint

   ;; collect
   <collection-condition>
   <collection-error>
   collectionp
   sequencep
   accumulate
   accumulate1
   allp
   anyp
   concatenate
   delete
   do
   element
   emptyp
   fill
   map
   member
   remove
   reverse   
   size

   ;; copy
   deep-copy
   shallow-copy

   ;; format
   format

  )

  ;; from telosint, export them all while developing
  (export
   class-of
   class-name
   class-superclasses
   class-precedence-list
   class-slots
   class-keywords
   class-subclasses
   class-instance-size
   class-abstract-p			; class-abstract?
;   class?
;   subclass?
   generic-name
   generic-args
   generic-optargs?
   generic-methods
   generic-cache1
   generic-cache2
   method-generic
   method-function
   method-domain
   add-method
   slot-name
   slot-keyword
   slot-default
   slot-required-p
  )

  (defmacro block (tag . body)
    (if (symbolp tag)
	`(let/cc ,tag ,@body)
	(error "not a symbol in block"
	       <compilation-general-error>
	       value: tag)))

  (defmacro return-from (tag . val)
    (if (symbolp tag)
	(if (null val)
	    `(,tag ())
	    `(,tag ,@val))
	(error "not a symbol in return-from"
	       <compilation-general-error>
               value: tag)))

  (define (letrec-binding binding)
    (list
     (car binding)
     (cons 'lambda (cdr binding))))

  (defmacro labels (bindings . body)
    `(letrec
      ,(map-list letrec-binding bindings)
      ,@body))

  (defmacro when (test . body)
    `(if ,test (progn ,@body) ()))

  (defmacro unless (test . body)
    `(if ,test () (progn ,@body)))

  (defmacro while (test . body)
    `(let/cc {break}			; break can be captured in body
       (letrec
	((loop (lambda ()
		 (when ,test
		   ,@body
		   (loop)))))
	(loop))))

  (define (definable-name? name)
    (and (consp name)
	 (or (eq (car name) 'setter)
	     (eq (car name) 'converter))))

  ; (defun foo (x) ...)
  ; (defun (setter foo) (x) ...)
  ; (defun (converter foo) (x) ...)
  (defmacro defun (name args . body)
    (cond ((symbolp name)
	   (if (bound? name)
	       (progn
		 (prin "*** redefining ")
		 (prin name)
		 (prin " in module ")
		 (print (current-module))))
	   `(define ,(cons name args)
	      ,@body))
	  ((definable-name? name)
	   `(progn
	     ((setter ,(car name)) ,(cadr name)
	      (lambda ,args ,@body))
	     ',name))
	  (t (error "malformed name in defun" 
		    <compilation-general-error>
		    value: name))))

  ; (defgeneric foo (x)
  ;    method: ((x <int>) ...)
  ;    method: ((y <flt>) ...)
  ;    ...)
  (defmacro defgeneric (name args . body)
    (cond ((symbolp name)
	   `(progn (define-generic ,(cons name args))
		   ,@(defgeneric-methods name body)
		   ',name))
	  ((definable-name? name)
	   `(progn
	     (define-generic ,(cons 'setter/converter args))
	     ((setter ,(car name)) ,(cadr name) setter/converter)
	     ,@(defgeneric-methods
		 (list 'setter (cadr name))
		 body)
	     ',name))
	  (t (error "malformed name in defgeneric"
		    <compilation-general-error>
		    value: name))))

  (define (defgeneric-methods name body)
    (cond ((null body) ())
	  ((not (eq (car body) method:))
	   (error "unknown keyword in defgeneric"
		  <compilation-general-error>
                  value: (car body)))
	  ((null (cdr body))
	   (error "odd-length keyword list in defgeneric"
		  <compilation-general-error>
		  value: name))	  
	  (t (cons
	      `(defmethod ,name ,(caadr body) ,@(cdadr body))		 
	      (defgeneric-methods name (cdr (cdr body)))))))

  (defmacro defmethod (name args . body)
    (if (or (symbolp name)
	    (definable-name? name))
	`(define-method ,(cons name args) ,@body)
	(error "malformed name in defgeneric"
	       <compilation-general-error>
	       value: name)))

  (defmacro generic-lambda (args . body)
    `(let (anonymous-generic)
       (define-generic (anonymous-generic ,@args))
       ,@(defgeneric-methods
	   'anonymous-generic
	   body)
       anonymous-generic))

  (defmacro method-lambda (args . body)
    `(lambda (next-methods arg-list ,@args)
       ,@body))

  (export block return-from labels when unless while defun defgeneric
	  defmethod generic-lambda method-lambda)

  (defmacro deflocal (var val)
    `(define ,var ,val))

  (defconstant t #t)
  (defconstant nil ())

  (export deflocal defconstant)

  (defmacro import (mod)
    (if (not (or (stringp mod)
		 (symbolp mod)))
	(error "bad module name in import"
	       <compilation-general-error>
	       value: mod)
	`(progn
	   (setq curmod (find-module (current-module)))
	   (%IMPORT curmod ,mod))))

  (export import)

  (defmacro defmodule (name . body)
    (error "only use defmodule in root module"
	   <compilation-general-error>
	   value: name))

  (export defmodule)

)
