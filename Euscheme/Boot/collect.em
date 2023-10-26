;;; collect.em -*- Lisp -*-
;;; Euscheme code Copyright (c) 1994 Russell Bradford
;;;
;;; many are inefficient and could do with a rewrite
;;;
;;; the semantics of some (tables, in particular) may be awry

;; <collection-condition>
;; accumulate
;; accumulate1
;; allp
;; anyp
;; collectionp
;; concatenate
;; delete
;; do
;; element
;; (setter element)
;; emptyp
;; fill
;; map
;; member
;; remove
;; reverse
;; sequencep
;; size
;; and converters

;; lists, strings, vectors, tables

(defmodule collect

  (import (root thread telos condcl setter convert macros copy))

;;  (defcondition <collection-condition> <condition>)
  (defclass <collection-condition> (<condition>)
    ()
    abstractp: t)
  (defcondition <collection-error> <collection-condition>
    value "no value")

  (define (missing-op name val)
    (error
     (if (collectionp val)
	 (string-append
	  "missing " name " operation for collection")
	 (string-append
	  "not a collection in " name))
     <collection-error>
     value: val))

  (define-generic (collectionp x))

  (define-method (collectionp x) #f)

  (define-generic (sequencep x))

  (define-method (sequencep x) #f)

  ;;
  (define-generic (accumulate (fn <function>) init c))

  (define-method (accumulate (fn <function>) init c)
    (missing-op "accumulate" c))

  ;;
  (define-generic (accumulate1 (fn <function>) c))

  (define-method (accumulate1 (fn <function>) c)
    (missing-op "accumulate" c))

  ;;
  (define-generic (allp (fn <function>) c . more))

  (define-method (allp (fn <function>) c . more)
    (missing-op "allp" c))

  ;;
  (define-generic (anyp (fn <function>) c . more))

  (define-method (anyp (fn <function>) c . more)
    (missing-op "anyp" c))

  ;;
  (define-generic (concatenate c . more))

  (define-method (concatenate c . more)
    (missing-op "concatenate" c))

  ;;
  (define-generic (delete obj c . fn))

  (define-method (delete obj c . fn)
    (missing-op "delete" c))

  ;;
  (define-generic (do (fn <function>) c . more))

  (define-method (do (fn <function>) c . more)
    (missing-op "do" c))

  ;;
  (define-generic (element c n))

  (define-method (element c n)
    (missing-op "element" c))

  ;;
  (define-generic (setter-element c n v))

  (define-method (setter-element c n v)
    (missing-op "(setter element)" c))

  ((setter setter) element setter-element)

  ;;
  (define-generic (emptyp c))

  (define-method (emptyp c)
    (missing-op "emptyp" c))

  ;;
  (define-generic (fill c o . k))

  (define-method (fill c o . k)
    (missing-op "fill" c))

  ;;
  (define-generic (map (fn <function>) c . more))

  (define-method (map (fn <function>) c . more)
    (missing-op "map" c))

  ;;
  (define-generic (member o c . test))

  (define-method (member o c . test)
    (missing-op "member" c))

  ;;
  (define-generic (remove obj c . fn))

  (define-method (remove obj c . fn)
    (missing-op "remove" c))

  ;;
  (define-generic (reverse c))

  (define-method (reverse c)
    (missing-op "reverse" c))

  ;;
  (define-generic (size x))

  (define-method (size c)
    (missing-op "size" c))

  ;; lists
  (define-method (collectionp (l <list>)) #t)
  (define-method (sequencep (l <list>)) #t)

  (define-method (accumulate (fn <function>) init (l <list>))
    (acc-list fn l init))

  (define (acc-list fn l sofar)
    (if (atom? l)
	sofar
	(acc-list fn (cdr l) (fn sofar (car l)))))

  (define-method (accumulate1 (fn <function>) (l <list>))
    (acc1-list fn l))

  (define (acc1-list fn l)
    (if (atom? l)
	()
	(acc-list fn (cdr l) (car l))))

  (define-method (allp (fn <function>) (l <list>) . more)
    (allp-list fn l more))

  (define (allp-list fn l more)
    (cond ((null? more)
           (allp-1 fn l))
          ((null? (cdr more))
           (allp-2 fn l (convert (car more) <list>)))
          (t (allp-n fn (cons l (map-list (converter <list>) more))))))

  (define (allp-1 fn l)
    (cond ((atom? l) #t)
          ((fn (car l))
	   (allp-1 fn (cdr l)))
          (t #f)))

  (define (anyp-2 fn l1 l2)
    (cond ((or (atom? l1) (atom? l2)) #t)
          ((fn (car l1) (car l2))
	   (anyp-2 fn (cdr l1) (cdr l2)))
	  (t #f)))

  (define (anyp-n fn ls)
    (cond ((any-atoms? ls) #t)
          ((apply fn (map-list car ls))
	   (anyp-n fn (map-list cdr ls)))
	  (t #f)))

  (define-method (anyp (fn <function>) (l <list>) . more)
    (anyp-list fn l more))
    
  (define (anyp-list fn l more)
    (cond ((null? more)
	   (anyp-1 fn l))
	  ((null? (cdr more))
	   (anyp-2 fn l (convert (car more) <list>)))
	  (t (anyp-n fn (cons l (map-list (converter <list>) more))))))

  (define (anyp-1 fn l)
    (cond ((atom? l) ())
	  ((fn (car l)) #t)
	  (t (anyp-1 fn (cdr l)))))

  (define (anyp-2 fn l1 l2)
    (cond ((or (atom? l1) (atom? l2)) ())
          ((fn (car l1) (car l2)) #t)
          (t (anyp-2 fn (cdr l1) (cdr l2)))))

  (define (anyp-n fn ls)
    (cond ((any-atoms? ls) ())
	  ((apply fn (map-list car ls)) #t)
	  (t (anyp-n fn (map-list cdr ls)))))

  (define (any-atoms? l)
    (cond ((atom? l) #f)
	  ((atom? (car l)) #t)
	  (t (any-atoms? (cdr l)))))

  (define-method (concatenate (l <list>) . more)
    (if (null? more)
	l
	(concatenate-lists l more)))

  (define (concatenate-lists l more)
    (apply append l (map-list (converter <list>) more)))

  (define-method (delete obj (l <list>) . fn)
    (delete-list obj l (if (null? fn) eqv? (car fn))))

  (define (delete-list obj l comp)
    (cond ((atom? l) (if (comp obj l) () l))
	  ((comp obj (car l)) (delete-list obj (cdr l) comp))
	  (t (set-cdr! l (delete-list obj (cdr l) comp)))))

  (define-method (do (fn <function>) (l <list>) . more)
    (do-list fn l more))

  (define (do-list fn l more)
    (cond ((null? more)
	   (for-each fn l))
	  ((null? (cdr more))
	   (for-each fn l (convert (car more) <list>)))
	  (t
	   (apply for-each fn l
	    (map-list (converter <list>) more)))))

  (define-method (element (l <list>) (n <integer>))
    (list-ref l n))

  (define-method (setter-element (l <list>) (n <integer>) v)
    (set-car! (list-tail l n) v)
    v)

  (define-method (emptyp (l <list>))
    (null? l))

  (define-method (fill (l <list>) o . k)
    (cond ((null? k)
	   (fill-all-list l o))
	  ((collectionp (car k))
	   (fill-keyed-list l o (car k)))
	  ((and (integer? (car k))
		(not (atom? (cdr k)))
		(integer? (cadr k)))
	   (fill-index-list l o (car k) (cadr k)))
	  (t (error "bad keys in fill"
		    <collection-error>
		    value: k)))
    ())

  (define (fill-all-list l o)
    (cond ((atom? l) ())
	  ((atom? (cdr l))
	   (set-car! l o))
	  (t (set-car! l o)
	     (fill-all-list (cdr l) o))))

  (define (fill-keyed-list l o k)
    (for-each
     (lambda (key)
       (set-car! (list-tail l key) o))
     (convert k <list>)))

  (define (fill-index-list l o start end)
    (fill-index-list-aux (list-tail l start) o start end))

  (define (fill-index-list-aux l o index end)
    (if (<= index end)
	(begin
	 (set-car! l o)
	 (fill-index-list-aux (cdr l) o (+ index 1) end))))

  (define-method (map (fn <function>) (c <list>) . more)
    (maplist fn c more))

  (define (maplist fn c more)
    (cond ((null? more)
	   (map-list fn c))
	  ((null? (cdr more))
	   (map-list fn c (convert (car more) <list>)))
	  (t (apply map-list fn c
		    (map-list (converter <list>) more)))))

  (define-method (member o (l <list>) . test)
    (memberlist o l (if (null? test) eqv? (car test))))

  (define (memberlist o l test)
    (cond ((atom? l) ())
	  ((test o (car l)) l)
	  (t (memberlist o (cdr l) test))))

  (define-method (remove obj (l <list>) . fn)
    (remove-list obj l (if (null? fn) eqv? (car fn))))

  (define (remove-list obj l comp)
    (cond ((atom? l) (if (comp obj l) () l))
	  ((comp obj (car l)) (remove-list obj (cdr l) comp))
	  (t (cons (car l) (remove-list obj (cdr l) comp)))))

  (define-method (reverse (l <list>))
    (reverse-list l))

  (define-method (size (l <list>))
    (length l))

  ;; strings
  (define-method (collectionp (l <string>)) #t)
  (define-method (sequencep (l <string>)) #t)

  (define-method (accumulate (fn <function>) init (s <string>))
    (acc-list fn (string->list s) init))

  (define-method (accumulate1 (fn <function>) (s <string>))
    (acc1-list fn (string->list s)))

  (define-method (allp (fn <function>) (s <string>) . more)
    (allp-list fn (convert s <list>) more))

  (define-method (anyp (fn <function>) (s <string>) . more)
    (anyp-list fn (convert s <list>) more))

  (define-method (concatenate (s <string>) . more)
    (if (null? more)
	s
	(let ((result (concatenate-lists (convert s <list>) more)))
	  (if (allp-1 char? result)
	      (convert result <string>)
	      (error "not a char in result of concatenate string"
		     <collection-error>
		     value: result)))))

  (define-method (delete obj (s <string>) . fn)
    (remove-seq obj s fn <string>))

  (define (remove-seq obj seq fn class)
    (convert (delete-list obj
			  (convert seq <list>)
			  (if (null? fn) eqv? (car fn)))
	     class))

  (define-method (do (fn <function>) (s <string>) . more)
    (do-list fn (convert s <list>) more))

  (define-method (element (s <string>) (n <integer>))
    (string-ref s n))

  (define-method (setter-element (s <string>) (n <integer>) v)
    (string-set! s n v))

  (define-method (emptyp (s <string>))
    (string-null? s))

  (define-method (fill (s <string>) o . k)
    (cond ((null? k)
	   (fill-string s o 0 (string-length s)))
          ((collectionp (car k))
           (fill-keyed-string s o (car k)))
          ((and (integer? (car k))
                (not (atom? (cdr k)))
                (integer? (cadr k)))
           (fill-string s o (car k) (cadr k)))
          (t (error "bad keys in fill"
                    <collection-error>
                    value: k)))
    ())

  (define (fill-string s o index end)
    (if (< index end)
	(begin
	 (string-set! s index o)
	 (fill-all-string s o (+ index 1) end))))

  (define (fill-keyed-string s o k)
    (for-each
     (lambda (key)
       (string-set! s key o))
     (convert k <list>)))

  (define-method (map (fn <function>) (s <string>) . more)
    (let ((result (maplist fn (convert s <list>) more)))
      (if (allp-1 char? result)
	  (convert result <string>)
	  (error "not a char in result of map string"
		 <collection-error>
		 value: result))))

  (define-method (remove obj (s <string>) . fn)
    (remove-seq obj s fn <string>))

  (define-method (reverse (s <string>))
    (convert (reverse-list (convert s <list>)) <string>))

  (define-method (member o (s <string>) . test)
    (if (memberlist o (convert s <list>)
		    (if (null? test) eqv? (car test)))
	#t
	#f))

  (define-method (size (s <string>))
    (string-length s))

  ;; vectors
  (define-method (collectionp (l <vector>)) #t)
  (define-method (sequencep (l <vector>)) #t)

  (define-method (accumulate (fn <function>) init (v <vector>))
    (acc-list fn (vector->list v) init))

  (define-method (accumulate1 (fn <function>) (v <vector>))
    (acc1-list fn (vector->list v)))

  (define-method (allp (fn <function>) (v <vector>) . more)
    (allp-list fn (convert v <list>) more))

  (define-method (anyp (fn <function>) (v <vector>) . more)
    (anyp-list fn (convert v <list>) more))

  (define-method (concatenate (v <vector>) . more)
    (if (null? more)
	v
	(convert (concatenate-lists (convert v <list>) more)
		 <vector>)))

  (define-method (delete obj (v <vector>) . fn)
    (remove-seq obj v fn <vector>))

  (define-method (do (fn <function>) (v <vector>) . more)
    (do-list fn (convert v <list>) more))

  (define-method (element (v <vector>) (n <integer>))
    (vector-ref v n))

  (define-method (setter-element (v <vector>) (n <integer>) e)
    (vector-set! v n e))

  (define-method (emptyp (v <vector>))
    (= (vector-length v) 0))

  (define-method (fill (v <vector>) o . k)
    (cond ((null? k)
	   (fill-vector v o 0 (vector-length v)))
          ((collectionp (car k))
           (fill-keyed-vector v o (car k)))
          ((and (integer? (car k))
                (not (atom? (cdr k)))
                (integer? (cadr k)))
           (fill-vector v o (car k) (cadr k)))
          (t (error "bad keys in fill"
                    <collection-error>
                    value: k)))
    ())

  (define (fill-vector v o index end)
    (if (< index end)
	(begin
	 (vector-set! v index o)
	 (fill-vector v o (+ index 1) end))))

  (define (fill-keyed-vector v o k)
    (for-each
     (lambda (key)
       (vector-set! v key o))
     (convert k <list>)))

  (define-method (map (fn <function>) (v <vector>) . more)
    (convert (maplist fn (convert v <list>) more) <vector>))

  (define-method (member o (v <vector>) . test)
    (if (memberlist o (convert v <list>)
		    (if (null? test) eqv? (car test)))
	#t
	#f))

  (define-method (remove obj (v <vector>) . fn)
    (remove-seq obj v fn <vector>))

  (define-method (reverse (v <vector>))
    (convert (reverse-list (convert v <list>)) <vector>))

  (define-method (size (v <vector>))
    (vector-length v))

  ;; tables 
  (define-method (collectionp (l <table>)) #t)

  (define-method (accumulate (fn <function>) init (t <table>))
    (acc-list fn (table-values t) init))

  (define-method (accumulate1 (fn <function>) (t <table>))
    (acc1-list fn (table-values t)))

  (define-method (allp (fn <function>) (t <table>) . more)
    (allp-list fn (convert t <list>) more))

  (define-method (anyp (fn <function>) (t <table>) . more)
    (anyp-list fn (convert t <list>) more))

  (define-method (concatenate (t <table>) . more)
    (if (null? more)
	t
	(let ((new (make-table (table-comparator t) (table-fill t))))
	  (for-each
	   (lambda (old)
	     (for-each
	      (lambda (key)
		(table-set! new key (table-ref old key)))
	      (table-keys old)))
	   (cons t (map-list (converter <table>) more)))
	  new)))

  (define-method (delete obj (t <table>) . fn)
    (if (not (or (null? fn) (eq? (car fn) (table-comparator t))))
	(error "comparator incompatible with table in delete"
	       <collection-error>
	       value: (car fn))
	(begin
	 (table-delete t obj)
	 t)))

  (define-method (do (fn <function>) (t <table>) . more)
    (do-list fn (convert t <list>) more))

  (define-method (element (t <table>) key)
    (table-ref t key))

  (define-method (setter-element (t <table>) key v)
    (table-set! t key v))

  (define-method (emptyp (t <table>))
    (= (table-length t) 0))

  (define-method (fill (t <table>) o . k)
    (cond ((null? k)
	   (for-each
	    (lambda (key)
	      (table-set! t key o))
	    (table-keys t))
	   (set-table-fill! t o))
	  ((collectionp (car k))
	   (fill-keyed-table t o (car k)))
	  (t (error "no natural order for tables in fill"
		    <collection-error>
		    value: k))))

  (define-method (map (fn <function>) (t <table>) . more)
    (cond ((null? more)
	   (map-table fn t))
	  (t (error "no natural order for table in map"
		    <collection-error>
		    value: f))))

  (define (map-table fn t)
    (let ((new (make-table (table-comparator t) (fn (table-fill t)))))
      (for-each
       (lambda (k)
	 (table-set! new k (fn (table-ref t k))))
       (table-keys t))
      new))

  (define-method (remove obj (t <table>) . fn)
    (if (not (or (null? fn) (eq? (car fn) (table-comparator t))))
	(error "comparator incompatible with table in remove"
	       <collection-error>
	       value: (car fn))
	(let ((new (shallow-copy t)))
	 (table-delete new obj)
	 new)))

  (define-method (reverse (t <table>))
    t)

  (define-method (member o (t <table>) . test)
    (if (memberlist o (table-values t)
		    (if (null? test) eqv? (car test)))
	#t
	#f))

  (define-method (size (t <table>))
    (table-length t))


  (export <collection-condition> <collection-error> 
	  collectionp sequencep accumulate accumulate1 allp anyp concatenate
	  delete do element emptyp fill map member remove reverse size)

)
