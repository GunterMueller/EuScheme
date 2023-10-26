;;; condcl.em -*- Lisp -*-
;;; Euscheme code Copyright (c) 1994 Russell Bradford

(defmodule condcl

  (import (root telos))

  (defclass <condition> ()
    ((message reader: condition-message
;;	      default: "no message"
	      keyword: message:))
    predicate: condition?
    abstractp: t)

  (defclass <error> (<condition>)
    ((value reader: error-value
	    keyword: value:))
    abstractp: t)

  (defcondition <general-error> <error>)

;;  (defcondition <telos-condition> <condition>)
  (defclass <telos-condition> (<condition>)
    ()
    abstractp: t)

;;  (defcondition <telos-error> <telos-condition>
;;    value "no value")
  (defclass <telos-error> (<telos-condition>)
    ((value reader: telos-error-value
	    keyword: value:))
    abstractp: t)

  (defcondition <telos-general-error> <telos-error>)
  (defcondition <no-applicable-method> <telos-error>)
  (defcondition <no-next-method> <telos-error>)
  (defcondition <incompatible-method-domain> <telos-error>)
  (defcondition <telos-bad-ref> <telos-error>
    expected-type <class>)

;;  (defcondition <arithmetic-condition> <condition>)
  (defclass <arithmetic-condition> (<condition>)
    ()
    abstractp: t)
  (defcondition <arithmetic-error> <arithmetic-condition>
    value "no value")
  
  (defcondition <bad-type> <error>
    expected-type <class>)
  (defcondition <unbound-error> <error>)
;;  (defcondition <compilation-error> <error>)
  (defclass <compilation-error> (<error>)
    ()
    abstractp: t)
  (defcondition <compilation-general-error> <compilation-error>)
  (defcondition <macro-error> <compilation-error>)

  (defcondition <user-interrupt> <condition>)

  (defcondition <socket-error> <error>)
  (defcondition <syntax-error> <error>)

  (export defcondition condition? condition-message condition-value
	  <condition>
	    <telos-condition>
              <telos-error>
	        <telos-general-error>
  	        <no-applicable-method>
		<no-next-method>
		<incompatible-method-domain>
		<telos-bad-ref>
	    <arithmetic-condition>
	      <arithmetic-error>
	    <error>
  	      <general-error>
	      <bad-type>
	      <unbound-error>
	      <compilation-error>
	        <compilation-general-error>
	        <macro-error>
	      <socket-error>
	      <syntax-error>
	  <user-interrupt>)

  ; reusable conditions for run-time errors
  (define general-error (make <general-error>))
  (define telos-error (make <telos-general-error>))
  (define telos-bad-ref (make <telos-bad-ref>))
  (define no-applicable-method-error (make <no-applicable-method>))
  (define no-next-method-error (make <no-next-method>))
  (define incompatible-method (make <incompatible-method-domain>))
  (define bad-type-error (make <bad-type>))
  (define unbound-error (make <unbound-error>))
  (define arith-error (make <arithmetic-error>))
  (define user-interrupt (make <user-interrupt>))
  (define compilation-error (make <compilation-general-error>))
  (define macro-error (make <macro-error>))
  (define socket-error (make <socket-error>))
  (define syntax-error (make <syntax-error>))

)
