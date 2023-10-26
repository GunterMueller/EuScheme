/* xsinit.c - xscheme initialization routines */
/*	Copyright (c) 1988, by David Michael Betz
	All Rights Reserved */
/* Euscheme code Copyright (c) 1994 Russell Bradford */

#include "xscheme.h"
#include "xsbcode.h"

/* macro to store a byte into a bytecode vector */
#define pb(x)	(*bcode++ = (x))

/* global variables */
LVAL true,eof_object,default_object,s_unassigned;
LVAL cs_map1,cs_foreach1,cs_withfile1,cs_load1,cs_force1,cs_initloop1;
LVAL c_lpar,c_rpar,c_dot,c_quote,s_quote;
LVAL s_eval,s_unbound,s_stdin,s_stdout,s_stderr,s_filein;
LVAL s_fixfmt,s_flofmt;
LVAL s_direct_slots, s_direct_keywords, s_name, s_default, s_requiredp;
LVAL s_keyword;
LVAL s_abstractp, s_predicate, s_constructor, s_keywords, s_superclasses;
LVAL s_reader, s_writer, s_accessor, s_class, s_defclass, s_find_slot_index;
LVAL s_getivar, s_setivar, s_list, s_lambda, s_defun, s_object, s_value;
LVAL s_backtracep, s_eq, s_eqv, s_equal, s_equals;
LVAL s_import, s_only, s_except, s_rename, s_rename_flag, s_callcc;
LVAL s_make, s_apply, s_setter, s_signal, s_unwind_protect;
LVAL s_general_error, s_no_applic_error, s_no_next_md_error;
LVAL s_bad_type_error, s_telos_error, s_telos_bad_ref, s_incompatible_md;
LVAL s_unbound_error, s_arith_error, s_user_intr, s_syntax_error;
LVAL s_compile_error;
LVAL s_letname, s_begin, s_compile, s_setmodule, s_getmodule, s_reintern;
LVAL s_module_directives;
LVAL s_binary_plus, s_binary_minus, s_binary_times, s_binary_divide;
LVAL s_quotient, s_binary_less, s_binary_equal, s_current_thread;
LVAL s_thread_class, s_qualified_symbols, s_structure, s_set_generic_args;
LVAL s_macro_error, s_supplied_env, s_debug, s_xlframe, s_gcmsgs;
LVAL s_arg_list, s_next_methods;
#ifndef NO_CHECK_REF
LVAL s_check_ref;
#endif
#ifdef SOCK
LVAL s_socket_error;
#endif

/* external variables */
extern JMP_BUF top_level;
extern FUNDEF funtab[];
extern int xsubrcnt;
extern int csubrcnt;
extern LVAL xlenter_keyword();

extern void init_root_module();
extern void init_root_exports();
extern void set_ticks();

#ifdef __STDC__
void xlsymbols(void);
#else
void xlsymbols();
#endif

/* xlinitws - create an initial workspace */
void xlinitws(ssize)
  unsigned int ssize;
{
    unsigned char *bcode;
    int type,i;
    LVAL code;
    FUNDEF *p;
    extern LVAL s_gcmsgs;

    /* allocate memory for the workspace */
    xlminit(ssize);

    s_gcmsgs = NIL;

#ifndef OLDSYM
    obarray = newvector(HSIZE);
#endif
    init_root_module();

    s_unbound = NIL; /* to make cvsymbol work */

    /* enter the eof object */
    eof_object = cons(xlenter("**EOF**"),NIL);
    setvalue(car(eof_object), eof_object);

    /* enter the default object */
    default_object = cons(xlenter("**DEFAULT**"),NIL);

    /* install the built-in functions */
    for (i = 0, p = funtab; p->fd_subr != NULL; ++i, ++p) {
	type = (i < xsubrcnt ? XSUBR : (i < csubrcnt ? CSUBR : SUBR));
	xlsubr(p->fd_name,type,p->fd_subr,i);
    }

    /* setup some synonyms */
    setvalue(xlenter("not"),getvalue(xlenter("null?")));
    setvalue(xlenter("prin1"),getvalue(xlenter("write")));
    setvalue(xlenter("princ"),getvalue(xlenter("display")));

    /* enter all of the symbols used by the runtime system */
    xlsymbols();

    /* set the initial values of the symbols #T, T and NIL, etc. */
    setvalue(true,true);
    setvalue(xlenter("t"),true);
    setvalue(xlenter("nil"),NIL);
    setvalue(xlenter("backtrace?"), true);
    setvalue(xlenter("qualified-symbols?"), NIL);
    setvalue(xlenter("supplied-env"), NIL);
    setvalue(xlenter("pathname_prefix"), NIL);

    xloinit(); /* initialize xsobj.c */

    /* setup the print formats for numbers */
    setvalue(s_fixfmt,cvstring(IFMT));
    setvalue(s_flofmt,cvstring(FFMT));
    
    /* build the 'eval' function */
    code = newcode(4); cpush(code);
    setelement(code,0,newstring(0x12));
    setelement(code,1,xlenter("eval"));
    setelement(code,2,cons(xlenter("X"),NIL));
    setelement(code,3,xlenter("compile"));
    drop(1);

    /* store the byte codes */
    bcode = (unsigned char *)getstring(getbcode(code));

pb(OP_FRAME);pb(0x02);		/* 0000 12 02    FRAME 02		*/
pb(OP_MVARG);pb(0x01);		/* 0002 13 01    MVARG 01		*/
pb(OP_ALAST);			/* 0004 1a       ALAST			*/
pb(OP_SAVE);pb(0x00);pb(0x10);	/* 0005 0b 00 10 SAVE 0010		*/
pb(OP_EREF);pb(0x00);pb(0x01);	/* 0008 09 00 01 EREF 00 01 ; x		*/
pb(OP_PUSH);			/* 000b 10       PUSH			*/
pb(OP_GREF);pb(0x03);		/* 000c 05 03    GREF 03 ; compile	*/
pb(OP_CALL);pb(0x01);		/* 000e 0c 01    CALL 01		*/
pb(OP_CALL);pb(0x00);		/* 0010 0c 00    CALL 00		*/

    setvalue(getelement(code,1),cvclosure(code,NIL));

    /* setup the initialization code */
    code = newcode(6); cpush(code);
    setelement(code,0,newstring(0x11));
    setelement(code,1,xlenter("*INITIALIZE*"));
    setelement(code,3,cvstring("xscheme.ini"));
    setelement(code,4,xlenter("load"));
    setelement(code,5,xlenter("*TOPLEVEL*"));
    drop(1);

    /* store the byte codes */
    bcode = (unsigned char *)getstring(getbcode(code));

pb(OP_FRAME);pb(0x01);		/* 0000 12 01    FRAME 01		*/
pb(OP_ALAST);			/* 0002 1a       ALAST			*/
pb(OP_SAVE); pb(0x00); pb(0x0d);/* 0003 0b 00 0d SAVE 000d		*/
pb(OP_LIT);  pb(0x03);		/* 0006 04 03    LIT 03 ; "xscheme.ini"	*/
pb(OP_PUSH);			/* 0008 10       PUSH			*/
pb(OP_GREF); pb(0x04);		/* 0009 05 04    GREF 04 ; load		*/
pb(OP_CALL); pb(0x01);		/* 000b 0c 01    CALL 01		*/
pb(OP_GREF); pb(0x05);		/* 000d 05 05    GREF 05 ; *toplevel*	*/
pb(OP_CALL); pb(0x00);		/* 000f 0c 00    CALL 00		*/

    setvalue(getelement(code,1),cvclosure(code,NIL));

/*
(define (*toplev*)
  (if prompt?
      (begin 
        (newline)
        (display (current-module))
        (display "> ")))
  (setq *last* (read *FILE-INPUT*))
  (if (eq? *last* **eof**) (exit))
  (if prompt?
      (write (eval *last*))
      (eval *last*))
  (*toplev*))
*/

    /* setup the main loop code */
    code = newcode(15); cpush(code);
    setelement(code,0,newstring(0x70));
    setelement(code,1,xlenter("*TOPLEVEL*"));
    setelement(code,3,xlenter("prompt?"));
    setelement(code,4,xlenter("newline"));
    setelement(code,5,xlenter("%display"));
    setelement(code,6,cvstring("> "));
    setelement(code,7,xlenter("*FILE-INPUT*"));
    setelement(code,8,xlenter("read"));
    setelement(code,9,xlenter("*last*"));
    setelement(code,10,xlenter("**EOF**"));
    setelement(code,11,xlenter("exit"));
    setelement(code,12,xlenter("eval"));
    setelement(code,13,xlenter("write"));
    setelement(code,14,xlenter("*TOPLEVEL*"));
    drop(1);

    /* store the byte codes */
    bcode = (unsigned char *)getstring(getbcode(code));

/*

0000 12 01    FRAME 01
0002 1a       ALAST
0003 05 03    GREF 03 ; prompt?@root
0005 02 00 22 BRF 0022
0008 0b 00 0f SAVE 000f
000b 05 04    GREF 04 ; newline@root
000d 0c 00    CALL 00
000f 0b 00 18 SAVE 0018
0012 50       current-module
0013 10       PUSH
0014 05 05    GREF 05 ; display@root
0016 0c 01    CALL 01
0018 0b 00 22 SAVE 0022
001b 04 06    LIT 06 ; "> "
001d 10       PUSH
001e 05 05    GREF 05 ; display@root
0020 0c 01    CALL 01
0022 0b 00 2c SAVE 002c
0025 05 07    GREF 07 ; *FILE-INPUT*@root
0027 10       PUSH
0028 05 08    GREF 08 ; read@root
002a 0c 01    CALL 01
002c 06 09    GSET 09 ; *last*@root
002e 05 0a    GREF 0a ; **eof**@root
0030 10       PUSH
0031 05 09    GREF 09 ; *last*@root
0033 1f       eq?
0034 02 00 3e BRF 003e
0037 0b 00 3e SAVE 003e
003a 05 0b    GREF 0b ; exit@root
003c 0c 00    CALL 00
003e 05 03    GREF 03 ; prompt?@root
0040 02 00 58 BRF 0058
0043 0b 00 55 SAVE 0055
0046 0b 00 50 SAVE 0050
0049 05 09    GREF 09 ; *last*@root
004b 10       PUSH
004c 05 0c    GREF 0c ; eval@root
004e 0c 01    CALL 01
0050 10       PUSH
0051 05 0d    GREF 0d ; write@root
0053 0c 01    CALL 01
0055 03 00 62 BR 0062
0058 0b 00 62 SAVE 0062
005b 05 09    GREF 09 ; *last*@root
005d 10       PUSH
005e 05 0c    GREF 0c ; eval@root
0060 0c 01    CALL 01
0062 05 0e    GREF 0e ; *toplev*@root
0064 0c 00    CALL 00

*/

  pb(OP_FRAME); pb(0x01);
  pb(OP_ALAST);
  pb(OP_GREF); pb(0x03);
  pb(OP_BRF); pb(0x00); pb(0x22);
  pb(OP_SAVE); pb(0x00); pb(0x0f);
  pb(OP_GREF); pb(0x04);
  pb(OP_CALL); pb(0x00);
  pb(OP_SAVE); pb(0x00); pb(0x18);
  pb(OP_CURMOD);
  pb(OP_PUSH);
  pb(OP_GREF); pb(0x05);
  pb(OP_CALL); pb(0x01);
  pb(OP_SAVE); pb(0x00); pb(0x22);
  pb(OP_LIT); pb(0x06);
  pb(OP_PUSH);
  pb(OP_GREF); pb(0x05);
  pb(OP_CALL); pb(0x01);
  pb(OP_SAVE); pb(0x00); pb(0x2c);
  pb(OP_GREF); pb(0x07);
  pb(OP_PUSH);
  pb(OP_GREF); pb(0x08);
  pb(OP_CALL); pb(0x01);
  pb(OP_GSET); pb(0x09);
  pb(OP_GREF); pb(0x0a);
  pb(OP_PUSH);
  pb(OP_GREF); pb(0x09);
  pb(OP_EQ);
  pb(OP_BRF); pb(0x00); pb(0x3e);
  pb(OP_SAVE); pb(0x00); pb(0x3e);
  pb(OP_GREF); pb(0x0b);
  pb(OP_CALL); pb(0x00);
  pb(OP_GREF); pb(0x03);
  pb(OP_BRF); pb(0x00); pb(0x58);
  pb(OP_SAVE); pb(0x00); pb(0x55);
  pb(OP_SAVE); pb(0x00); pb(0x50);
  pb(OP_GREF); pb(0x09);
  pb(OP_PUSH);
  pb(OP_GREF); pb(0x0c);
  pb(OP_CALL); pb(0x01);
  pb(OP_PUSH);
  pb(OP_GREF); pb(0x0d);
  pb(OP_CALL); pb(0x01);
  pb(OP_BR); pb(0x00); pb(0x62);
  pb(OP_SAVE); pb(0x00); pb(0x62);
  pb(OP_GREF); pb(0x09);
  pb(OP_PUSH);
  pb(OP_GREF); pb(0x0c);
  pb(OP_CALL); pb(0x01);
  pb(OP_GREF); pb(0x0e);
  pb(OP_CALL); pb(0x00);

    setvalue(getelement(code,1),cvclosure(code,NIL));
    init_root_exports();
}

/* xlsymbols - lookup/enter all symbols used by the runtime system */
void xlsymbols()
{
    LVAL sym, env;
    extern int quiet;
    
    /* top-level procedure symbol */
    s_eval = xlenter("eval");
    
    /* enter the symbols used by the system */
    true         = xlenter("#t");
    s_unbound	 = xlenter("*UNBOUND*");
    s_unassigned = xlenter("#!UNASSIGNED");

    /* enter the i/o symbols */
    s_stdin  = xlenter("*STANDARD-INPUT*");
    s_stdout = xlenter("*STANDARD-OUTPUT*");
    s_stderr = xlenter("*ERROR-OUTPUT*");
    s_filein = xlenter("*FILE-INPUT*");
    
    /* enter the symbols used by the printer */
    s_fixfmt = xlenter("*FIXNUM-FORMAT*");
    s_flofmt = xlenter("*FLONUM-FORMAT*");

    /* enter symbols needed by the reader */
    c_lpar   = xlenter("(");
    c_rpar   = xlenter(")");
    c_dot    = xlenter(".");
    c_quote  = xlenter("'");
    s_quote  = xlenter("quote");

    /* 'else' is a useful synonym for #t in cond clauses */
    sym = xlenter("else");
    setvalue(sym,true);

    /* do we want a prompt? */
    sym = xlenter("prompt?");
    if (quiet) {
      setvalue(sym, NIL);
    }
    else {
      setvalue(sym, true);
    }

    /* GC messages */
    sym = xlenter("*gc-msgs*");
    setvalue(sym, NIL);

    /* restore OS environment */
    s_supplied_env = xlenter("supplied-env");
    env = getvalue(s_supplied_env);
    for (; consp(env); env = cdr(env))
      putenv(getstring(cdr(car(env))));

    /* time info */
    set_ticks();

    /* setup stdin/stdout/stderr */
    setvalue(s_stdin,cvport(stdin,PF_INPUT));
    setvalue(s_stdout,cvport(stdout,PF_OUTPUT));
    setvalue(s_stderr,cvport(stderr,PF_OUTPUT));
    setvalue(s_filein,cvport(filein,PF_INPUT));

    /* get the built-in continuation subrs */
    cs_map1 = getvalue(xlenter("%MAP1"));
    cs_foreach1 = getvalue(xlenter("%FOR-EACH1"));
    cs_withfile1 = getvalue(xlenter("%WITH-FILE1"));
    cs_load1 = getvalue(xlenter("%LOAD1"));
    cs_force1 = getvalue(xlenter("%FORCE1"));
    cs_initloop1 = getvalue(xlenter("%INITLOOP1"));

    s_direct_slots = xlenter_keyword("direct-slots:");
    s_direct_keywords = xlenter_keyword("direct-keywords:");
    s_name = xlenter_keyword("name:");
    s_default = xlenter_keyword("default:");
    s_requiredp = xlenter_keyword("requiredp:");
    s_keyword = xlenter_keyword("keyword:");
    s_abstractp = xlenter_keyword("abstractp:");
    s_predicate = xlenter_keyword("predicate:");
    s_constructor = xlenter_keyword("constructor:");
    s_keywords = xlenter_keyword("keywords:");
    s_superclasses = xlenter_keyword("superclasses:");
    s_reader = xlenter_keyword("reader:");
    s_writer = xlenter_keyword("writer:");
    s_accessor = xlenter_keyword("accessor:");
    s_class = xlenter_keyword("class:");
    s_defclass = xlenter("defclass");

    s_find_slot_index = xlenter("find-slot-index");
    s_getivar = xlenter("%GETIVAR");
    s_setivar = xlenter("%SETIVAR");
    s_list = xlenter("list");
    s_lambda = xlenter("lambda");
    s_defun = xlenter("defun");
    s_object = xlenter("object");
    s_value = xlenter("value");

    s_backtracep = xlenter("backtrace?");

    s_eq = xlenter("eq?");
    s_eqv = xlenter("eqv?");
    s_equal = xlenter("equal?");
    s_equals = xlenter("=");

    s_import = xlenter("import");
    s_only = xlenter("only");
    s_except = xlenter("except");
    s_rename = xlenter("rename");
    s_rename_flag = xlenter("%rename");

    s_callcc = xlenter("call/cc");

    s_make = xlenter("make");
    s_apply = xlenter("apply");
    s_setter = xlenter("setter");

    s_signal = xlenter("signal");
    s_unwind_protect = xlenter("unwind-protect");
    s_letname = xlenter("let-binding");
    s_general_error = xlenter("general-error");
    s_no_applic_error = xlenter("no-applicable-method-error");
    s_no_next_md_error = xlenter("no-next-method-error");
    s_bad_type_error = xlenter("bad-type-error");
    s_telos_error = xlenter("telos-error");
    s_telos_bad_ref = xlenter("telos-bad-ref");
    s_incompatible_md = xlenter("incompatible-method");
    s_unbound_error = xlenter("unbound-error");
    s_arith_error = xlenter("arith-error");
    s_user_intr = xlenter("user-interrupt");
    s_syntax_error = xlenter("syntax-error");
    s_compile_error = xlenter("compilation-error");

    s_begin = xlenter("begin");
    s_compile = xlenter("compile");
    s_setmodule = xlenter("set-module");
    s_getmodule = xlenter("current-module");
    s_reintern = xlenter("reintern-module-symbols");
    s_module_directives = xlenter("module-directives");

    s_binary_plus = xlenter("binary+");
    s_binary_minus = xlenter("binary-");
    s_binary_times = xlenter("binary*");
    s_binary_divide = xlenter("binary/");
    s_quotient = xlenter("quotient");
    s_binary_less = xlenter("binary<");
    s_binary_equal = xlenter("binary=");

    s_current_thread = xlenter("current-self");
    s_thread_class = xlenter("<thread>");
    s_qualified_symbols = xlenter("qualified-symbols?");

    s_structure = xlenter("<structure>");

    s_set_generic_args = xlenter("set-generic-args!");

    s_macro_error = xlenter("macro-error");
#ifdef SOCK
    s_socket_error = xlenter("socket-error");
#endif

    s_debug = xlenter("debug");
    s_xlframe = xlenter("*xlframe*");

    s_gcmsgs = xlenter("*gc-msgs*");
    s_arg_list = xlenter("arg-list");
    s_next_methods = xlenter("next-methods");

#ifndef NO_CHECK_REF
    s_check_ref = xlenter("check-ref");
#endif
}
