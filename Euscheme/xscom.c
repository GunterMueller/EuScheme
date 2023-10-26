/* xscom.c - a simple scheme bytecode compiler */
/*	Copyright (c) 1988, by David Michael Betz
	All Rights Reserved */
/* Euscheme code Copyright (c) 1994 Russell Bradford */

#define NOISY_LOAD

#include "xscheme.h"
#include "xsbcode.h"

extern LVAL s_unbound, s_name, s_keyword, s_default, s_requiredp,
	    s_direct_slots, s_direct_keywords;
extern LVAL s_abstractp, s_predicate, s_constructor, s_keywords,
  	    s_superclasses;
extern LVAL s_reader, s_writer, s_accessor, s_class, s_find_slot_index;
extern LVAL s_getivar, s_setivar, s_list, s_lambda, s_object, s_value;
extern LVAL s_eval, s_import, s_only, s_except, s_rename;
extern LVAL s_callcc, s_stderr, s_defclass;
extern LVAL s_make, s_apply, s_setter, s_unwind_protect, s_letname;
extern LVAL s_begin, s_quote, s_compile, s_setmodule, s_getmodule, s_reintern;
extern LVAL s_module_directives, s_macro_error, s_compile_error;
extern LVAL s_syntax_error, s_telos_error;
#ifndef NO_CHECK_REF
extern LVAL s_check_ref, s_defun;
#endif

#define xlerror(a,b)  xlcerror(a,b,s_compile_error)

extern LVAL find_key(), xlreverse(), xlenter_keyword(), get_module();

static char *module_search_path[] = { MODULE_SEARCH_PATH, 0 };

/* size of code buffer */
#define CMAX	10000

/* continuation types */
#define C_RETURN	-1
#define C_NEXT		-2

#define SPACES() (spacy + 20 - (indent > 20 ? 20 : indent < 0 ? 0 : indent))

/* external variables */
extern LVAL true;

/* local variables */
static LVAL info;		/* compiler info */

/* code buffer */
static unsigned char cbuff[CMAX];	/* base of code buffer */
static int cbase;			/* base for current function */
static int cptr;			/* code buffer pointer */

/* forward declarations */
#ifdef __STDC__
static void do_expr(LVAL expr,int cont);
static int in_ntab(LVAL expr,int cont);
static int in_ftab(LVAL expr,int cont);
static void do_define(LVAL form,int cont);
static void do_defconstant(LVAL form,int cont);
static void define1(LVAL list,LVAL body,int cont);
static void do_set(LVAL form,int cont);
static void do_setvar(LVAL form,int cont);
static void do_quote(LVAL form,int cont);
static void do_lambda(LVAL form,int cont);
static void cd_fundefinition(LVAL fun,LVAL fargs,LVAL body);
static void parse_lambda_list(LVAL fargs,LVAL body);
static int find_internal_definitions(LVAL body,LVAL last);
static void do_delay(LVAL form,int cont);
static void do_let(LVAL form,int cont);
static void do_named_let(LVAL form,int cont);
static void cd_let(LVAL name,LVAL form,int cont);
static void do_letrec(LVAL form,int cont);
static void do_letstar(LVAL form,int cont);
static void letstar1(LVAL blist,LVAL body);
static int push_dummy_values(LVAL blist);
static int push_init_expressions(LVAL blist);
static void parse_let_variables(LVAL blist,LVAL body);
static void set_bound_variables(LVAL blist);
static LVAL make_code_object(LVAL fun);
static void do_cond(LVAL form,int cont);
static void do_and(LVAL form,int cont);
static void do_or(LVAL form,int cont);
static void do_if(LVAL form,int cont);
static void do_begin(LVAL form,int cont);
static void do_while(LVAL form,int cont);
static void do_access(LVAL form,int cont);
static void do_setaccess(LVAL form,int cont);
static void do_call(LVAL form,int cont);
static int push_args(LVAL form);
static void do_nary(int op,int n,LVAL form,int cont);
static int push_nargs(LVAL form,LVAL body,int n);
static void do_literal(LVAL lit,int cont);
static void do_identifier(LVAL sym,int cont);
static void do_continuation(int cont);
static int add_level(void);

static void remove_level(int oldcbase);
static int findvariable(LVAL sym,int *plev,int *poff);
static int findcvariable(LVAL sym,int *poff);
static int findliteral(LVAL lit);
static void cd_variable(int op,LVAL sym);
static void cd_evariable(int op,int lev,int off);
static void cd_literal(LVAL lit);
static int putcbyte(int b);
static int putcword(int w);
static void fixup(int chn);

static void do_defmodule(LVAL form, int cont);
static void do_export(LVAL form,int cont);
static void do_expose(LVAL form,int cont);
static void do_enter_module(LVAL form,int cont);
static void do_reenter_module(LVAL form,int cont);
static void do_import(LVAL form,int cont);
static void reintern_module_symbols(LVAL body);

static void do_define_generic(LVAL form,int cont);
static void do_define_method(LVAL form,int cont);
static void do_cnm(LVAL form,int cont);
static void do_next_method_p(LVAL form,int cont);
static void do_defclass(LVAL form,int cont);

static void do_defcondition(LVAL form,int cont);
static LVAL reintern_symbol(LVAL a);

LVAL append(LVAL a, LVAL b);
static LVAL filter_imports(LVAL implist, LVAL sofar);
extern LVAL xlmember(LVAL x, LVAL list, int (*fcn)());

#else

static int in_ntab(), in_ftab(), find_internal_definitions();
static int push_dummy_values(), push_init_expressions(), push_args();
static int push_nargs(), add_level(), findvariable(), findcvariable();
static int putcbyte(), putcword();
static void do_expr(), define1(), do_setvar(), cd_fundefinition();
static void parse_lambda_list(), do_named_let(), cd_let(), letstar1();
static void parse_let_variables(), set_bound_variables();
static void do_setaccess(), do_call(), do_nary(), do_literal();
static void do_identifier(), do_continuation(), remove_level();
static void cd_variable(), cd_evariable(), cd_literal(), fixup();
static void do_define(),do_defconstant(),
static void do_set(),do_quote(),do_lambda(),do_delay();
static void do_let(),do_letrec(),do_letstar(),do_cond(),do_and(),do_or();
static void do_if(),do_begin(),do_while(),do_access();
static void do_defmodule(), do_export(), do_expose(), do_enter_module();
static void do_reenter_module(), do_import(), reintern_module_symbols();
static void do_define_generic(), do_define_method(), do_cnm();
static void do_next_method_p(), do_defclass();
static void do_defcondition();
static LVAL make_code_object();
static void do_cnm(), do_next_method_p();
LVAL reintern_symbol();

LVAL append();
static LVAL filter_imports();
LVAL xlmember();

#endif

/* integrable function table */
typedef struct { char *nt_name; int nt_code,nt_args; } NTDEF;
static NTDEF *nptr,ntab[] = {
	{"atom?",		OP_ATOM,	1},
	{"eq?",			OP_EQ,		2},
	{"null?",		OP_NULL,	1},
	{"not",			OP_NULL,	1},
	{"cons",			OP_CONS,	2},
	{"car",			OP_CAR,		1},
	{"cdr",			OP_CDR,		1},
	{"set-car!",		OP_SETCAR,	2},
	{"set-cdr!",		OP_SETCDR,	2},
	{"+",			OP_ADD,		-2},
	{"-",			OP_SUB,		-2},
	{"*",			OP_MUL,		-2},
	{"/",			OP_DIV,		-2},
	{"quotient",		OP_QUO,		-2},
	{"<",			OP_LSS,		-2},
	{"=",			OP_EQL,		-2},
	{">",			OP_GTR,		-2},
	{"class-of",             OP_CLASSOF,      1},
	{"%GETIVAR",             OP_GETIVAR,      2},
	{"%SETIVAR",             OP_SETIVAR,      3},
	{"get",                  OP_GET,          2},
	{"put",                  OP_PUT,          3},
	{"current-module",       OP_CURMOD,       0},
	{"pair?",                OP_PAIRP,        1},
	{"symbol?",              OP_SYMBOLP,      1},
	{"vector?",              OP_VECTORP,      1},
	{"append",               OP_APPEND,       -2},
	{"list",                 OP_LIST,         -2},
	{"length",               OP_LENGTH,       1},
	{"reverse",              OP_REVERSE,      1},
	{"caar",                 OP_CAAR,         1},
	{"cadr",                 OP_CADR,         1},
	{"cdar",                 OP_CDAR,         1},
	{"cddr",                 OP_CDDR,         1},
	{"get-syntax",		OP_GETSYNTAX,    2},
	{"put-syntax",		OP_PUTSYNTAX,    3},
#ifndef NO_CHECK_REF
	{"check-ref",    	OP_CHECKREF,     2},
#endif
	{(char*)0, 0, 0}
};

/* special form table */
typedef struct { char *ft_name; void (*ft_fcn)(); } FTDEF;
FTDEF ftab[] = {
	{"quote",	do_quote},
	{"lambda",	do_lambda},
	{"delay",	do_delay},
	{"let",		do_let},
	{"let*",		do_letstar},
	{"letrec",	do_letrec},
	{"define",	do_define},
	{"defconstant",	do_defconstant},
	{"setq",		do_set},
	{"if",		do_if},
	{"cond",		do_cond},
	{"begin",	do_begin},
	{"sequence",	do_begin},
	{"and",		do_and},
	{"or",		do_or},
	{"while",	do_while},
	{"access",	do_access},
        {"defmodule",    do_defmodule},
	{"export",       do_export},
	{"expose",       do_expose},
	{"enter-module", do_enter_module},
	{"!>",           do_enter_module},
	{"reenter-module", do_reenter_module},
	{"!>>",          do_reenter_module},
        {"%IMPORT",       do_import},
	{"define-generic",   do_define_generic},
	{"define-method",    do_define_method},
	{"call-next-method", do_cnm},
	{"next-method?", do_next_method_p},
	{"defclass",     do_defclass},
	{"defcondition", do_defcondition},
	{(char*)0, (void(*)())0}
};

/* xlcompile - compile an expression */
LVAL xlcompile(expr,ctenv)
  LVAL expr,ctenv;
{
    /* initialize the compile time environment */
    info = cons(NIL,NIL); cpush(info);
    rplaca(info,newframe(ctenv,1));
    rplacd(info,cons(NIL,NIL));

    /* setup the base of the code for this function */
    cbase = cptr = 0;

    /* setup the entry code */
    putcbyte(OP_FRAME);
    putcbyte(1);

    /* compile the expression */
    do_expr(expr,C_RETURN);

    /* build the code object */
    settop(make_code_object(NIL));
    return (pop());
}

/* xlfunction - compile a function */
LVAL xlfunction(fun,fargs,body,ctenv)
  LVAL fun,fargs,body,ctenv;
{
    /* initialize the compile time environment */
    info = cons(NIL,NIL); cpush(info);
    rplaca(info,newframe(ctenv,1));
    rplacd(info,cons(NIL,NIL));

    /* setup the base of the code for this function */
    cbase = cptr = 0;

    /* compile the lambda list and the function body */
    parse_lambda_list(fargs,body);
    do_begin(body,C_RETURN);

    /* build the code object */
    settop(make_code_object(fun));
    return (pop());
}

/* do_expr - compile an expression */
/* (deflocal a (let ((lambda (lambda (lambda) lambda))) (lambda lambda))) */
static void do_expr(expr,cont)
  LVAL expr; int cont;
{
    LVAL sym;
    int lev, off;

    cpush(expr);

    if (consp(expr)) {
	sym = car(expr);
 	if (!symbolp(sym) ||	/* ((foo 1) 2) */
	    findvariable(sym,&lev,&off) || /* (let ((car cdr)) (car x)) */
	    getmodule(sym) != root_module ||
	    (!in_ntab(expr,cont) && !in_ftab(expr,cont)))
	  do_call(expr,cont);
    }
    else if (symbolp(expr))
	do_identifier(expr,cont);
    else
	do_literal(expr,cont);

    drop(1);
}

/* in_ntab - check for a function in ntab */
static int in_ntab(expr,cont)
  LVAL expr; int cont;
{
    char *pname;
    LVAL sym;

    sym = car(expr);

    pname = getstring(getpname(sym));
    for (nptr = ntab; nptr->nt_name; ++nptr)
	if (strcmp(pname,nptr->nt_name) == 0) {
	    do_nary(nptr->nt_code,nptr->nt_args,expr,cont);
	    return (TRUE);
	}
    return (FALSE);
}

/* in_ftab - check for a function in ftab */
static int in_ftab(expr,cont)
  LVAL expr; int cont;
{
    char *pname;
    FTDEF *fptr;
    LVAL sym;

    sym = car(expr);

    pname = getstring(getpname(sym));
    for (fptr = ftab; fptr->ft_name; ++fptr)
	if (strcmp(pname,fptr->ft_name) == 0) {
	    (*fptr->ft_fcn)(cdr(expr),cont);
	    return (TRUE);
	}
    return (FALSE);
}

/* do_define - handle the (DEFINE ... ) expression */
static void do_define(form,cont)
  LVAL form; int cont;
{
    if (atom(form))
	xlerror("expecting symbol or function template",form);
    define1(car(form),cdr(form),cont);
}

/* do_defconstant - handle the (defconstant ... ) expression */
static void do_defconstant(form,cont)
  LVAL form; int cont;
{
  LVAL sym;
  int off;

  if (atom(form))
    xlerror("expecting symbol in defconstant", form);

  sym = car(form);
  if (!symbolp(sym))
    xlerror("expecting a symbol in defconstant", sym);

  if (keywordp(sym))
    xlerror("trying to set a keyword in defconstant", sym);

  form = cdr(form);

  if (form && atom(form))
    xlerror("expecting value expression in defconstant",form);

  /* compile the value expression */
  do_expr(form == NIL ? NIL : car(form), C_NEXT);

  /* define the variable value */
  if (findcvariable(sym,&off))
    xlerror("defconstant not at top level", sym);

  cd_variable(OP_GSET,sym);
  setconstant(sym, true);

  do_literal(sym,cont);
}


/* define1 - helper routine for do_define */
static void define1(list,body,cont)
  LVAL list,body; int cont;
{
    LVAL fargs;
    int off;

    /* handle nested definitions */
    if (consp(list)) {
	cpush(cons(s_lambda,NIL));              /* (LAMBDA) */
	rplacd(top(),cons(cdr(list),NIL));	/* (LAMBDA args) */
	rplacd(cdr(top()),body);		/* (LAMBDA args body) */
	settop(cons(top(),NIL));		/* ((LAMBDA args body)) */
	define1(car(list),top(),cont);
	drop(1);
    }
    
    /* compile procedure definitions */
    else {

	/* make sure it's a symbol */
	if (!symbolp(list))
	    xlerror("expecting a symbol",list);

	/* check for a procedure definition */
	if (consp(body)
        &&  consp(car(body))
        &&  car(car(body)) == s_lambda) {
	    fargs = car(cdr(car(body)));
	    body = cdr(cdr(car(body)));
	    cd_fundefinition(list,fargs,body);
	}

	/* compile the value expression or procedure body */
	else
	    do_begin(body,C_NEXT);
    
	/* define the variable value */
	if (findcvariable(list,&off))
	    cd_evariable(OP_ESET,0,off);
	else
	    cd_variable(OP_GSET,list);
	do_literal(list,cont);
    }
}

/* do_set - compile the (SET! ... ) expression */
static void do_set(form,cont)
  LVAL form; int cont;
{
    if (atom(form))
	xlerror("expecting symbol or ACCESS form",form);
    else if (symbolp(car(form)))
	do_setvar(form,cont);
    else if (consp(car(form)))
	do_setaccess(form,cont);
    else
	xlerror("expecting symbol or ACCESS form",form);
}

/* do_setvar - compile the (SET! var value) expression */
static void do_setvar(form,cont)
  LVAL form; int cont;
{
    int lev,off;
    LVAL sym;

    /* get the variable name */
    sym = car(form);

    if (keywordp(sym))
      xlerror("trying to set a keyword", sym);

    /* compile the value expression */
    form = cdr(form);
    if (atom(form))
	xlerror("expecting value expression",form);
    do_expr(car(form),C_NEXT);

    /* set the variable value */
    if (findvariable(sym,&lev,&off))
	cd_evariable(OP_ESET,lev,off);
    else if (constantp(sym))
        xlerror("trying to set a constant binding", sym);
    else
	cd_variable(OP_GSET,sym);
    do_continuation(cont);
}

/* do_quote - compile the (QUOTE ... ) expression */
static void do_quote(form,cont)
  LVAL form; int cont;
{
    if (atom(form))
	xlerror("expecting quoted expression",form);
    do_literal(car(form),cont);
}

/* do_lambda - compile the (LAMBDA ... ) expression */
static void do_lambda(form,cont)
  LVAL form; int cont;
{
    if (atom(form))
	xlerror("expecting argument list",form);
    cd_fundefinition(NIL,car(form),cdr(form));
    do_continuation(cont);
}

/* cd_fundefinition - compile the function */
static void cd_fundefinition(fun,fargs,body)
  LVAL fun,fargs,body;
{
    int oldcbase;

    /* establish a new environment frame */
    oldcbase = add_level();

    /* compile the lambda list and the function body */
    parse_lambda_list(fargs,body);
    do_begin(body,C_RETURN);

    /* build the code object */
    cpush(make_code_object(fun));
    
    /* restore the previous environment */
    remove_level(oldcbase);

    /* compile code to create a closure */
    do_literal(pop(),C_NEXT);
    putcbyte(OP_CLOSE);
}

/* parse_lambda_list - parse the formal argument list */
static void parse_lambda_list(fargs,body)
  LVAL fargs,body;
{
    LVAL arg,restarg,new,last;
    int frame,slotn;
    
    /* setup the entry code */
    putcbyte(OP_FRAME);
    frame = putcbyte(0);

    /* initialize the argument name list and slot number */
    restarg = last = NIL;
    slotn = 1;
    
    /* handle each required argument */
    while (consp(fargs) && (arg = car(fargs)) != NIL) {

	/* make sure the argument is a symbol */
	if (!symbolp(arg))
	    xlerror("variable in lambda-list must be a symbol",arg);

	/* and not a keyword */
	if (keywordp(arg))
	  xlerror("trying to bind a keyword in lambda-list", arg);

	/* add the argument name to the name list */
	new = cons(arg,NIL);
	if (last) rplacd(last,new);
	else setelement(car(car(info)),0,new);
	last = new;

	/* generate an instruction to move the argument into the frame */
	putcbyte(OP_MVARG);
	putcbyte(slotn++);
	
	/* move the formal argument list pointer ahead */
	fargs = cdr(fargs);
    }

    /* check for the a dotted tail */
    if (restarg == NIL && symbolp(fargs)) {
	restarg = fargs;

	if (keywordp(restarg))
	  xlerror("trying to bind keyword in lambda-list", restarg);

	/* add the argument name to the name list */
	new = cons(restarg,NIL);
	if (last) rplacd(last,new);
	else setelement(car(car(info)),0,new);
	last = new;

	/* make the #!rest argument list */
	putcbyte(OP_MVRARG);
	putcbyte(slotn++);
	fargs = NIL;
    }

    /* check for the end of the argument list */
    if (fargs != NIL)
	xlerror("bad argument list tail in lambda-list",fargs);

    /* make sure the user didn't supply too many arguments */
    if (restarg == NIL)
	putcbyte(OP_ALAST);
	
    /* scan the body for internal definitions */
    slotn += find_internal_definitions(body,last);
	
    /* fixup the frame instruction */
    cbuff[cbase+frame] = slotn;
}

/* find_internal_definitions - find internal definitions */
static int find_internal_definitions(body,last)
  LVAL body,last;
{
    LVAL define,sym,new;
    int n=0;

    /* look for all (define...) forms */
    for (define = xlenter("define"); consp(body); body = cdr(body))
	if (consp(car(body)) && car(car(body)) == define) {
	    sym = cdr(car(body)); /* the rest of the (define...) form */
	    if (consp(sym)) {     /* make sure there is a second subform */
		sym = car(sym);   /* get the second subform */
		while (consp(sym))/* check for a procedure definition */
		    sym = car(sym);
		if (symbolp(sym)) {
		    new = cons(sym,NIL);
		    if (last) rplacd(last,new);
		    else setelement(car(car(info)),0,new);
		    last = new;
		    ++n;
		}
	    }
	}
    return (n);
}

/* do_delay - compile the (DELAY ... ) expression */
static void do_delay(form,cont)
  LVAL form; int cont;
{
    int oldcbase;

    /* check argument list */
    if (atom(form))
	xlerror("expecting delay expression",form);

    /* establish a new environment frame */
    oldcbase = add_level();

    /* setup the entry code */
    putcbyte(OP_FRAME);
    putcbyte(1);

    /* compile the expression */
    do_expr(car(form),C_RETURN);

    /* build the code object */
    cpush(make_code_object(NIL));
    
    /* restore the previous environment */
    remove_level(oldcbase);

    /* compile code to create a closure */
    do_literal(pop(),C_NEXT);
    putcbyte(OP_DELAY);
    do_continuation(cont);
}

/* do_let - compile the (LET ... ) expression */
static void do_let(form,cont)
  LVAL form; int cont;
{
    /* handle named let */
    if (consp(form) && symbolp(car(form)))
	do_named_let(form,cont);
    
    /* handle unnamed let */
    else
        cd_let(NIL,form,cont);
}

/* do_named_let - compile the (LET name ... ) expression */
static void do_named_let(form,cont)
  LVAL form; int cont;
{
    int oldcbase,nxt;

    /* save a continuation */
    if (cont != C_RETURN) {
	putcbyte(OP_SAVE);
	nxt = putcword(0);
    }
    
    /* establish a new environment frame */
    oldcbase = add_level();
    setelement(car(car(info)),0,cons(car(form),NIL));

    /* setup the entry code */
    putcbyte(OP_FRAME);
    putcbyte(2);
    
    /* compile the let expression */
    cd_let(car(form),cdr(form),C_RETURN);

    /* build the code object */
    cpush(make_code_object(s_letname));
    
    /* restore the previous environment */
    remove_level(oldcbase);

    /* compile code to create a closure */
    do_literal(pop(),C_NEXT);
    putcbyte(OP_CLOSE);

    /* apply the function */
    putcbyte(OP_CALL);
    putcbyte(1);

    /* target for the continuation */
    if (cont != C_RETURN)
	fixup(nxt);
}

/* cd_let - code a let expression */
static void cd_let(name,form,cont)
  LVAL name,form; int cont;
{
    int oldcbase,nxt,lev,off,n;

    /* make sure there is a binding list */
    if (atom(form) || !listp(car(form)))
	xlerror("expecting binding list",form);

    /* save a continuation */
    if (cont != C_RETURN) {
	putcbyte(OP_SAVE);
	nxt = putcword(0);
    }
    
    /* push the initialization expressions */
    n = push_init_expressions(car(form));

    /* establish a new environment frame */
    oldcbase = add_level();

    /* compile the binding list */
    parse_let_variables(car(form),cdr(form));

    /* compile the body of the let/letrec */
    do_begin(cdr(form),C_RETURN);

    /* build the code object */
    cpush(make_code_object(s_letname));
    
    /* restore the previous environment */
    remove_level(oldcbase);

    /* compile code to create a closure */
    do_literal(pop(),C_NEXT);
    putcbyte(OP_CLOSE);

    /* store the procedure */
    if (name && findvariable(name,&lev,&off))
	cd_evariable(OP_ESET,lev,off);

    /* apply the function */
    putcbyte(OP_CALL);
    putcbyte(n);

    /* target for the continuation */
    if (cont != C_RETURN)
	fixup(nxt);
}

/* do_letrec - compile the (LETREC ... ) expression */
static void do_letrec(form,cont)
  LVAL form; int cont;
{
    int oldcbase,nxt,n;

    /* make sure there is a binding list */
    if (atom(form) || !listp(car(form)))
	xlerror("expecting binding list",form);

    /* save a continuation */
    if (cont != C_RETURN) {
	putcbyte(OP_SAVE);
	nxt = putcword(0);
    }
    
    /* push the initialization expressions */
    n = push_dummy_values(car(form));

    /* establish a new environment frame */
    oldcbase = add_level();

    /* compile the binding list */
    parse_let_variables(car(form),cdr(form));

    /* compile instructions to set the bound variables */
    set_bound_variables(car(form));
    
    /* compile the body of the let/letrec */
    do_begin(cdr(form),C_RETURN);

    /* build the code object */
    cpush(make_code_object(s_letname));
    
    /* restore the previous environment */
    remove_level(oldcbase);

    /* compile code to create a closure */
    do_literal(pop(),C_NEXT);
    putcbyte(OP_CLOSE);

    /* apply the function */
    putcbyte(OP_CALL);
    putcbyte(n);

    /* target for the continuation */
    if (cont != C_RETURN)
	fixup(nxt);
}

/* do_letstar - compile the (LET* ... ) expression */
static void do_letstar(form,cont)
  LVAL form; int cont;
{
    int nxt;
    
    /* make sure there is a binding list */
    if (atom(form) || !listp(car(form)))
	xlerror("expecting binding list",form);

    /* handle the case where there are bindings */
    if (consp(car(form))) {
    
	/* save a continuation */
	if (cont != C_RETURN) {
	    putcbyte(OP_SAVE);
	    nxt = putcword(0);
	}
    
	/* build the nested lambda expressions */
	letstar1(car(form),cdr(form));
    
	/* target for the continuation */
	if (cont != C_RETURN)
	    fixup(nxt);
    }
    
    /* handle the case where there are no bindings */
    else
	do_begin(cdr(form),cont);
}

/* letstar1 - helper routine for let* */
static void letstar1(blist,body)
  LVAL blist,body;
{
    int oldcbase,n;

    /* push the next initialization expressions */
    cpush(cons(car(blist),NIL));
    n = push_init_expressions(top());

    /* establish a new environment frame */
    oldcbase = add_level();

    /* handle the case where there are more bindings */
    if (consp(cdr(blist))) {
	parse_let_variables(top(),NIL);
	letstar1(cdr(blist),body);
    }
    
    /* handle the last binding */
    else {
	parse_let_variables(top(),body);
	do_begin(body,C_RETURN);
    }
	
    /* build the code object */
    settop(make_code_object(s_letname));
    
    /* restore the previous environment */
    remove_level(oldcbase);

    /* compile code to create a closure */
    do_literal(pop(),C_NEXT);
    putcbyte(OP_CLOSE);

    /* apply the function */
    putcbyte(OP_CALL);
    putcbyte(n);
}

/* push_dummy_values - push dummy values for a 'letrec' expression */
static int push_dummy_values(blist)
  LVAL blist;
{
    int n=0;
    if (consp(blist)) {
	putcbyte(OP_NIL);
	for (; consp(blist); blist = cdr(blist), ++n)
	    putcbyte(OP_PUSH);
    }
    return (n);
}

/* push_init_expressions - push init expressions for a 'let' expression */
static int push_init_expressions(blist)
  LVAL blist;
{
    int n;
    if (consp(blist)) {
	n = push_init_expressions(cdr(blist));
	if (consp(car(blist)) && consp(cdr(car(blist))))
	    do_expr(car(cdr(car(blist))),C_NEXT);
	else
	    putcbyte(OP_NIL);
	putcbyte(OP_PUSH);
	return (n+1);
    }
    return (0);
}

/* parse_let_variables - parse the binding list */
static void parse_let_variables(blist,body)
  LVAL blist,body;
{
    LVAL arg,new,last;
    int frame,slotn;
    
    /* setup the entry code */
    putcbyte(OP_FRAME);
    frame = putcbyte(0);

    /* initialize the argument name list and slot number */
    last = NIL;
    slotn = 1;
    
    /* handle each required argument */
    while (consp(blist) && (arg = car(blist)) != NIL) {

        if (keywordp(arg) || (consp(arg) && keywordp(car(arg))))
	  xlerror("trying to bind a keyword in let", arg);

	/* make sure the argument is a symbol */
	if (symbolp(arg))
	    new = cons(arg,NIL);
	else if (consp(arg) && symbolp(car(arg)))
	    new = cons(car(arg),NIL);
	else
	    xlerror("invalid binding in let",arg);

	/* add the argument name to the name list */
	if (last) rplacd(last,new);
	else setelement(car(car(info)),0,new);
	last = new;

	/* generate an instruction to move the argument into the frame */
	putcbyte(OP_MVARG);
	putcbyte(slotn++);
	
	/* move the formal argument list pointer ahead */
	blist = cdr(blist);
    }
    putcbyte(OP_ALAST);

    /* scan the body for internal definitions */
    slotn += find_internal_definitions(body,last);
	
    /* fixup the frame instruction */
    cbuff[cbase+frame] = slotn;
}

/* set_bound_variables - set bound variables in a 'letrec' expression */
static void set_bound_variables(blist)
  LVAL blist;
{
    int lev,off;
    for (; consp(blist); blist = cdr(blist)) {
	if (consp(car(blist)) && consp(cdr(car(blist)))) {
	    do_expr(car(cdr(car(blist))),C_NEXT);
	    if (findvariable(car(car(blist)),&lev,&off))
		cd_evariable(OP_ESET,lev,off);
	    else
		xlerror("compiler error -- can't find",car(car(blist)));
	}
    }
}

/* make_code_object - build a code object */
static LVAL make_code_object(fun)
  LVAL fun;
{
    unsigned char *cp;
    LVAL code,p;
    int i;

    /* create a code object */
    code = newcode(FIRSTLIT + length(car(cdr(info)))); cpush(code);
    setbcode(code,newstring(cptr - cbase));
    setcname(code,fun);			       	 /* function name */
    setvnames(code,getelement(car(car(info)),0));/* lambda list variables */

    /* copy the literals into the code object */
    for (i = FIRSTLIT, p = car(cdr(info)); consp(p); p = cdr(p), ++i)
	setelement(code,i,car(p));

    /* copy the byte codes */
    for (i = cbase, cp = (unsigned char *)getstring(getbcode(code));
	 i < cptr; )
	*cp++ = cbuff[i++];

    /* return the new code object */
    return (pop());
}

/* do_cond - compile the (COND ... ) expression */
static void do_cond(form,cont)
  LVAL form; int cont;
{
    int nxt,end;
    if (consp(form)) {
	for (end = 0; consp(form); form = cdr(form)) {
	    if (atom(car(form)))
		xlerror("expecting a cond clause",form);
	    do_expr(car(car(form)),C_NEXT);
	    putcbyte(OP_BRF);
	    nxt = putcword(0);
	    if (cdr(car(form)))
		do_begin(cdr(car(form)),cont);
	    else
		do_continuation(cont);
	    if (cont == C_NEXT) {
		putcbyte(OP_BR);
		end = putcword(end);
	    }
	    fixup(nxt);
	}
	fixup(end);
    }
    else
	putcbyte(OP_NIL);
    do_continuation(cont);
}

/* do_and - compile the (AND ... ) expression */
static void do_and(form,cont)
  LVAL form; int cont;
{
    int end;
    if (consp(form)) {
	for (end = 0; consp(form); form = cdr(form)) {
	    if (cdr(form)) {
		do_expr(car(form),C_NEXT);
		putcbyte(OP_BRF);
		end = putcword(end);
	    }
	    else
		do_expr(car(form),cont);
	}
	fixup(end);
    }
    else
	putcbyte(OP_T);
    do_continuation(cont);
}

/* do_or - compile the (OR ... ) expression */
static void do_or(form,cont)
  LVAL form; int cont;
{
    int end;
    if (consp(form)) {
	for (end = 0; consp(form); form = cdr(form)) {
	    if (cdr(form)) {
		do_expr(car(form),C_NEXT);
		putcbyte(OP_BRT);
		end = putcword(end);
	    }
	    else
		do_expr(car(form),cont);
	}
	fixup(end);
    }
    else
	putcbyte(OP_NIL);
    do_continuation(cont);
}

/* do_if - compile the (IF ... ) expression */
static void do_if(form,cont)
  LVAL form; int cont;
{
    int nxt,end;

    /* compile the test expression */
    if (atom(form))
	xlerror("expecting test expression",form);
    do_expr(car(form),C_NEXT);

    /* skip around the 'then' clause if the expression is false */
    putcbyte(OP_BRF);
    nxt = putcword(0);

    /* skip to the 'then' clause */
    form = cdr(form);
    if (atom(form))
	xlerror("expecting then clause",form);

    /* compile the 'then' and 'else' clauses */
    if (consp(cdr(form))) {
	if (cont == C_NEXT) {
	    do_expr(car(form),C_NEXT);
	    putcbyte(OP_BR);
	    end = putcword(0);
	}
	else {
	    do_expr(car(form),cont);
	    end = -1;
	}
	fixup(nxt);
	do_expr(car(cdr(form)),cont);
	nxt = end;
    }

    /* compile just a 'then' clause */
    else
	do_expr(car(form),cont);

    /* handle the end of the statement */
    if (nxt >= 0) {
	fixup(nxt);
	do_continuation(cont);
    }
}

/* do_begin - compile the (BEGIN ... ) expression */
static void do_begin(form,cont)
  LVAL form; int cont;
{
    if (consp(form))
	for (; consp(form); form = cdr(form))
	    if (consp(cdr(form)))
		do_expr(car(form),C_NEXT);
	    else
		do_expr(car(form),cont);
    else {
	putcbyte(OP_NIL);
	do_continuation(cont);
    }
}

/* do_while - compile the (WHILE ... ) expression */
static void do_while(form,cont)
  LVAL form; int cont;
{
    int loop,nxt;

    /* make sure there is a test expression */
    if (atom(form))
	xlerror("expecting test expression",form);

    /* skip around the 'body' to the test expression */
    putcbyte(OP_BR);
    nxt = putcword(0);

    /* compile the loop body */
    loop = cptr - cbase;
    do_begin(cdr(form),C_NEXT);

    /* label for the first iteration */
    fixup(nxt);

    /* compile the test expression */
    nxt = cptr - cbase;
    do_expr(car(form),C_NEXT);

    /* skip around the 'body' if the expression is false */
    putcbyte(OP_BRT);
    putcword(loop);

    /* compile the continuation */
    do_continuation(cont);
}

/* do_access - compile the (ACCESS var env) expression */
static void do_access(form,cont)
  LVAL form; int cont;
{
    LVAL sym;

    /* get the variable name */
    if (atom(form) || !symbolp(car(form)))
	xlerror("expecting symbol",form);
    sym = car(form);

    /* compile the environment expression */
    form = cdr(form);
    if (atom(form))
	xlerror("expecting environment expression",form);
    do_expr(car(form),C_NEXT);

    /* get the variable value */
    cd_variable(OP_AREF,sym);
    do_continuation(cont);
}

/* do_setaccess - compile the (SET! (ACCESS var env) value) expression */
static void do_setaccess(form,cont)
  LVAL form; int cont;
{
    LVAL aform,sym;

    /* make sure this is an access form */
    aform = car(form);
    if (atom(aform) || car(aform) != xlenter_module("access", root_module))
	xlerror("expecting an ACCESS form",aform);

    /* get the variable name */
    aform = cdr(aform);
    if (atom(aform) || !symbolp(car(aform)))
	xlerror("expecting symbol",aform);
    sym = car(aform);

    /* compile the environment expression */
    aform = cdr(aform);
    if (atom(aform))
	xlerror("expecting environment expression",aform);
    do_expr(car(aform),C_NEXT);
    putcbyte(OP_PUSH);

    /* compile the value expression */
    form = cdr(form);
    if (atom(form))
	xlerror("expecting value expression",form);
    do_expr(car(form),C_NEXT);

    /* set the variable value */
    cd_variable(OP_ASET,sym);
    do_continuation(cont);
}

/* do_call - compile a function call */
static void do_call(form,cont)
  LVAL form; int cont;
{
    int nxt,n;
    
    /* save a continuation */
    if (cont != C_RETURN) {
	putcbyte(OP_SAVE);
	nxt = putcword(0);
    }
    
    /* compile each argument expression */
    n = push_args(cdr(form));

    /* compile the function itself */
    do_expr(car(form),C_NEXT);

    /* apply the function */
    putcbyte(OP_CALL);
    putcbyte(n);

    /* target for the continuation */
    if (cont != C_RETURN)
	fixup(nxt);
}

/* push_args - compile the arguments for a function call */
static int push_args(form)
  LVAL form;
{
    int n;
    if (consp(form)) {
	n = push_args(cdr(form));
	do_expr(car(form),C_NEXT);
	putcbyte(OP_PUSH);
	return (n+1);
    }
    return (0);
}

/* do_nary - compile nary operator expressions */
static void do_nary(op,n,form,cont)
  int op,n; LVAL form; int cont;
{
    if (n < 0 && (n = (-n)) != length(cdr(form)))
	do_call(form,cont);
    else {
	push_nargs(car(form), cdr(form), n);
	putcbyte(op);
	do_continuation(cont);
    }
}

/* push_nargs - compile the arguments for an inline function call */
static int push_nargs(fun, body, n)
  LVAL fun, body; int n;
{
    if (consp(body)) {
	if (n == 0)
	    xlerror("too many arguments", fun);
	if (push_nargs(fun, cdr(body), n-1))
	    putcbyte(OP_PUSH);
	do_expr(car(body), C_NEXT);
	return (TRUE);
    }
    if (n)
	xlerror("too few arguments", fun);
    return (FALSE);
}

/* do_literal - compile a literal */
static void do_literal(lit,cont)
  LVAL lit; int cont;
{
    cd_literal(lit);
    do_continuation(cont);
}

/* do_identifier - compile an identifier */
static void do_identifier(sym,cont)
  LVAL sym; int cont;
{
    int lev,off;
    if (sym == true)
	putcbyte(OP_T);
    else if (findvariable(sym,&lev,&off))
	cd_evariable(OP_EREF,lev,off);
    else
	cd_variable(OP_GREF,sym);
    do_continuation(cont);
}

/* do_continuation - compile a continuation */
static void do_continuation(cont)
  int cont;
{
    switch (cont) {
    case C_RETURN:
	putcbyte(OP_RETURN);
	break;
    case C_NEXT:
	break;
    }
}

/* add_level - add a nesting level */
static int add_level()
{
    int oldcbase;
    
    /* establish a new environment frame */
    rplaca(info,newframe(car(info),1));
    rplacd(info,cons(NIL,cdr(info)));

    /* setup the base of the code for this function */
    oldcbase = cbase;
    cbase = cptr;

    /* return the old code base */
    return (oldcbase);
}

/* remove_level - remove a nesting level */
static void remove_level(oldcbase)
  int oldcbase;
{
    /* restore the previous environment */
    rplaca(info,cdr(car(info)));
    rplacd(info,cdr(cdr(info)));

    /* restore the base and code pointer */
    cptr = cbase;
    cbase = oldcbase;
}

/* findvariable - find an environment variable */
static int findvariable(sym,plev,poff)
  LVAL sym; int *plev,*poff;
{
    int lev,off;
    LVAL e,a;
    for (e = car(info), lev = 0; envp(e); e = cdr(e), ++lev)
	for (a = getelement(car(e),0), off = 1; consp(a); a = cdr(a), ++off)
	    if (sym == car(a)) {
		*plev = lev;
		*poff = off;
		return (TRUE);
	    }
    return (FALSE);
}

/* findcvariable - find an environment variable in the current frame */
static int findcvariable(sym,poff)
  LVAL sym; int *poff;
{
    int off;
    LVAL a;
    a = getelement(car(car(info)),0);
    for (off = 1; consp(a); a = cdr(a), ++off)
	if (sym == car(a)) {
	    *poff = off;
	    return (TRUE);
	}
    return (FALSE);
}

/* litequal - test for equality that distinguishes symbols from
   different modules */
static int litequal(a, b)
  LVAL a, b;
{
  if (a == b) return TRUE;
  if (symbolp(a)) return FALSE;
  return equal(a, b);
}

/* findliteral - find a literal in the literal frame */
static int findliteral(lit)
  LVAL lit;
{
  int o = FIRSTLIT;
  LVAL t,p;
  if ((t = car(cdr(info))) != NIL) {
    for (p = NIL; consp(t); p = t, t = cdr(t), ++o)
      if (litequal(lit,car(t)))
	return (o);
    rplacd(p,cons(lit,NIL));
  }
  else
    rplaca(cdr(info),cons(lit,NIL));
  return (o);
}

/* cd_variable - compile a variable reference */
static void cd_variable(op,sym)
  int op; LVAL sym;
{
    int index;

    index = findliteral(sym);
    if (index < 256) {
      putcbyte(op);
      putcbyte(index);
    }
    else {
      putcbyte(op + L_OFF);
      putcword(index);
    }
}

/* cd_evariable - compile an environment variable reference */
static void cd_evariable(op,lev,off)
  int op,lev,off;      
{
    putcbyte(op);
    putcbyte(lev);
    putcbyte(off);
}

/* cd_literal - compile a literal reference */
static void cd_literal(lit)
  LVAL lit;
{
    int index;

    if (lit == NIL)
	putcbyte(OP_NIL);
    else if (lit == true)
	putcbyte(OP_T);
    else {
        index = findliteral(lit);
	if (index < 256) {
	  putcbyte(OP_LIT);
	  putcbyte(index);
	}
	else {
	  putcbyte(OP_LITL);
	  putcword(index);
	}
    }
}

/* putcbyte - put a code byte into data space */
static int putcbyte(b)
  int b;
{
    int adr;
    if (cptr >= CMAX)
	xlabort("insufficient code space");
    adr = (cptr - cbase);
    cbuff[cptr++] = b;
    return (adr);
}

/* putcword - put a code word into data space */
static int putcword(w)
  int w;
{
    int adr;
    adr = putcbyte(w >> 8);
    putcbyte(w);
    return (adr);
}

/* fixup - fixup a reference chain */
static void fixup(chn)
  int chn;
{
    int val,hval,nxt;

    /* store the value into each location in the chain */
    val = cptr - cbase; hval = val >> 8;
    while (chn) {
	nxt = (cbuff[cbase+chn] << 8) | (cbuff[cbase+chn+1]);
	cbuff[cbase+chn] = hval;
	cbuff[cbase+chn+1] = val;
	chn = nxt;
    }
}

/* length - find the length of a list */
int length(list)
  LVAL list;
{
    int len;
    for (len = 0; consp(list); list = cdr(list))
	++len;
    return (len);
}

/* check if symbol is in list -- error if from different module */
LVAL findsym(symbol, list)
  LVAL symbol, list;
{
  LVAL sym, module;
  char *name;

  name = getstring(getpname(symbol));
  module = getmodule(symbol);

   for (sym = list; sym; sym = cdr(sym))
     if (strcmp(name,getstring(getpname(car(sym)))) == 0) {
       if (getmodule(car(sym)) == module)
	 return car(sym);
       xlerror("symbol name clash in defmodule or import", car(sym));
     }

  return NIL;
}

/* try to load a module */
static int load_module(sym)
  LVAL sym;
{
  char name[256];
  LVAL file, expr, curmod;
  FILE *fp;
  extern FILE *osaopen();
  extern int quiet, osclose();
  static int indent = 0;
  static char *spacy = "                    ";
  extern FILE *path_open();
  char buf[256], path[256];
  int readit;

  if (symbolp(sym)) strcpy(name, getstring(getpname(sym)));
  else strcpy(name, getstring(sym));
#ifndef RISCOS
  strcat(name, ".em");
#endif

  cpush(sym);

  fp = path_open(name, "EU_MODULE_PATH", module_search_path, path);

#ifdef RISCOS
  if (fp == NULL) {
    strcat(name, ".em");
    fp = path_open(name, "EU_MODULE_PATH", module_search_path, path);
  }
#endif

  if (fp == NULL) {
    drop(1);
    return FALSE;			/* fail */
  }

  file = cvport(fp, PF_INPUT);
  cpush(file);

  if (!quiet) {
    if (strcmp(path, ".") == 0)
      sprintf(buf, "%s<reading %s>\n", SPACES(), name);
    else
      sprintf(buf, "%s<reading %s/%s>\n", SPACES(), path, name);
    xlputstr(getvalue(s_stderr),buf);
  }
  indent += 2;

  curmod = current_module;
  current_module = root_module;	/* read the form in root module */
#ifdef TRACE_SETMODULE
xlputstr(curoutput(), "<2curmod=root>");
#endif

  readit = xlread(file,&expr);
  setfile(file, NULL);
  osclose(fp);

  if (readit) {
    if (!consp(expr) ||
	!symbolp(car(expr)) ||
	strcmp(getstring(getpname(car(expr))), "defmodule")) {
      current_module = curmod;
#ifdef TRACE_SETMODULE
xlputstr(curoutput(), "<3curmod=");xlprin1(current_module,curoutput());
xlputstr(curoutput(), ">");
#endif
      xlerror("not a defmodule in file", sym);
    }
    cpush(expr);
    do_defmodule(cdr(expr),C_NEXT);
    drop(1);
  }				/* else fail */
  else {
    xlerror("error in reading module file", sym);
  }

  current_module = curmod;
#ifdef TRACE_SETMODULE
xlputstr(curoutput(), "<4curmod=");xlprin1(current_module,curoutput());
xlputstr(curoutput(), ">");
#endif

  indent -= 2;
  if (!quiet) {
    sprintf(buf, "%s<read %s>\n", SPACES(), name);
    xlputstr(getvalue(s_stderr),buf);
  }

  drop(2);
  return TRUE;
}

/* sym a string or symbol */
LVAL find_or_load_module(sym)
  LVAL sym;
{
  LVAL mod;

  cpush(sym);
  mod = find_module(sym);
  if (mod == NIL) {
    (void)load_module(sym);
    mod = find_module(sym);
  }
  drop(1);
  return mod;
}

static void add_imported_symbols(array, syms)
  LVAL array, syms;
{
  char *name;
  LVAL sym, symlist;
  int hval;

  for (; syms; syms = cdr(syms)) {
    sym = car(syms);
    name = getstring(getpname(sym));
    hval = hash(name, HSIZE);
    if (findsym(sym, getelement(array,hval)) == NIL) {
      symlist = cons(sym, getelement(array,hval));
      setelement(array, hval, symlist);
    }
  }

}

static void check_symlist(symlist)
  LVAL symlist;
{
  for (; symlist; symlist = cdr(symlist))
    if (!symbolp(car(symlist)))
      xlerror("not a symbol in only/except list", car(symlist));
}

static int same_name(a, b)
  LVAL a, b;
{
  return (strcmp(getstring(getpname(a)), getstring(getpname(b))) == 0);
}

static LVAL filter_all(modname, sofar)
  LVAL modname, sofar;
{
  LVAL module, syms;

  cpush(sofar);
  module = find_or_load_module(modname);
  drop(1);

  if (module == NIL)
    xlerror("no such module in defmodule", modname);

  for (syms = getmexports(module); syms; syms = cdr(syms))
    sofar = cons(car(syms), sofar);

  return sofar;
}

static LVAL filter_only(symlist, implist, sofar)
  LVAL symlist, implist, sofar;
{
  LVAL syms, sym;

  check_symlist(symlist);

  cpush(sofar);
  syms = filter_imports(implist, NIL);
  drop(1);

  push(syms);

  for (; symlist; symlist = cdr(symlist)) {
    sym = xlmember(car(symlist), syms, same_name);
    if (sym != NIL) sofar = cons(car(sym), sofar);
  }

  drop(1);

  return sofar;
}

static LVAL filter_except(symlist, implist, sofar)
  LVAL symlist, implist, sofar;
{
  LVAL syms;

  check_symlist(symlist);

  cpush(sofar);
  syms = filter_imports(implist, NIL);
  drop(1);

  push(syms);

  for (; syms; syms = cdr(syms))
    if (xlmember(car(syms), symlist, same_name) == NIL)
      sofar = cons(car(syms), sofar);

  drop(1);

  return sofar;
}

static void check_rename_list(renamelist)
  LVAL renamelist;
{
  LVAL list, rename, old, new;

  if (!listp(renamelist))
    xlerror("malformed rename directive in defmodule", renamelist);

  for (list = renamelist; list; list = cdr(list)) {
    rename = car(list);

    if (!consp(rename) || !consp(cdr(rename)))
      xlerror("malformed rename pair in defmodule", rename);

    if (!symbolp(car(rename)))
      xlerror("not a symbol in rename in defmodule", car(rename));

    if (!symbolp(car(cdr(rename))))
      xlerror("not a symbol in rename in defmodule", car(cdr(rename)));
  }

  /* check for repeats
   * (rename ((+ add) (- add)) ...) bad
   * (rename ((+ plus) (+ add)) ...) seems OK
   */
  for (; renamelist; renamelist = cdr(renamelist)) {
    rename = car(renamelist);
    old = car(rename);
    new = car(cdr(rename));
    for (list = cdr(renamelist); list; list = cdr(list)) {
      rename = car(list);
      if ((new == car(cdr(rename))) && (old != car(rename)))
	xlerror("repeat in rename in defmodule", rename);
    }
  }
}

/* import all symbols, flag some as renamed */
static LVAL filter_rename(renamelist, implist, sofar)
  LVAL renamelist, implist, sofar;
{
  LVAL syms, rename, oldname, newname, found, except;
  extern LVAL s_rename_flag;

  check(3);
  push(sofar);
  syms = filter_imports(implist, NIL);
  push(syms);

  check_rename_list(renamelist);

  except = NIL;

  for (; renamelist; renamelist = cdr(renamelist)) {
    rename = car(renamelist);

    oldname = car(rename);
    newname = car(cdr(rename));

    /* renaming to self? */
    if (oldname == newname) continue;

    found = xlmember(oldname, syms, same_name);
    if (found == NIL)
      xlerror("no such imported symbol to rename in defmodule", oldname);

    oldname = car(found);

    /* don't import this one */
    except = cons(oldname, except);

    /* run down the renaming chain */
    while ((rename = xlgetsyntax(oldname, s_rename_flag)) != NIL) {
      oldname = rename;
    }
    push(except);
    newname = reintern_symbol(newname);
    xlputsyntax(newname, oldname, s_rename_flag);
    drop(1);
  }

  push(except);

  for (; syms; syms = cdr(syms))
    if (xlmember(car(syms), except, same_name) == NIL)
      sofar = cons(car(syms), sofar);

  drop(3);

  return sofar;
}

static LVAL filter_imports(implist, sofar)
  LVAL implist, sofar;
{
  LVAL imp;

  for (; implist; implist = cdr(implist)) {

    imp = car(implist);

    if (symbolp(imp)) {
      sofar = filter_all(imp, sofar);
    } else if (!consp(imp)
	       || !symbolp(car(imp))
	       || !consp(cdr(imp))
	       || !listp(car(cdr(imp)))) {
      xlerror("malformed import directive in defmodule", car(implist));
    }
    else if (car(imp) == s_only) {
      sofar = filter_only(car(cdr(imp)), cdr(cdr(imp)), sofar);
    }
    else if (car(imp) == s_except) {
      sofar = filter_except(car(cdr(imp)), cdr(cdr(imp)), sofar);
    }
    else if (car(imp) == s_rename) {
      sofar = filter_rename(car(cdr(imp)), cdr(cdr(imp)), sofar);
    }
    else
      xlerror("bad import directive in defmodule", car(imp));
  }

  return sofar;
}

static void process_import_directive(array, implist)
  LVAL array, implist;
{
  LVAL symlist;

  symlist = filter_imports(implist, NIL);
  cpush(symlist);

  add_imported_symbols(array, symlist);

  drop(1);
}

static void process_module_directives(array, directives)
  LVAL array, directives;
{
  LVAL directive, value;

  check(2);
  push(array);
  push(directives);

  while (directives) {
    directive = car(directives);
    if (!symbolp(directive))
      xlerror("malformed directive in defmodule", directive);

    directives = cdr(directives);
    if (!consp(directives))
      xlerror("missing value for directive", directive);

    value = car(directives);

    if (directive == s_import)
      process_import_directive(array, value);
    else {
      LVAL out;
      out = getvalue(s_stderr);
      xlputstr(out, "*** ignoring directive \"");
      xlprin1(directive, out);
      xlputstr(out, "\" in defmodule\n");
    }

    directives = cdr(directives);
  }

  drop(2);
}

/* intern symbol in current module */
/* can't use symbol name directly as it might move during a GC in xlenter */
static LVAL reintern_symbol(sym)
  LVAL sym;
{
  char buf[STRMAX+1];

  if (keywordp(sym) || getmodule(sym) == reintern_module) return sym;
  strcpy(buf, getstring(getpname(sym)));
  return xlenter(buf);
}

/* make all symbols in a vector be in current module */
static void reintern_vector_symbols(form)
  LVAL form;
{
  int i, len;
  LVAL elt;

  len = getsize(form);
  for (i = 0; i < len; i++) {
    elt = getelement(form,i);
    if (symbolp(elt)) {
      setelement(form,i,reintern_symbol(elt));
    }
    else
      reintern_module_symbols(elt);
  }
}

/* make all symbols in a defmodule body be in current module */
/* structures: lists & vectors only */
static void reintern_module_symbols(body)
  LVAL body;
{
  LVAL form;

  for (; consp(body); body = cdr(body)) {
    form = car(body);
    if (symbolp(form))
      rplaca(body, reintern_symbol(form));
    else if (consp(form))
      reintern_module_symbols(form);
    else if (vectorp(form))
      reintern_vector_symbols(form);
    if (symbolp(cdr(body)))	/* (a b . c) */
      rplacd(body, reintern_symbol(cdr(body)));
  }
  if (vectorp(body))		/* (a b . #(c d)) */
    reintern_vector_symbols(body);
}

LVAL xreintern()
{
  static char *cfn_name = "reintern";
  LVAL body;

  body = xlgalist();
  xllastarg();

  reintern_module_symbols(cdr(body)); /* (begin@ROOT .... ) */

  return true;
}

#if 1
LVAL xreintern_syntax()
{
  static char *cfn_name = "reintern-syntax";
  LVAL sym;

  sym = xlgasymbol();
  xllastarg();

  return reintern_symbol(sym);
}
#else
LVAL reintern_syntax_form(), reintern_syntax_vector();

LVAL reintern_syntax_form(form)
LVAL form;
{
  if (symbolp(form))
    return reintern_symbol(form);
  else if (consp(form))
    return cons(car(form), reintern_syntax_form(cdr(form)));
  else if (vectorp(form))
    return reintern_syntax_vector(form);
  else
    return form;
}

LVAL reintern_syntax_vector(form)
LVAL form;
{
  int i, len;
  LVAL new, elt;

  len = getsize(form);
  new = newvector(len);
  cpush(new);
  for (i = 0; i < len; i++)
    setelement(new, i, reintern_syntax_form(getelement(form, i)));
  drop(1);
  return new;
}

LVAL xreintern_syntax()
{
  static char *cfn_name = "reintern-syntax";
  LVAL form;

  form = xlgetarg();
  xllastarg();

  cpush(form);

  if (symbolp(form) || consp(form) || vectorp(form))
    form = reintern_syntax_form(form);

  drop(1);
  return form;
}
#endif

LVAL xmodule_directives()
{
  static char *cfn_name = "module-directives";
  LVAL array, form;

  array = xlgavector();
  form = xlgalist();
  xllastarg();

  process_module_directives(array, form);

  return true;
}

/* load those modules that this one depends on */
/* implist is a list of module descriptors (i.e., names or filters) */
static void load_dependent_modules2(implist)
  LVAL implist;
{
  LVAL imp, module;

  for (; implist; implist = cdr(implist)) {
    imp = car(implist);
    if (symbolp(imp)) {
      module = find_or_load_module(imp);
      if (module == NIL)
	xlerror("no such module in defmodule", imp);
    }
    else if (!consp(imp)
	     || !symbolp(car(imp))
	     || !consp(cdr(imp))
	     || !listp(car(cdr(imp)))) {
      xlerror("malformed import directive in defmodule", imp);
    }
    else if (car(imp) == s_only
	     || car(imp) == s_except
	     || car(imp) == s_rename) {
      load_dependent_modules2(cdr(cdr(imp)));
    }
    else
      xlerror("bad import directive in defmodule", car(imp));	  
  }
}

static void load_dependent_modules(directives)
  LVAL directives;
{
  LVAL directive;

  while (directives) {
    directive = car(directives);
    if (!symbolp(directive))
      xlerror("malformed directive in defmodule", directive);

    directives = cdr(directives);
    if (!consp(directives))
      xlerror("missing value for directive", directive);

    if (directive == s_import)
      load_dependent_modules2(car(directives));
    directives = cdr(directives);
  }
}

/* compile a defmodule
   (defmodule foo (import a ..) body ...) ->
   (load-dependent-modules (import a ..))
   (set-module foo)
   (module-directives (import ...) current-module-obarray)
   (reintern (begin body ...))
   ((compile (begin body ...)))     ; compile and run
   (set-module root)
*/
static void do_defmodule(form,cont)
  LVAL form; int cont;
{
  char modname[STRMAX];
  LVAL newmod, array, body, expr;
#ifdef NOISY_LOAD
  char buf[128];
  LVAL s_display;
  extern int quiet;
#endif

  if (current_module != root_module)
    xlerror("only use defmodule in root module", form);

  if (atom(form))
    xlerror("expecting module name in defmodule", form);

  if (!symbolp(car(form)) && !stringp(car(form)))
    xlerror("expecting module name in defmodule", form);

  /* copy as strings can move in GC */
  if (symbolp(car(form)))
    strcpy(modname, getstring(getpname(car(form))));
  else
    strcpy(modname, getstring(car(form)));

  if (atom(cdr(form)) || !listp(car(cdr(form))))
    xlerror("expecting module import list in defmodule", form);

  check(3);
  push(form);
  newmod = cvmodule(modname);
  push(newmod);

  array = getmsymbols(newmod);

  load_dependent_modules(car(cdr(form)));

  expr = cons(s_setmodule, cons(newmod, NIL));
  do_expr(expr, C_NEXT);

  array = getmsymbols(newmod);
  expr = cons(s_module_directives,
	      cons(array,
		   cons(cons(s_quote, cons(car(cdr(form)), NIL)), NIL)));
  do_expr(expr, C_NEXT);

  body = cdr(cdr(form));
  body = cons(s_begin, body);
  body = cons(s_quote, cons(body, NIL));
  body = cons(body, NIL);
  push(body);
  expr = cons(s_reintern, body);
  do_expr(expr, C_NEXT);
  drop(1);

#ifdef NOISY_LOAD
  if (!quiet) {
    push(body);
    sprintf(buf, "<%s...", modname);
    s_display = xlenter_module("display", root_module);
    expr = cons(s_display, cons(cvstring(buf), NIL));
    do_expr(expr, C_NEXT);
    drop(1);
  }
#endif

  body = cons(s_compile, body);
  body = cons(body, NIL);	/* call the compiled expression */
  do_expr(body, C_NEXT);

  expr = cons(s_setmodule, cons(root_module, NIL));
  do_expr(expr, C_NEXT);

  module_list = cons(newmod, module_list);

#ifdef NOISY_LOAD
  if (!quiet) {
    sprintf(buf, "done>\n");
    expr = cons(s_display, cons(cvstring(buf), NIL));
    do_expr(expr, C_NEXT);
  }
#endif

  if (symbolp(car(form)))
    do_literal(getpname(car(form)),cont);
  else
    do_literal(car(form),cont);

  current_module = root_module;
#ifdef TRACE_SETMODULE
xlputstr(curoutput(), "<5curmod=");xlprin1(current_module,curoutput());
xlputstr(curoutput(), ">");
#endif

  drop(2);
}

static void do_export(form,cont)
  LVAL form; int cont;
{
  LVAL sym, syms, exports;

  exports = getmexports(current_module);
  cpush(form);

  for (syms = form; syms; syms = cdr(syms)) {
    sym = car(syms);
    if (!symbolp(sym))
      xlerror("non-symbol in export", sym);
    else
      exports = cons(sym, exports);
  }

  setmexports(current_module, exports);

  drop(1);

  putcbyte(OP_T);
  do_continuation(cont);
}

LVAL append(a,b)
  LVAL a, b;
{
  if (a == NIL) return b;
  return cons(car(a),append(cdr(a),b));
}

static void do_expose(form,cont)
  LVAL form; int cont;
{
  LVAL sym, syms, exports, mod;

  exports = getmexports(current_module);
  cpush(form);

  for (syms = form; syms; syms = cdr(syms)) {
    sym = car(syms);
    if (!symbolp(sym) && !stringp(sym))
      xlerror("bad module name in expose", sym);
    else {
      mod = find_or_load_module(sym);
      if (mod == NIL) xlerror("no such module in expose", sym);
      cpush(exports);
      exports = append(getmexports(mod), exports);
      drop(1);
    }
  }

  setmexports(current_module, exports);

  putcbyte(OP_T);
  do_continuation(cont);

  drop(1);
}

static void do_enter_module(form,cont)
  LVAL form; int cont;
{
  LVAL sym, mod;

  if (atom(form))
    xlfail("module name expected in enter-module", s_syntax_error);

  sym = car(form);
  if (!symbolp(sym) && !stringp(sym))
    xlerror("bad module name in enter-module", sym);

  cpush(form);

  mod = find_or_load_module(sym);
  if (mod == NIL)
    xlerror("unknown module in enter-module", sym);

  do_expr(cons(s_setmodule, cons(mod, NIL)), C_NEXT);

  putcbyte(OP_T);
  cont = C_RETURN;
  do_continuation(cont);

  drop(1);
}

static void do_reenter_module(form,cont)
  LVAL form; int cont;
{
  LVAL sym, mod;
  int loaded;

  if (atom(form))
    xlfail("module name expected in reenter-module", s_syntax_error);

  sym = car(form);
  if (!symbolp(sym) && !stringp(sym))
    xlerror("bad module name in reenter-module", sym);

  cpush(form);

  loaded = load_module(sym);

  mod = find_module(sym);
  if (mod == NIL)
    xlerror("unknown module in reenter-module", sym);

  if (!loaded)
    xlerror("can't find module in reenter-module", sym);

  do_expr(cons(s_setmodule, cons(mod, NIL)), C_NEXT);

  putcbyte(OP_T);
  cont = C_RETURN;
  do_continuation(cont);

  drop(1);
}

/* an extra for those who can't wait */
static void do_import(form, cont)
  LVAL form; int cont;
{
  LVAL mod, array;
  int nxt;

  cpush(form);

  mod = find_or_load_module(car(cdr(form)));
  if (mod == NIL)
    xlerror("unknown module in import", car(cdr(form)));

  array = getmsymbols(current_module);

  putcbyte(OP_SAVE);
  nxt = putcword(0);
  do_expr(car(form), C_NEXT);
  putcbyte(OP_PUSH);
  cd_variable(OP_GREF, xlenter_module("set-module", root_module));
  putcbyte(OP_CALL);
  putcbyte(1);
  fixup(nxt);

  form = cdr(form);
  if (stringp(car(form)))
    rplaca(form, xlenter_module(getstring(car(form)), root_module));
  form = cons(form, NIL);
  form = cons(s_import, form);
  drop(1);
  push(form);

  putcbyte(OP_SAVE);
  nxt = putcword(0);
  do_literal(form, C_NEXT);
  putcbyte(OP_PUSH);
  do_literal(array, C_NEXT);
  putcbyte(OP_PUSH);
  cd_variable(OP_GREF, s_module_directives);
  putcbyte(OP_CALL);
  putcbyte(2);
  fixup(nxt);

  do_literal(getmexports(mod),cont);

  drop(1);
}

static void genargs(gf, args)
  LVAL gf, args;
{
  extern LVAL object;
  LVAL sym;
  int lev, off;

  if (!consp(args)) {
    /* mark if optional args allowed */
    setgopt(gf, args == NIL ? NIL : true);
    return;
  }

  genargs(gf, cdr(args));

  putcbyte(OP_PUSH);

  if (symbolp(car(args)))
    do_literal(object, C_NEXT);
  else if (consp(car(args)) && symbolp(car(car(args))) &&
      consp(cdr(car(args))) && symbolp(car(cdr(car(args))))) {
    sym = car(cdr(car(args)));
    if (findvariable(sym, &lev, &off))
      cd_evariable(OP_EREF,lev,off);
    else
      cd_variable(OP_GREF,sym);
  }
  else
    xlerror("bad argument for defgeneric", car(args));

  putcbyte(OP_CONS);
}

/* get classes of required args for gf */
static void do_set_genargs(gf, args)
  LVAL gf, args;
{
  extern LVAL s_set_generic_args;
  int nxt;

  putcbyte(OP_SAVE);
  nxt = putcword(0);
  putcbyte(OP_NIL);
  genargs(gf, args);
  putcbyte(OP_PUSH);
  do_literal(gf, C_NEXT);
  putcbyte(OP_PUSH);
  cd_variable(OP_GREF, s_set_generic_args);
  putcbyte(OP_CALL);
  putcbyte(2);
  fixup(nxt);
}

static void do_define_generic(form, cont)
  LVAL form; int cont;
{
  LVAL name, args, gf;
  int off;

  if (atom(form))
    xlfail("missing body in defgeneric", s_syntax_error);

  if (!consp(car(form)))
    xlerror("bad name/args in defgeneric", car(form));

  name = car(car(form));
  if (!symbolp(name))
    xlerror("bad name in defgeneric", name);

  args = cdr(car(form));
  if (!listp(args))
    xlerror("bad arglist in defgeneric", car(form));
  if (!consp(args))
    xlerror("must have at least one required arg in defgeneric", car(form));

  gf = newgeneric();
  cpush(gf);

  setgname(gf, name);
  setgargs(gf, NIL);
  setgopt(gf, NIL);
  setgmethods(gf, NIL);
  setgcache1(gf, NIL);
  setgcache2(gf, NIL);

  do_literal(gf, C_NEXT);
  if (findcvariable(name, &off))
    cd_evariable(OP_ESET,0,off);
  else
    cd_variable(OP_GSET,name);

  do_set_genargs(gf, args);

  drop(1);
  do_literal(name,cont);
}

static LVAL define_method_args(arglist)
  LVAL arglist;
{
  LVAL arg, args, tail;

  if (!consp(arglist)) return arglist;

  args = NIL;
  tail = NIL;

  check(1);

  for (; consp(arglist); arglist = cdr(arglist)) {
    arg = car(arglist);
    if (consp(arg)) arg = car(arg);
    if (!symbolp(arg)) 
      xlerror("argument must be a symbol in defmethod", car(arglist));
    if (args) {
      rplacd(tail, cons(arg, NIL));
      tail = cdr(tail);
    }
    else {
      tail = args = cons(arg, NIL);
      push(args);
    }
  }

  if (symbolp(arglist))		/* optional args */
    rplacd(tail, arglist);
  else if (arglist != NIL)
    xlerror("rest argument must be a symbol in defmethod", arglist);

  drop(1);

  return args;
}

/* the required args classes */
static LVAL define_method_classes(arglist)
  LVAL arglist;
{
  LVAL arg, classes, tail, s_object_class;

  if (!consp(arglist)) return NIL;

  s_object_class = xlenter_module("<object>", root_module);
  classes = NIL;
  tail = NIL;

  check(1);

  for (; consp(arglist); arglist = cdr(arglist)) {
    arg = car(arglist);
    if (consp(arg)) {
      arg = cdr(arg);
      if (!consp(arg)) xlerror("malformed argument in defmethod",
			       car(arglist));
      arg = car(arg);
      if (!symbolp(arg)) 
	xlerror("expecting a class name in defmethod", car(arglist));
    }
    else if (!symbolp(arg))
      xlerror("malformed argument in defmethod", car(arglist));
    else
      arg = s_object_class;

    if (classes) {
      rplacd(tail, cons(arg, NIL));
      tail = cdr(tail);
    }
    else {
      tail = classes = cons(arg, NIL);
      push(classes);
    }
  }

  drop(1);

  return classes;
}

/* cons up the method domain */
static int push_method_domain(classes)
  LVAL classes;
{
  int len, lev, off;

  if (classes == NIL) return 0;

  len = push_method_domain(cdr(classes));

  if (findvariable(car(classes), &lev, &off))
    cd_evariable(OP_EREF, lev, off);
  else
    cd_variable(OP_GREF, car(classes));
  putcbyte(OP_PUSH);

  return len + 1;
}

static void do_define_method(form, cont)
  LVAL form; int cont;
{
  LVAL name, arglist, args, classes;
  LVAL body, sym, opts;
  int nxt1, nxt2, len;
  extern LVAL s_arg_list, s_next_methods;

  if (atom(form))
    xlfail("missing body in defmethod", s_syntax_error);

  if (!consp(car(form)))
    xlerror("bad name/args in defmethod", car(form));

  name = car(car(form));
  if (!(symbolp(name) ||
	(consp(name) && symbolp(car(name)))))
    xlerror("bad name in defmethod", name);

  arglist = cdr(car(form));
  if (!listp(arglist))
    xlerror("bad arglist in defmethod", car(form));
  if (!consp(arglist))
    xlerror("must have at least one required arg in defmethod", car(form));

  cpush(form);
  classes = define_method_classes(arglist);
  cpush(classes);
  args = define_method_args(arglist);
  cpush(args);

  drop(1);
  args = cons(s_arg_list, args);
  args = cons(s_next_methods, args);
  cpush(args);

  putcbyte(OP_SAVE);		/* add-method */
  nxt1 = putcword(0);

  /* optional args? */
  for (opts = args; consp(opts); opts = cdr(opts));
  if (opts == NIL)
    putcbyte(OP_NIL);
  else
    putcbyte(OP_T);
  putcbyte(OP_PUSH);

  putcbyte(OP_SAVE);		/* list */
  nxt2 = putcword(0);

  len = push_method_domain(classes);

  cd_variable(OP_GREF, s_list);
  putcbyte(OP_CALL);
  putcbyte(len);
  fixup(nxt2);
  putcbyte(OP_PUSH);

  body = cdr(form);
  cd_fundefinition(name, args, body); /* the method function */
  putcbyte(OP_PUSH);

  do_expr(name, C_NEXT);	/* the generic */

  putcbyte(OP_PUSH);

  sym = xlenter_module("make-and-add-method", root_module);
  cd_variable(OP_GREF, sym);
  
  putcbyte(OP_CALL);
  putcbyte(4);			/* gf, closure, domain, optargs? */
  fixup(nxt1);

  do_literal(name,cont);

  drop(3);
  return;

}

static void do_cnm(form, cont)
  LVAL form; int cont;
{
  extern LVAL s_next_methods, s_arg_list;
  int nxt, lev, off;

  if (form != NIL)
    xlerror("extra forms in call-next-method", form);

  if (cont != C_RETURN) {
    putcbyte(OP_SAVE);
    nxt = putcword(0);
  }

  if (findvariable(s_arg_list, &lev, &off)) /* arg list */
    cd_evariable(OP_EREF, lev, off);
  else
    xlfail("call-next-method outside of a method", s_syntax_error);

  putcbyte(OP_PUSH);

  if (findvariable(s_next_methods, &lev, &off)) /* method list */
    cd_evariable(OP_EREF, lev, off);
  else
    xlfail("call-next-method outside of a method", s_syntax_error);

  putcbyte(OP_CNM);

  if (cont != C_RETURN)
    fixup(nxt);

  do_continuation(cont);
  
}

static void do_next_method_p(form, cont)
  LVAL form; int cont;
{
  extern LVAL s_next_methods;
  int lev, off;

  if (form != NIL)
    xlerror("extra forms in next-method?", form);

  if (findvariable(s_next_methods, &lev, &off)) /* arg list */
    cd_evariable(OP_EREF, lev, off);
  else
    xlfail("next-method? called outside of a method", s_syntax_error);

  putcbyte(OP_NULL);
  putcbyte(OP_NULL);

  do_continuation(cont);

}

static int do_superclass(super)
  LVAL super;
{
  int lev, off;

  if (super == NIL)
    return 0;

  putcbyte(OP_NIL);
  putcbyte(OP_PUSH);

  if (findvariable(car(super), &lev, &off))
    cd_evariable(OP_EREF, lev, off);
  else
    cd_variable(OP_GREF, car(super));

  putcbyte(OP_CONS);
  putcbyte(OP_PUSH);

  do_literal(s_superclasses, C_NEXT);
  putcbyte(OP_PUSH);

  return 2;

}

static int do_abstractp(classopts)
  LVAL classopts;
{
  for (; classopts; classopts = cdr(cdr(classopts)))
    if (car(classopts) == s_abstractp) {
      do_literal(car(cdr(classopts)), C_NEXT);
      putcbyte(OP_PUSH);
      do_literal(s_abstractp, C_NEXT);
      putcbyte(OP_PUSH);
      return 2;
    }

  return 0;
}

static int do_slots(slots)
  LVAL slots;
{
  int nargs1, nargs2, nxt1, nxt2;
  LVAL slot, slotname, key, defn, reqd;

  if (slots == NIL) return 0;

  putcbyte(OP_SAVE);
  nxt1 = putcword(0);

  slots = xlreverse(slots);
  cpush(slots);

  for (nargs1 = 0; slots; slots = cdr(slots), nargs1++) {
    slot = car(slots);

    if (slot == NIL)
      xlfail("missing slot name in defclass", s_syntax_error);

    nargs2 = 0;
    putcbyte(OP_SAVE);
    nxt2 = putcword(0);
      
    if (symbolp(slot)) {
      do_literal(slot, C_NEXT);
      putcbyte(OP_PUSH);
      nargs2++;
      do_literal(s_name, C_NEXT);
      putcbyte(OP_PUSH);
      nargs2++;
    }
    else {
      /* slot name */
      slotname = car(slot);
      do_literal(slotname, C_NEXT);
      putcbyte(OP_PUSH);
      nargs2++;
      do_literal(s_name, C_NEXT);
      putcbyte(OP_PUSH);
      nargs2++;

      slot = cdr(slot);

      /* slot keyword */
      key = find_key(s_keyword, slot, s_unbound);
      if (key != s_unbound) {
	do_literal(key, C_NEXT);
	putcbyte(OP_PUSH);
	nargs2++;
	do_literal(s_keyword, C_NEXT);
	putcbyte(OP_PUSH);
	nargs2++;
      }

      /* slot default */
      defn = find_key(s_default, slot, s_unbound);
      if (defn != s_unbound) {
	defn = cons(defn, NIL);
	cd_fundefinition(slotname, NIL, defn);
	putcbyte(OP_PUSH);
	nargs2++;
	do_literal(s_default, C_NEXT);
	putcbyte(OP_PUSH);
	nargs2++;
      }

      /* slot requiredp */
      reqd = find_key(s_requiredp, slot, s_unbound);
      if (reqd != s_unbound) {
	do_literal(reqd, C_NEXT);
	putcbyte(OP_PUSH);
	nargs2++;
	do_literal(s_requiredp, C_NEXT);
	putcbyte(OP_PUSH);
	nargs2++;
      }
    }

    cd_variable(OP_GREF, s_list);
    putcbyte(OP_CALL);
    putcbyte(nargs2);
    fixup(nxt2);
    putcbyte(OP_PUSH);
  }

  cd_variable(OP_GREF, s_list);
  putcbyte(OP_CALL);
  putcbyte(nargs1);
  fixup(nxt1);
  putcbyte(OP_PUSH);
  do_literal(s_direct_slots, C_NEXT);
  putcbyte(OP_PUSH);

  drop(1);

  return 2;
}

static int do_keywords(slots, classopts)
  LVAL slots, classopts;
{
  LVAL keys, slot, val, kwd;
  int nargs;

  nargs = 0;

  check(1);

  keys = NIL;

  /* slot keywords */
  for (; slots; slots = cdr(slots)) {
    slot = car(slots);
    if (consp(slot)) {
      val = find_key(s_keyword, cdr(slot), s_unbound);
      if (val != s_unbound)
	keys = cons(val, keys);
    }
  }

  /* class keywords */
  for (; classopts; classopts = cdr(cdr(classopts))) {
    kwd = car(classopts);
    if (kwd == s_keywords) {
      if (!listp(car(cdr(classopts))))
        xlerror("bad keyword list in defclass", car(cdr(classopts)));
      push(keys);
      keys = append(car(cdr(classopts)), keys);
      drop(1);
    }
    else if (kwd != s_constructor &&
	     kwd != s_predicate &&
	     kwd != s_class &&
	     kwd != s_abstractp) {
      do_expr(car(cdr(classopts)), C_NEXT);
      putcbyte(OP_PUSH);
      do_literal(car(classopts), C_NEXT);
      putcbyte(OP_PUSH);
      nargs += 2;
    }
  }

  if (keys != NIL) {
    do_literal(keys, C_NEXT);
    putcbyte(OP_PUSH);
    do_literal(s_direct_keywords, C_NEXT);
    putcbyte(OP_PUSH);
    nargs += 2;
  }

  return nargs;

}

static LVAL mkarg(n)
  int n;
{
  char buf[128];

  sprintf(buf, "arg%d", n);
  return xlenter(buf);
}

static void do_general_constructor(classname, name)
  LVAL classname, name;
{
  LVAL args, body;
  int lev, off;

  if (!symbolp(name))
    xlerror("bad name for constructor in defclass", name);

  args = mkarg(0);
  body = cons(args, NIL);
  body = cons(classname, body);
  body = cons(s_make, body);
  body = cons(s_apply, body);	/* (apply make foo args) */
  body = cons(body, NIL);
  cpush(body);
  cd_fundefinition(name, args, body);
  drop(1);
  if (findvariable(name, &lev, &off))
    cd_evariable(OP_ESET, lev, off);
  else
    cd_variable(OP_GSET, name);
}

static void do_constructor(classname, classopts)
  LVAL classname, classopts;
{
  LVAL name, keys, args, body, arg;
  int count, lev, off;

  for (; classopts; classopts = cdr(cdr(classopts))) {

    if (car(classopts) == s_constructor) {

      name = car(cdr(classopts));
      if (consp(name)) {
	keys = cdr(name);
	name = car(name);
      }
      else {
	do_general_constructor(classname, name);
	return;
      }

      if (!symbolp(name))
	xlerror("bad name for constructor in defclass", name);
      
      args = NIL;
      body = NIL;
      check(3);
      keys = xlreverse(keys);
      push(keys);

      for (count = 0; keys; keys = cdr(keys), count++) {
	push(args); push(body);
	arg = mkarg(count);
	drop(2);
	args = cons(arg, args);
	push(args);
	body = cons(arg, body);
	body = cons(car(keys), body);
	drop(1);
      }
      drop(1);
      push(args);
      body = cons(classname, body);
      body = cons(s_make, body); /* (make cl a: arg1 b: arg2) */
      body = cons(body, NIL);
      push(body);
      cd_fundefinition(name, args, body);
      drop(2);
      if (findvariable(name, &lev, &off))
        cd_evariable(OP_ESET, lev, off);
      else
        cd_variable(OP_GSET, name);
    }
  }
}

static void do_predicate(classname, classopts)
  LVAL classname, classopts;
{
  LVAL s_class_of, s_subclassp;
  LVAL name, args, body;
  int lev, off, previous;

  s_class_of = xlenter_module("class-of", root_module);
  s_subclassp = xlenter_module("subclass?", root_module);

  check(2);

  previous = FALSE;

  for (; classopts; classopts = cdr(cdr(classopts))) {

    if (car(classopts) == s_predicate) {

      name = car(cdr(classopts));
      if (!symbolp(name))
	xlerror("bad name for predicate in defclass", name);

      if (!previous) {		/* make predicate function */
	args = cons(s_object, NIL);      
	push(args);

	body = cons(classname, NIL);
	push(body);
	body = cons(s_object, NIL);
	body = cons(s_class_of, body);
	body = cons(body, pop());
	body = cons(s_subclassp, body); /* (subclass? (class-of object) cl) */
	body = cons(body, NIL);
	push(body);

	cd_fundefinition(name, args, body);

	drop(2);
	previous = TRUE;
      }

      if (findvariable(name, &lev, &off))
	cd_evariable(OP_ESET, lev, off);
      else
	cd_variable(OP_GSET, name);
    }
  }
}

static void do_named_reader(readername, slotname, classname)
  LVAL readername, slotname, classname;
{
  int lev, off, nxt1, nxt2, nxt3, nxt4;

  putcbyte(OP_SAVE);
  nxt1 = putcword(0);
  putcbyte(OP_SAVE);
  nxt2 = putcword(0);
  putcbyte(OP_SAVE);
  nxt3 = putcword(0);
  putcbyte(OP_SAVE);
  nxt4 = putcword(0);
  if (findvariable(classname, &lev, &off))
    cd_evariable(OP_EREF, lev, off);
  else
    cd_variable(OP_GREF, classname);
  putcbyte(OP_PUSH);
  do_literal(slotname, C_NEXT);
  putcbyte(OP_PUSH);
  cd_variable(OP_GREF, s_find_slot_index); /* finds index of slot in class */
  putcbyte(OP_CALL);
  putcbyte(2);
  fixup(nxt4);
  putcbyte(OP_PUSH);
  do_literal(s_object, C_NEXT);
  putcbyte(OP_PUSH);
  do_literal(s_getivar, C_NEXT);
  putcbyte(OP_PUSH);
  cd_variable(OP_GREF, s_list);	/* (%getivar object indx) */
  putcbyte(OP_CALL);
  putcbyte(3);
  fixup(nxt3);
  putcbyte(OP_PUSH);
#ifndef NO_CHECK_REF
  putcbyte(OP_SAVE);
  nxt3 = putcword(0);
  do_literal(s_object, C_NEXT);
  putcbyte(OP_PUSH);
  if (findvariable(classname, &lev, &off))
    cd_evariable(OP_EREF, lev, off);
  else
    cd_variable(OP_GREF, classname);
  putcbyte(OP_PUSH);
  do_literal(s_check_ref, C_NEXT);
  putcbyte(OP_PUSH);
  cd_variable(OP_GREF, s_list);	/* (check-ref class object) */
  putcbyte(OP_CALL);
  putcbyte(3);
  fixup(nxt3);
  putcbyte(OP_PUSH);
#endif
  putcbyte(OP_NIL);
  putcbyte(OP_PUSH);
  do_literal(s_object, C_NEXT);
  putcbyte(OP_CONS);
  putcbyte(OP_PUSH);
  do_literal(s_lambda, C_NEXT);
  putcbyte(OP_PUSH);
  cd_variable(OP_GREF, s_list); /* (lambda (object) (check-ref class object) */
  putcbyte(OP_CALL);		/*                  (%getivar object indx)) */
#ifndef NO_CHECK_REF
  putcbyte(4);
#else
  putcbyte(3);
#endif
  fixup(nxt2);
  putcbyte(OP_PUSH);
  cd_variable(OP_GREF, s_eval);
  putcbyte(OP_CALL);
  putcbyte(1);
  fixup(nxt1);
  if (findvariable(readername, &lev, &off))
    cd_evariable(OP_ESET, lev, off);
  else
    cd_variable(OP_GSET, readername);
}

static void do_named_writer(writername, slotname, classname, setterp)
  LVAL writername, slotname, classname;
  int setterp;
{
  int lev, off, nxt1, nxt2, nxt3, nxt4;

  putcbyte(OP_SAVE);
  nxt1 = putcword(0);
  putcbyte(OP_SAVE);
  nxt2 = putcword(0);
  putcbyte(OP_SAVE);
  nxt3 = putcword(0);
  do_literal(s_value, C_NEXT);
  putcbyte(OP_PUSH);
  putcbyte(OP_SAVE);
  nxt4 = putcword(0);
  if (findvariable(classname, &lev, &off))
    cd_evariable(OP_EREF, lev, off);
  else
    cd_variable(OP_GREF, classname);
  putcbyte(OP_PUSH);
  do_literal(slotname, C_NEXT);
  putcbyte(OP_PUSH);
  cd_variable(OP_GREF, s_find_slot_index); /* finds index of slot in class */
  putcbyte(OP_CALL);
  putcbyte(2);
  fixup(nxt4);
  putcbyte(OP_PUSH);
  do_literal(s_object, C_NEXT);
  putcbyte(OP_PUSH);
  do_literal(s_setivar, C_NEXT);
  putcbyte(OP_PUSH);
  cd_variable(OP_GREF, s_list);	/* (%setivar object indx val) */
  putcbyte(OP_CALL);
  putcbyte(4);
  fixup(nxt3);
  putcbyte(OP_PUSH);
#ifndef NO_CHECK_REF
  putcbyte(OP_SAVE);
  nxt3 = putcword(0);
  do_literal(s_object, C_NEXT);
  putcbyte(OP_PUSH);
  if (findvariable(classname, &lev, &off))
    cd_evariable(OP_EREF, lev, off);
  else
    cd_variable(OP_GREF, classname);
  putcbyte(OP_PUSH);
  do_literal(s_check_ref, C_NEXT);
  putcbyte(OP_PUSH);
  cd_variable(OP_GREF, s_list);	/* (check-ref class object) */
  putcbyte(OP_CALL);
  putcbyte(3);
  fixup(nxt3);
  putcbyte(OP_PUSH);
#endif
  putcbyte(OP_NIL);
  putcbyte(OP_PUSH);
  do_literal(s_value, C_NEXT);
  putcbyte(OP_CONS);
  putcbyte(OP_PUSH);
  do_literal(s_object, C_NEXT);
  putcbyte(OP_CONS);
  putcbyte(OP_PUSH);
  do_literal(s_lambda, C_NEXT);
  putcbyte(OP_PUSH);
  cd_variable(OP_GREF, s_list); /* (lambda (object) (check-ref class object) */
  putcbyte(OP_CALL);		/*               (%setivar object indx val)) */
#ifndef NO_CHECK_REF
  putcbyte(4);
#else
  putcbyte(3);
#endif
  fixup(nxt2);
  putcbyte(OP_PUSH);
  cd_variable(OP_GREF, s_eval);
  putcbyte(OP_CALL);
  putcbyte(1);
  fixup(nxt1);

  if (setterp) {
    putcbyte(OP_SAVE);
    nxt1 = putcword(0);
    putcbyte(OP_PUSH);
    if (findvariable(writername, &lev, &off))
      cd_evariable(OP_EREF, lev, off);
    else
      cd_variable(OP_GREF, writername);
    putcbyte(OP_PUSH);
    putcbyte(OP_SAVE);
    nxt2 = putcword(0);
    cd_variable(OP_GREF, s_setter);
    putcbyte(OP_PUSH);
    cd_variable(OP_GREF, s_setter);
    putcbyte(OP_CALL);
    putcbyte(1);
    fixup(nxt2);
    putcbyte(OP_CALL);
    putcbyte(2);
    fixup(nxt1);
  }
  else {
    if (findvariable(writername, &lev, &off))
      cd_evariable(OP_ESET, lev, off);
    else
      cd_variable(OP_GSET, writername);
  }

}

static void do_readers(classname, slots)
  LVAL classname, slots;
{
  LVAL slot, slotl;

  for (; slots; slots = cdr(slots)) {
    slot = car(slots);
    if (consp(slot)) {
      for (slotl = cdr(slot); slotl; slotl = cdr(cdr(slotl))) {
	if (cdr(slotl) == NIL)
	  xlcerror("odd-length slot init list", slot, s_telos_error);
	if (car(slotl) == s_reader)
	  do_named_reader(car(cdr(slotl)), car(slot), classname);
      }
    }
  }
}

static void do_writers(classname, slots)
  LVAL classname, slots;
{
  LVAL slot, slotl;

  for (; slots; slots = cdr(slots)) {
    slot = car(slots);
    if (consp(slot)) {
      for (slotl = cdr(slot); slotl; slotl = cdr(cdr(slotl))) {
	if (cdr(slotl) == NIL)
	  xlcerror("odd-length slot init list", slot, s_telos_error);
	if (car(slotl) == s_writer)
	  do_named_writer(car(cdr(slotl)), car(slot), classname, FALSE);
      }
    }
  }
}

static void do_class_class(classopts)
 LVAL classopts;
{
  LVAL cls;

  cls = NIL;

  for (; classopts; classopts = cdr(cdr(classopts)))
    if (car(classopts) == s_class) {
      if (!symbolp(car(cdr(classopts))))
	xlerror("bad name for class in defclass", car(cdr(classopts)));
      cls = car(cdr(classopts));
      break;
    }

  if (cls == NIL) cls = xlenter_module("<simple-class>", root_module);
  cpush(cls);
  cd_variable(OP_GREF, cls);
  putcbyte(OP_PUSH);
  drop(1);
}

static void do_accessors(classname, slots)
 LVAL classname, slots;
{
  LVAL slot, slotl;

  for (; slots; slots = cdr(slots)) {
    slot = car(slots);
    if (consp(slot)) {
      for (slotl = cdr(slot); slotl; slotl = cdr(cdr(slotl))) {
	if (cdr(slotl) == NIL)
	  xlcerror("odd-length slot init list", slot, s_telos_error);
	if (car(slotl) == s_accessor) {
	  do_named_reader(car(cdr(slotl)), car(slot), classname);
	  do_named_writer(car(cdr(slotl)), car(slot), classname, TRUE);
	}
      }
    }
  }
}

static void check_slot_options(slots)
  LVAL slots;
{
  LVAL slot, kwd;

  for (; slots; slots = cdr(slots)) {
    slot = car(slots);

    if (symbolp(slot)) continue;

    if (!consp(slot))
      xlerror("bad slot description in defclass", slot);

    if ((length(slot) & 1) == 0)
      xlerror("odd-length slot description in defclass", slot);

    for (slot = cdr(slot); slot; slot = cdr(cdr(slot))) {
      if (!consp(slot))
	xlerror("excess form in slot description", slot);

      kwd = car(slot);
      if (kwd != s_keyword &&
	  kwd != s_default &&
	  kwd != s_reader  &&
	  kwd != s_writer  &&
	  kwd != s_accessor &&
	  kwd != s_requiredp)
	xlerror("unknown keyword in slot description", kwd);
    }
  }
}

static void check_class_options(classopts)
  LVAL classopts;
{
  if ((length(classopts) & 1) == 1)
    xlerror("odd-length class option list in defclass", classopts);

  for (; classopts; classopts = cdr(cdr(classopts))) {
    if (!consp(classopts))
      xlerror("excess form in defclass", classopts);

#if 0
    kwd = car(classopts);
    if (kwd != s_keywords &&
	kwd != s_constructor &&
	kwd != s_predicate &&
	kwd != s_class &&
	kwd != s_abstractp)
      xlerror("unknown keyword in class options", kwd);
#endif /* done in initialize-object */
  }
}

static void do_defclass(form, cont)
  LVAL form; int cont;
{
  LVAL name, super, slots, classopts;
  int lev, off, nxt, nargs;

  cpush(form);

  if (atom(form))
    xlfail("missing body in defclass", s_syntax_error);

  name = car(form);
  if (!symbolp(name))
    xlerror("bad class name in defclass", form);

  if (atom(cdr(form)))
    xlerror("missing superclass in defclass", form);

  super = car(cdr(form));
  if (!listp(super) || (super != NIL && !symbolp(car(super))))
    xlerror("bad superclass in defclass", form);

  if (atom(cdr(cdr(form))))
    xlerror("missing slot descriptions in defclass", form);

  slots = car(cdr(cdr(form)));
  if (!listp(slots))
    xlerror("bad slot descriptions in defclass", form);

  classopts = cdr(cdr(cdr(form)));

  check_slot_options(slots);
  check_class_options(classopts);

  putcbyte(OP_SAVE);
  nxt = putcword(0);

  nargs = 0;

  nargs += do_keywords(slots, classopts);
  nargs += do_slots(slots);
  nargs += do_superclass(super);
  nargs += do_abstractp(classopts);

  do_literal(name, C_NEXT);
  putcbyte(OP_PUSH);
  nargs++;

  do_literal(s_name, C_NEXT);
  putcbyte(OP_PUSH);
  nargs++;

  do_class_class(classopts);
  nargs++;

  cd_variable(OP_GREF, s_make);
  putcbyte(OP_CALL);
  putcbyte(nargs);
  fixup(nxt);

  if (findvariable(name, &lev, &off))
    cd_evariable(OP_ESET, lev, off);
  else
    cd_variable(OP_GSET, name);

  do_constructor(name, classopts);
  do_predicate(name, classopts);
  do_readers(name, slots);
  do_writers(name, slots);
  do_accessors(name, slots);

  do_literal(name, cont);

  drop(1);
}

static LVAL defcondition_slot(name, value)
  LVAL name, value;
{
  LVAL slot, key;
  char keyname[STRMAX+1];
  int len;

  if (!symbolp(name))
    xlerror("condition slot not a symbol", name);

  strcpy(keyname, getstring(getpname(name)));
  len = strlen(keyname);
  if (keyname[len-1] != ':') {
    keyname[len] = ':';
    keyname[len+1] = 0;
  }
  key = xlenter_keyword(keyname);

  slot = cons(name,
	      cons(s_keyword,
		   cons(key,
			cons(s_default,
			     cons(value, NIL)))));
  return slot;
}

static void do_defcondition(form, cont)
  LVAL form; int cont;
{
  LVAL s_condition, name, super, def;

  s_condition = xlenter_module("<condition>", get_module("condcl"));
  if (atom(form))
    xlfail("missing body in defcondition", s_syntax_error);
  name = car(form);

  if (atom(cdr(form)))
    xlerror("missing supercondition in defcondition", name);
  super = car(cdr(form));
  if (!symbolp(super) && (super != NIL))
    xlerror("bad supercondition in defcondition", super);

  cpush(form);

  def = cons(s_defclass, NIL);
  cpush(def);

  rplacd(def, cons(name, NIL));
  def = cdr(def);
  if (super == NIL)
    rplacd(def, cons(cons(s_condition, NIL), NIL));
  else
    rplacd(def, cons(cons(super, NIL), NIL));
  def = cdr(def);

  rplacd(def, cons(NIL, NIL));
  def = cdr(def);

  form = cdr(cdr(form));
  while (form) {
    if (atom(cdr(form)))
      xlerror("malformed defclass option", car(form));
    if (car(def) == NIL) {
      rplaca(def, cons(defcondition_slot(car(form), car(cdr(form))), NIL));
      def = car(def);
    }
    else {
      rplacd(def, cons(defcondition_slot(car(form), car(cdr(form))), NIL));
      def = cdr(def);
    }
    form = cdr(cdr(form));
  }

  do_expr(pop(), cont);
  drop(1);

}

LVAL xmacro_error()
{
  static char *cfn_name = "raise-macro-error";
  LVAL msg, value;

  msg = xlgastring();
  value = xlgetarg();
  xllastarg();

  xlcerror(getstring(msg), value, s_macro_error);
  return NIL;			/* not reached */
}

/* instruction output formats */
#define FMT_NONE	0
#define FMT_BYTE	1
#define FMT_LOFF	2
#define FMT_WORD	3
#define FMT_EOFF	4
#define FMT_LOFFL       5

typedef struct { int ot_code; char *ot_name; int ot_fmt; } OTDEF;
OTDEF otab[] = {
{	OP_BRT,		"BRT",		FMT_WORD	},
{	OP_BRF,		"BRF",		FMT_WORD	},
{	OP_BR,		"BR",		FMT_WORD	},
{	OP_LIT,		"LIT",		FMT_LOFF	},
{	OP_GREF,	"GREF",		FMT_LOFF	},
{	OP_GSET,	"GSET",		FMT_LOFF	},
{	OP_EREF,	"EREF",		FMT_EOFF	},
{	OP_ESET,	"ESET",		FMT_EOFF	},
{	OP_SAVE,	"SAVE",		FMT_WORD	},
{	OP_CALL,	"CALL",		FMT_BYTE	},
{	OP_RETURN,	"RETURN",	FMT_NONE	},
{	OP_T,		"T",		FMT_NONE	},
{	OP_NIL,		"NIL",		FMT_NONE	},
{	OP_PUSH,	"PUSH",		FMT_NONE	},
{	OP_CLOSE,	"CLOSE",	FMT_NONE	},
{	OP_DELAY,	"DELAY",	FMT_NONE	},

{	OP_FRAME,	"FRAME",	FMT_BYTE	},
{	OP_MVARG,	"MVARG",	FMT_BYTE	},
{	OP_MVOARG,	"MVOARG",	FMT_BYTE	},
{	OP_MVRARG,	"MVRARG",	FMT_BYTE	},
{	OP_ADROP,	"ADROP",	FMT_NONE	},
{	OP_ALAST,	"ALAST",	FMT_NONE	},

{	OP_AREF,	"AREF",		FMT_LOFF	},
{	OP_ASET,	"ASET",		FMT_LOFF	},

{       OP_CNM,         "CALL-NEXT-METHOD", FMT_NONE    },

{       OP_GREFL,       "GREFL",        FMT_LOFFL       },
{       OP_GSETL,       "GSETL",        FMT_LOFFL       },
{       OP_LITL,        "LITL",         FMT_LOFFL       },

{0,0,0}
};

/* decode_procedure - decode the instructions in a code object */
void decode_procedure(fptr,fun)
  LVAL fptr,fun;
{
    int len,lc;
    LVAL code,env;
    code = getcode(fun);
    env = getcenv(fun);
    len = getslength(getbcode(code));
    for (lc = 0; lc < len; )
	lc += decode_instruction(fptr,code,lc,env);
}

/* decode_instruction - decode a single bytecode instruction */
int decode_instruction(fptr,code,lc,env)
  LVAL fptr,code; int lc; LVAL env;
{
    unsigned char *cp;
    char buf[100];
    OTDEF *op;
    NTDEF *np;
    int i,n=1;
    LVAL tmp;

    /* get a pointer to the bytecodes for this instruction */
    cp = (unsigned char *)getstring(getbcode(code)) + lc;

    /* show the address and opcode */
    if ((tmp = getcname(code)) != NIL)
	sprintf(buf,"%s:%04x %02x ",getstring(getpname(tmp)),lc,*cp);
    else {
	sprintf(buf,AFMT,code);
	xlputstr(fptr,buf);
	fflush(getfile(fptr));
    	sprintf(buf,":%04x %02x ",lc,*cp);
    }
    xlputstr(fptr,buf); fflush(getfile(fptr));

    /* display the operands */
    for (op = otab; op->ot_name; ++op)
	if (*cp == op->ot_code) {
	    switch (op->ot_fmt) {
	    case FMT_NONE:
		sprintf(buf,"      %s\n",op->ot_name);
		xlputstr(fptr,buf); fflush(getfile(fptr));
		break;
	    case FMT_BYTE:
		sprintf(buf,"%02x    %s %02x\n",cp[1],op->ot_name,cp[1]);
		xlputstr(fptr,buf); fflush(getfile(fptr));
		n += 1;
		break;
	    case FMT_LOFF:
		sprintf(buf,"%02x    %s %02x ; ",cp[1],op->ot_name,cp[1]);
		xlputstr(fptr,buf); fflush(getfile(fptr));
		{ LVAL elt, mod;
		  elt = getelement(code,cp[1]);
		  xlprin1(elt,fptr); fflush(getfile(fptr));
		  if (symbolp(elt)) {
		    mod = getmodule(elt);
		    if (mod != NIL) {
		      xlputstr(fptr,"@"); fflush(getfile(fptr));
		      xlputstr(fptr, getstring(getmname(mod))); fflush(getfile(fptr));
		    }
		  }
		  xlterpri(fptr);
		}
		n += 1;
		break;
	    case FMT_WORD:
		sprintf(buf,"%02x %02x %s %02x%02x\n",cp[1],cp[2],
			op->ot_name,cp[1],cp[2]);
		xlputstr(fptr,buf); fflush(getfile(fptr));
		n += 2;
		break;
	    case FMT_EOFF:
		if ((i = cp[1]) == 0)
		    tmp = getvnames(code);
		else {
		    for (tmp = env; i > 1; --i) tmp = cdr(tmp);
		    tmp = getelement(car(tmp),0);
		}
		for (i = cp[2]; i > 1 && tmp; --i) tmp = cdr(tmp);
		sprintf(buf,"%02x %02x %s %02x %02x ; ",cp[1],cp[2],
			op->ot_name,cp[1],cp[2]);
		xlputstr(fptr,buf); fflush(getfile(fptr));
		if (tmp == NIL) {
		  sprintf(buf,"??? offset %d not found", cp[2]);
		  xlputstr(fptr, buf); fflush(getfile(fptr));
		}
		else
		  xlprin1(car(tmp),fptr); fflush(getfile(fptr));
		xlterpri(fptr);
		n += 2;
		break;
	    case FMT_LOFFL:
		sprintf(buf,"%02x %02x %s %02x%02x ; ",cp[1],cp[2],
			op->ot_name,cp[1],cp[2]);
		xlputstr(fptr,buf); fflush(getfile(fptr));
		{ LVAL elt, mod;
		  elt = getelement(code,(cp[1] << 8) | cp[2]);
		  xlprin1(elt,fptr); fflush(getfile(fptr));
		  if (symbolp(elt)) {
		    mod = getmodule(elt);
		    if (mod != NIL) {
		      xlputstr(fptr,"@"); fflush(getfile(fptr));
		      xlputstr(fptr, getstring(getmname(mod))); fflush(getfile(fptr));
		    }
		  }
		  xlterpri(fptr);
		}
		n += 2;
		break;
	    }
	    return (n);
	}
    
    /* check for an integrable function */
    for (np = ntab; np->nt_name; ++np)
	if (*cp == np->nt_code) {
	    sprintf(buf,"      %s\n",np->nt_name);
	    xlputstr(fptr,buf); fflush(getfile(fptr));
	    return (n);
	}

    /* unknown opcode */
    sprintf(buf,"      <UNKNOWN>\n");
    xlputstr(fptr,buf); fflush(getfile(fptr));
    return (n);
}
