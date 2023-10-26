/* xssym.c - symbol handling routines */
/*	Copyright (c) 1988, by David Michael Betz
	All Rights Reserved */
/* Euscheme code Copyright (c) 1994 Russell Bradford */

#include "xscheme.h"

LVAL obarray;

/* forward declarations */
#ifdef __STDC__
static LVAL findprop(LVAL sym,LVAL prp), findsyntax(LVAL sym,LVAL prp);
#else
static LVAL findprop(), findsyntax();
#endif

extern LVAL c_dot, s_rename_flag;

/* xlsubr - define a builtin function */
void xlsubr(sname,type,fcn,offset)
  char *sname; int type; LVAL (*fcn)(); int offset;
{
    LVAL sym;
    sym = xlenter(sname);
    setvalue(sym,cvsubr(type,fcn,offset));
}

#if 0
#define trace_symbol(a,ss,m) fprintf(stderr,"<%s:%s:%s>", a, ss, m)
#else
#define trace_symbol(a,s,m)
#endif

LVAL xlenter_keyword(name)
  char *name;
{
    LVAL sym,symbol;
    int i;

    i = hash(name,HSIZE);

    /* check if symbol is already in table */
    for (sym = getelement(keyword_array,i); sym; sym = cdr(sym))
      if (strcmp(name,getstring(getpname(car(sym)))) == 0) {
	trace_symbol("O", name, "keyword");
	return (car(sym));
      }

    /* make a new symbol node and link it into the list */
    symbol = cvsymbol(name);
    setmodule(symbol,NIL);
    setvalue(symbol, symbol);	/* self-evaluating */

    sym = cons(symbol,getelement(keyword_array,i));
    setelement(keyword_array,i,sym);

    trace_symbol("N", name, "keyword");
    /* return the new symbol */
    return(symbol);
}

/* xlenter - enter a symbol into the obarray of the given module */
LVAL xlenter_module(name, module)
  char *name;
  LVAL module;
{
    LVAL array,sym,symbol;
    int i, len;
    char buf[STRMAX];

    /* see if symbol is marked for reinternment */
    len = strlen(name);
    if (len > 0 && name[0] == SYNTAX_OPEN && name[len-1] == SYNTAX_CLOSE) {
      strcpy(buf, name+1);
      buf[len-2] = 0;
      return xlenter_module(buf, reintern_module);
    }

    /* get the current obarray and the hash index for this symbol */
    array = getmsymbols(module);
    i = hash(name,HSIZE);

    /* check if symbol is already in table */
    for (sym = getelement(array,i); sym; sym = cdr(sym))
      if (strcmp(name,getstring(getpname(car(sym)))) == 0) {
	trace_symbol("O", name, getstring(getmname(getmodule(car(sym)))));
	return (car(sym));
      }

    /* make a new symbol node */
    symbol = cvsymbol(name);

    /* ensure correct home module */
    setmodule(symbol,module);

    /* link it into the list */
    sym = cons(symbol,getelement(array,i));
    setelement(array,i,sym);

    trace_symbol("N", name, getstring(getmname(module)));
    /* return the new symbol */
    return(symbol);
}

/* xlgetprop - get the value of a property */
LVAL xlgetprop(sym,prp)
  LVAL sym,prp;
{
    LVAL p;
    return ((p = findprop(sym,prp)) == NIL ? NIL : car(p));
}

/* xlputprop - put a property value onto the property list */
void xlputprop(sym,val,prp)
  LVAL sym,val,prp;
{
    LVAL pair;
    if ((pair = findprop(sym,prp)) != NIL)
	rplaca(pair,val);
    else
	setplist(sym,cons(prp,cons(val,getplist(sym))));
}

/* findprop - find a property pair */
static LVAL findprop(sym,prp)
  LVAL sym,prp;
{
    LVAL p;
    for (p = getplist(sym); consp(p) && consp(cdr(p)); p = cdr(cdr(p)))
	if (symboleq(car(p), prp))
	    return (cdr(p));
    return (NIL);
}

/* intern symbol in current module */
/* can't use symbol name directly as it might move during a GC in xlenter */
static LVAL reintern_symbol(sym)
  LVAL sym;
{
  char buf[STRMAX+1];

  if (keywordp(sym)) return sym;
  strcpy(buf, getstring(getpname(sym)));
  return xlenter(buf);
}

/* xlgetsyntax - find a macro or syntax property */
LVAL xlgetsyntax(sym,prp)
  LVAL sym, prp;
{
    LVAL p;

    /* reintern into current module if necessary */
    if (getmodule(sym) == reintern_module) {
      sym = reintern_symbol(sym);

/*      fprintf(stderr, "<re %s %s %s>", getstring(getpname(sym)),
	      getstring(getmname(current_module)),
	      getstring(getpname(prp)));*/
      p = findsyntax(sym,prp);
      return (p == NIL ? (prp == s_rename_flag ? sym : NIL) : car(p));
    }

    p = findsyntax(sym,prp);
    return (p == NIL ? NIL : car(p));
}

void xlputsyntax(sym,val,prp)
  LVAL sym,val,prp;
{
    LVAL pair;
    if ((pair = findsyntax(sym,prp)) != NIL)
	rplaca(pair,val);
    else
	putsyntax(sym,cons(prp,cons(val,getsyntax(sym))));
}

static LVAL findsyntax(sym,prp)
  LVAL sym,prp;
{
    LVAL p;
    for (p = getsyntax(sym); consp(p) && consp(cdr(p)); p = cdr(cdr(p)))
	if (car(p) == prp)	/* only ever %macro, %rename or %syntax */
	    return (cdr(p));
    return (NIL);
}

/* hash - hash a symbol name string */
int hash(str,len)
  char *str; int len;
{
    int i;
    for (i = 0; *str; )
	i = (i << 2) ^ *str++;
    i %= len;
    return (i < 0 ? -i : i);
}
