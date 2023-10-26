/* xsmath.c - xscheme built-in arithmetic functions */
/*	Copyright (c) 1988, by David Michael Betz
	All Rights Reserved */
/* Euscheme code Copyright (c) 1994 Russell Bradford */

#include "xscheme.h"
#include <math.h>

/* external variables */
extern LVAL true, s_arith_error;

/* forward declarations */
#ifdef __STDC__
static LVAL binary(int fcn);
static LVAL unary(int fcn);
static LVAL predicate(int fcn);
static LVAL compare(int fcn);
static FLOTYPE toflotype(LVAL val);
static void checkizero(FIXTYPE iarg, FIXTYPE num);
static void checkineg(FIXTYPE iarg);
static void checkfzero(FLOTYPE farg, FLOTYPE num);
static void checkfneg(FLOTYPE farg);
static void badiop(void);
static void badfop(void);
#else
static LVAL unary();
static LVAL binary();
static LVAL predicate();
static LVAL compare();
static FLOTYPE toflotype();
static void checkizero();
static void checkineg();
static void checkfzero();
static void checkfneg();
static void badiop();
static void badfop();
#endif

/* xexactp - built-in function 'exact?' */
/**** THIS IS REALLY JUST A STUB FOR NOW ****/
LVAL xexactp()
{
    static char *cfn_name = "exact?";
    (void)xlganumber();
    xllastarg();
    return (NIL);
}

/* xinexactp - built-in function 'inexact?' */
/**** THIS IS REALLY JUST A STUB FOR NOW ****/
LVAL xinexactp()
{
    static char *cfn_name = "inexact?";
    (void)xlganumber();
    xllastarg();
    return (true);
}

/* xatan - built-in function 'atan' */
LVAL xatan()
{
    static char *cfn_name = "atan";
    LVAL arg,arg2;
    FLOTYPE val;
    
    /* get the first argument */
    arg = xlganumber();
    
    /* handle two argument (atan y x) */
    if (moreargs()) {
	arg2 = xlganumber();
	xllastarg();
	val = atan2(toflotype(arg),toflotype(arg2));
    }
    
    /* handle one argument (atan x) */
    else
	val = atan(toflotype(arg));

    /* return the resulting flonum */
    return (cvflonum(val));
}

/* xfloor - built-in function 'floor' */
LVAL xfloor()
{
    static char *cfn_name = "floor";
    LVAL arg;

    /* get the argument */
    arg = xlgetarg();
    xllastarg();

    /* check its type */
    if (fixp(arg))
	return (arg);
    else if (floatp(arg))
	return (cvfixnum((FIXTYPE)floor(getflonum(arg))));
    xlbadtype(arg, "<number>", cfn_name);
    return (NIL); /* never reached */
}

/* xceiling - built-in function 'ceiling' */
LVAL xceiling()
{
    static char *cfn_name = "ceiling";
    LVAL arg;

    /* get the argument */
    arg = xlgetarg();
    xllastarg();

    /* check its type */
    if (fixp(arg))
	return (arg);
    else if (floatp(arg))
	return (cvfixnum((FIXTYPE)ceil(getflonum(arg))));
    xlbadtype(arg, "<number>", cfn_name);
    return (NIL); /* never reached */
}

/* xround - built-in function 'round' */
LVAL xround()
{
    static char *cfn_name = "round";
    FLOTYPE x,y,z;
    LVAL arg;

    /* get the argument */
    arg = xlgetarg();
    xllastarg();

    /* check its type */
    if (fixp(arg))
	return (arg);
    else if (floatp(arg)) {
	x = getflonum(arg);
	y = floor(x);
	z = x - y;
	if (z == 0.5) {
	    if (((FIXTYPE)y & 1) == 1)
		y += 1.0;
	    return (cvfixnum((FIXTYPE)y));
	}
	else if (z < 0.5)
	    return (cvfixnum((FIXTYPE)y));
	else
	    return (cvfixnum((FIXTYPE)(y + 1.0)));
    }
    xlbadtype(arg, "<number>", cfn_name);
    return (NIL); /* never reached */
}

/* xtruncate - built-in function 'truncate' */
LVAL xtruncate()
{
    static char *cfn_name = "truncate";
    LVAL arg;

    /* get the argument */
    arg = xlgetarg();
    xllastarg();

    /* check its type */
    if (fixp(arg))
	return (arg);
    else if (floatp(arg))
	return (cvfixnum((FIXTYPE)(getflonum(arg))));
    xlbadtype(arg, "<number>", cfn_name);
    return (NIL); /* never reached */
}

/* binary functions */
LVAL xadd()				/* + */
{
    if (!moreargs())
	return (cvfixnum((FIXTYPE)0));
    return (binary('+'));
}
LVAL xmul()				/* * */
{
    if (!moreargs())
	return (cvfixnum((FIXTYPE)1));
    return (binary('*'));
}
LVAL xsub()    { return (binary('-')); } /* - */
LVAL xdiv()    { return (binary('/')); } /* / */
LVAL xquo()    { return (binary('Q')); } /* quotient */
LVAL xrem()    { return (binary('R')); } /* remainder */
LVAL xmin()    { return (binary('m')); } /* min */
LVAL xmax()    { return (binary('M')); } /* max */
LVAL xexpt()   { return (binary('E')); } /* expt */
LVAL xlogand() { return (binary('&')); } /* logand */
LVAL xlogior() { return (binary('|')); } /* logior */
LVAL xlogxor() { return (binary('^')); } /* logxor */

/* binary - handle binary operations */
static LVAL binary(fcn)
  int fcn;
{
    static char *cfn_name = "binary arith op";
    FIXTYPE ival,iarg;
    FLOTYPE fval,farg;
    LVAL arg;
    int mode;

    /* get the first argument */
    arg = xlgetarg();

    /* set the type of the first argument */
    if (fixp(arg)) {
	ival = getfixnum(arg);
	mode = 'I';
    }
    else if (floatp(arg)) {
	fval = getflonum(arg);
	mode = 'F';
    }
    else
	xlbadtype(arg, "<number>", cfn_name);

    /* treat a single argument as a special case */
    if (!moreargs()) {
	switch (fcn) {
	case '-':
	    switch (mode) {
	    case 'I':
		ival = -ival;
		break;
	    case 'F':
		fval = -fval;
		break;
	    }
	    break;
	case '/':
	    switch (mode) {
	    case 'I':
		checkizero(ival, (FIXTYPE)1);
#ifdef OLDARITH
		if (ival != 1) {
		    fval = 1.0 / (FLOTYPE)ival;
		    mode = 'F';
		  }
#else
		ival = 1 / ival;
#endif
		break;
	    case 'F':
		checkfzero(fval, (FLOTYPE)1.0);
		fval = 1.0 / fval;
		break;
	    }
	}
    }

    /* handle each remaining argument */
    while (moreargs()) {

	/* get the next argument */
	arg = xlgetarg();

	/* check its type */
	if (fixp(arg)) {
	    switch (mode) {
	    case 'I':
	        iarg = getfixnum(arg);
	        break;
	    case 'F':
	        farg = (FLOTYPE)getfixnum(arg);
		break;
	    }
	}
	else if (floatp(arg)) {
	    switch (mode) {
	    case 'I':
	        fval = (FLOTYPE)ival;
		farg = getflonum(arg);
		mode = 'F';
		break;
	    case 'F':
	        farg = getflonum(arg);
		break;
	    }
	}
	else
	    xlbadtype(arg, "<number>", cfn_name);

	/* accumulate the result value */
	switch (mode) {
	case 'I':
	    switch (fcn) {
	    case '+':	ival += iarg; break;
	    case '-':	ival -= iarg; break;
	    case '*':	ival *= iarg; break;
	    case '/':	checkizero(iarg, ival);
#ifdef OLDARITH
			if ((ival % iarg) == 0)	    
			    ival /= iarg;
			else {
			    fval = (FLOTYPE)ival;
			    farg = (FLOTYPE)iarg;
			    fval /= farg;
			    mode = 'F';
			}
#else
	                ival /= iarg;
#endif
			break;
	    case 'Q':	checkizero(iarg, ival); ival /= iarg; break;
	    case 'R':	checkizero(iarg, ival); ival %= iarg; break;
	    case 'M':	if (iarg > ival) ival = iarg; break;
	    case 'm':	if (iarg < ival) ival = iarg; break;
	    case 'E':	return (cvflonum((FLOTYPE)pow((FLOTYPE)ival,(FLOTYPE)iarg)));
	    case '&':	ival &= iarg; break;
	    case '|':	ival |= iarg; break;
	    case '^':	ival ^= iarg; break;
	    default:	badiop();
	    }
	    break;
	case 'F':
	    switch (fcn) {
	    case '+':	fval += farg; break;
	    case '-':	fval -= farg; break;
	    case '*':	fval *= farg; break;
	    case '/':	checkfzero(farg, fval); fval /= farg; break;
	    case 'M':	if (farg > fval) fval = farg; break;
	    case 'm':	if (farg < fval) fval = farg; break;
	    case 'E':	fval = pow(fval,farg); break;
	    default:	badfop();
	    }
    	    break;
	}
    }

    /* return the result */
    switch (mode) {
    case 'I':	return (cvfixnum(ival));
    case 'F':	return (cvflonum(fval));
    }
    return (NIL); /* never reached */
}

/* unary functions */
LVAL xlognot() { return (unary('~')); } /* lognot */
LVAL xabs()      { return (unary('A')); } /* abs */
LVAL xadd1()     { return (unary('+')); } /* 1+ */
LVAL xsub1()     { return (unary('-')); } /* -1+ */
LVAL xsin()      { return (unary('S')); } /* sin */
LVAL xcos()      { return (unary('C')); } /* cos */
LVAL xtan()      { return (unary('T')); } /* tan */
LVAL xasin()     { return (unary('s')); } /* asin */
LVAL xacos()     { return (unary('c')); } /* acos */
LVAL xxexp()     { return (unary('E')); } /* exp */
LVAL xsqrt()     { return (unary('R')); } /* sqrt */
LVAL xxlog()     { return (unary('L')); } /* log */
LVAL xrandom()   { return (unary('?')); } /* random */

static char *unary_to_name(fcn)
  int fcn;
{
  switch (fcn) {
  case '~': return "lognot";
  case 'A': return "abs";
  case '+': return "+";
  case '-': return "-";
  case 'S': return "sin";
  case 'C': return "cos";
  case 'T': return "tan";
  case 's': return "asin";
  case 'c': return "acos";
  case 'E': return "exp";
  case 'R': return "sqrt";
  case 'L': return "log";
  case '?': return "random";
  }

  return "unknown unary op";
}

/* unary - handle unary operations */
static LVAL unary(fcn)
  int fcn;
{
    FLOTYPE fval;
    FIXTYPE ival;
    LVAL arg;

    /* get the argument */
    if (moreargs()) {		/* arg = xlgetarg(); */
      arg = nextarg();
    }
    else {
      xltoofew(unary_to_name(fcn));
    }

    if (xlargc != 0) {		/* xllastarg(); */
      xltoomany(unary_to_name(fcn));
    }

    /* check its type */
    if (fixp(arg)) {
	ival = getfixnum(arg);
	switch (fcn) {
	case '~':	ival = ~ival; break;
	case 'A':	ival = (ival < 0 ? -ival : ival); break;
	case '+':	ival++; break;
	case '-':	ival--; break;
	case 'S':	return (cvflonum((FLOTYPE)sin((FLOTYPE)ival)));
	case 'C':	return (cvflonum((FLOTYPE)cos((FLOTYPE)ival)));
	case 'T':	return (cvflonum((FLOTYPE)tan((FLOTYPE)ival)));
	case 's':	return (cvflonum((FLOTYPE)asin((FLOTYPE)ival)));
	case 'c':	return (cvflonum((FLOTYPE)acos((FLOTYPE)ival)));
	case 't':	return (cvflonum((FLOTYPE)atan((FLOTYPE)ival)));
	case 'E':	return (cvflonum((FLOTYPE)exp((FLOTYPE)ival)));
	case 'L':	return (cvflonum((FLOTYPE)log((FLOTYPE)ival)));
	case 'R':	checkineg(ival);
			return (cvflonum((FLOTYPE)sqrt((FLOTYPE)ival)));
	case '?':	ival = (FIXTYPE)osrand((int)ival); break;
	default:	badiop();
	}
	return (cvfixnum(ival));
    }
    else if (floatp(arg)) {
	fval = getflonum(arg);
	switch (fcn) {
	case 'A':	fval = (fval < 0.0 ? -fval : fval); break;
	case '+':	fval += 1.0; break;
	case '-':	fval -= 1.0; break;
	case 'S':	fval = sin(fval); break;
	case 'C':	fval = cos(fval); break;
	case 'T':	fval = tan(fval); break;
	case 's':	fval = asin(fval); break;
	case 'c':	fval = acos(fval); break;
	case 't':	fval = atan(fval); break;
	case 'E':	fval = exp(fval); break;
	case 'L':	fval = log(fval); break;
	case 'R':	checkfneg(fval);
			fval = sqrt(fval); break;
	default:	badfop();
	}
	return (cvflonum(fval));
    }

    xlbadtype(arg, "<number>", unary_to_name(fcn));
    return (NIL); /* never reached */
}

/* xgcd - greatest common divisor */
LVAL xgcd()
{
    static char *cfn_name = "gcd";
    FIXTYPE m,n,r;
    LVAL arg;

    if (!moreargs())			/* check for identity case */
	return (cvfixnum((FIXTYPE)0));
    arg = xlgafixnum();
    n = getfixnum(arg);
    if (n < (FIXTYPE)0) n = -n;		/* absolute value */
    while (moreargs()) {
	arg = xlgafixnum();
	m = getfixnum(arg);
	if (m < (FIXTYPE)0) m = -m;	/* absolute value */
	if (n > 0)
	  for (;;) {			/* euclid's algorithm */
	    r = m % n;
	    if (r == (FIXTYPE)0)
	      break;
	    m = n;
	    n = r;
	  }
	else n = m;		/* (gcd 0 m) */
    }
    return (cvfixnum(n));
}

/* unary predicates */
LVAL xnegativep() { return (predicate('-')); } /* negative? */
LVAL xzerop()     { return (predicate('Z')); } /* zero? */
LVAL xpositivep() { return (predicate('+')); } /* positive? */
LVAL xevenp()     { return (predicate('E')); } /* even? */
LVAL xoddp()      { return (predicate('O')); } /* odd? */

/* predicate - handle a predicate function */
static LVAL predicate(fcn)
  int fcn;
{
    static char *cfn_name = "arith predicate";
    FLOTYPE fval;
    FIXTYPE ival;
    LVAL arg;

    /* get the argument */
    arg = xlgetarg();
    xllastarg();

    /* check the argument type */
    if (fixp(arg)) {
	ival = getfixnum(arg);
	switch (fcn) {
	case '-':	ival = (ival < 0); break;
	case 'Z':	ival = (ival == 0); break;
	case '+':	ival = (ival > 0); break;
	case 'E':	ival = ((ival & 1) == 0); break;
	case 'O':	ival = ((ival & 1) != 0); break;
	default:	badiop();
	}
    }
    else if (floatp(arg)) {
	fval = getflonum(arg);
	switch (fcn) {
	case '-':	ival = (fval < 0); break;
	case 'Z':	ival = (fval == 0); break;
	case '+':	ival = (fval > 0); break;
	default:	badfop();
	}
    }
    else
	xlbadtype(arg, "<number>", cfn_name);

    /* return the result value */
    return (ival ? true : NIL);
}

/* comparison functions */
LVAL xlss() { return (compare('<')); } /* < */
LVAL xleq() { return (compare('L')); } /* <= */
LVAL xeql() { return (compare('=')); } /* = */
LVAL xgeq() { return (compare('G')); } /* >= */
LVAL xgtr() { return (compare('>')); } /* > */

/* compare - common compare function */
static LVAL compare(fcn)
  int fcn;
{
    static char *cfn_name = "arith compare op";
    FIXTYPE icmp,ival,iarg;
    FLOTYPE fcmp,fval,farg;
    LVAL arg;
    int mode;

    /* get the first argument */
    arg = xlgetarg();

    /* set the type of the first argument */
    if (fixp(arg)) {
	ival = getfixnum(arg);
	mode = 'I';
    }
    else if (floatp(arg)) {
	fval = getflonum(arg);
	mode = 'F';
    }
    else
	xlbadtype(arg, "<number>", cfn_name);

    /* handle each remaining argument */
    for (icmp = TRUE; icmp && moreargs(); ) {

	/* get the next argument */
	arg = xlgetarg();

	/* check its type */
	if (fixp(arg)) {
	    switch (mode) {
	    case 'I':
	        iarg = getfixnum(arg);
	        break;
	    case 'F':
	        farg = (FLOTYPE)getfixnum(arg);
		break;
	    }
	}
	else if (floatp(arg)) {
	    switch (mode) {
	    case 'I':
	        fval = (FLOTYPE)ival;
		farg = getflonum(arg);
		mode = 'F';
		break;
	    case 'F':
	        farg = getflonum(arg);
		break;
	    }
	}
	else
	    xlbadtype(arg, "<number>", cfn_name);

	/* compute result of the compare */
	switch (mode) {
	case 'I':
	    icmp = ival - iarg;
	    switch (fcn) {
	    case '<':	icmp = (icmp < 0); break;
	    case 'L':	icmp = (icmp <= 0); break;
	    case '=':	icmp = (icmp == 0); break;
	    case 'G':	icmp = (icmp >= 0); break;
	    case '>':	icmp = (icmp > 0); break;
	    }
	    break;
	case 'F':
	    fcmp = fval - farg;
	    switch (fcn) {
	    case '<':	icmp = (fcmp < 0.0); break;
	    case 'L':	icmp = (fcmp <= 0.0); break;
	    case '=':	icmp = (fcmp == 0.0); break;
	    case 'G':	icmp = (fcmp >= 0.0); break;
	    case '>':	icmp = (fcmp > 0.0); break;
	    }
	    break;
	}

	/* update the values */
	ival = iarg;
	fval = farg;
    }

    /* get rid of extra arguments */
    if (moreargs()) xlpoprest();

    /* return the result */
    return (icmp ? true : NIL);
}

/* toflotype - convert a lisp value to a floating point number */
static FLOTYPE toflotype(val)
  LVAL val;
{
    /* must be a number for this to work */
    switch (ntype(val)) {
    case FIXNUM:	return ((FLOTYPE)getfixnum(val));
    case FLONUM:	return (getflonum(val));
    }
    return ((FLOTYPE)0); /* never reached */
}

/* checkizero - check for integer division by zero */
static void checkizero(iarg, num)
  FIXTYPE iarg, num;
{
    if (iarg == 0)
	xlcerror("division by zero", cvfixnum(num), s_arith_error);
}

/* checkineg - check for square root of a negative number */
static void checkineg(iarg)
  FIXTYPE iarg;
{
    if (iarg < 0)
	xlcerror("square root of a negative number",
		 cvfixnum(iarg),
		 s_arith_error);
}

/* checkfzero - check for floating point division by zero */
static void checkfzero(farg, num)
  FLOTYPE farg, num;
{
    if (farg == 0.0)
	xlcerror("division by zero", cvflonum(num), s_arith_error);
}

/* checkfneg - check for square root of a negative number */
static void checkfneg(farg)
  FLOTYPE farg;
{
    if (farg < 0.0)
	xlcerror("square root of a negative number",
		 cvflonum(farg),
		 s_arith_error);
}

/* badiop - bad integer operation */
static void badiop()
{
    xlfail("bad integer operation", s_arith_error);
}

/* badfop - bad floating point operation */
static void badfop()
{
    xlfail("bad floating point operation", s_arith_error);
}
