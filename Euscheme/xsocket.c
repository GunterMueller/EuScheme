/* this code possibly written by David Halls */
/* socket.c -- functions for socket manipulation */
/* Euscheme code Copyright (c) 1994 Russell Bradford */

/* WARNING: this code is under development, and is possibly buggy */

#ifdef SOCK

#ifdef __STDC__
#include <stdlib.h>
#endif
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <fcntl.h>
#include <sys/time.h>
#include <string.h>
#include <rpc/types.h>
#if defined(sgi) && defined(SVR3)
/* infelicity in xdr.h on SGI 4.0.5 */
#include <stdio.h>
#endif
#include <rpc/xdr.h>
#include <errno.h>
#ifdef _ALL_SOURCE
#include <sys/select.h>
#endif

#include "xscheme.h"

#ifdef sgi
#define HAVE_STRERROR
#endif

#ifdef sun
#ifdef __SVR4
#define HAVE_STRERROR
#else
#define HAVE_ERRLIST
extern char *sys_errlist[];
#endif
#endif

/* AIX */
#ifdef _ALL_SOURCE
#define HAVE_STRERROR
#endif

#ifndef SEEK_CUR
#define SEEK_CUR 1
#endif

extern LVAL s_socket_error, true, s_unbound;
extern LVAL cvstring2();

#if 1
extern int reading, ctrl_c;
#define HANDLE(expr) \
reading = 1; \
expr; \
reading = 0; \
if (ctrl_c) { \
  ctrl_c = 0; \
  osflush(); \
  xltoplevel(); \
}
#else
#define HANDLE(expr) expr
#endif

#define FALSEVALUE NIL
#define TRUEVALUE  true

void xlsockerror(msg, val)
  char *msg;
  LVAL val;
{
  char buf[128];

#ifdef HAVE_STRERROR
  sprintf(buf, "%s: %s", msg, strerror(errno)); 
#else
#ifdef HAVE_ERRLIST
  sprintf(buf, "%s: %s", msg, sys_errlist[errno]);
#else
  sprintf(buf, "%s: errno = %d", msg, errno);
#endif
#endif

  errno = 0;
  xlcerror(buf, val, s_socket_error);
}

/* (socket-socket) */
LVAL socket_socket()
{
  static char *cfn_name = "socket-socket";
  int s;

  xllastarg();

  if ((s = socket(AF_INET, SOCK_STREAM, 0)) < 0)
    xlsockerror(cfn_name, cvstring("inet-stream"));
  
  return (cvfixnum((FIXTYPE)s));
}

/* (socket-connect fd name port) */
LVAL socket_connect()
{
  static char *cfn_name = "socket-connect";
  int s; char *name; int port;
  struct sockaddr_in sin;
  struct hostent *hp;

  LVAL arg_s, arg_name, arg_port;

  arg_s = xlgafixnum();
  s = (int)getfixnum(arg_s);

  arg_name = xlgastring();
  name = (char*)getstring(arg_name);

  arg_port = xlgafixnum();
  port = (int)getfixnum(arg_port);

  xllastarg();

#if 0
  if (port > 0 && port < IPPORT_RESERVED)
    xlcerror("socket-connect: port out of range", arg_port, s_socket_error);
#endif

  if ((hp = gethostbyname(name)) != NULL)
    memcpy(&sin.sin_addr.s_addr, hp->h_addr, hp->h_length);
  else
    xlcerror("socket-connect: unknown host", arg_name, s_socket_error);

  sin.sin_family = AF_INET;
  sin.sin_port = (port <= 0) ? 0 : htons(port);

  if (connect(s, (struct sockaddr*) &sin, sizeof sin) < 0)
    xlsockerror("socket-connect", cons(arg_name, arg_port));

  return TRUEVALUE;
}

/* (socket-bind fd port) */
LVAL socket_bind()
{
  static char *cfn_name = "socket-bind";
  int s; int port;
  struct sockaddr_in sin;

  LVAL arg_s, arg_port;

  arg_s = xlgafixnum();
  s = (int)getfixnum(arg_s);

  arg_port = xlgafixnum();
  port = (int)getfixnum(arg_port);

  xllastarg();

  if (port > 0 && port < IPPORT_RESERVED)
    xlcerror("socket-bind: port out of range", arg_port, s_socket_error);

  sin.sin_family = AF_INET;
  sin.sin_addr.s_addr = htonl(INADDR_ANY);
  sin.sin_port = (port <= 0) ? 0 : htons(port);

  if (bind(s, (struct sockaddr*) &sin, sizeof sin) < 0)
    xlsockerror(cfn_name, arg_port);

  return TRUEVALUE;
}

/* (socket-listen fd backlog) */
LVAL socket_listen()
{
  static char *cfn_name = "socket-listen";
  int s; int backlog;
  LVAL arg_s, arg_backlog;

  arg_s = xlgafixnum();
  s = (int)getfixnum(arg_s);

  arg_backlog = xlgafixnum();
  backlog = (int)getfixnum(arg_backlog);

  xllastarg();

  if (listen(s, backlog) < 0)
    xlsockerror(cfn_name, arg_s);

  return TRUEVALUE;
}

/* (socket-accept fd) */
LVAL socket_accept()
{
  static char *cfn_name = "socket-accept";
  int s;
  struct sockaddr_in sin;
  int new_socket, len = sizeof sin;

  LVAL arg_s;

  arg_s = xlgafixnum();
  s = (int)getfixnum(arg_s);

  xllastarg();

  if ((new_socket = accept(s, (struct sockaddr*) &sin, &len)) < 0)
    xlsockerror(cfn_name, arg_s);

  return (cvfixnum((FIXTYPE)new_socket));
}

/* (socket-block fd) */
LVAL socket_block()
{
  static char *cfn_name = "socket-block";
  int s;
  int fileflags;

  LVAL arg_s;

  arg_s = xlgafixnum();
  s = (int)getfixnum(arg_s);

  xllastarg();

  if ((fileflags = fcntl(s, F_GETFL)) < 0)
    xlsockerror(cfn_name, arg_s);

  if (fcntl(s, F_SETFL, fileflags & ~O_NDELAY) < 0)
    xlsockerror(cfn_name, arg_s);
  
  return TRUEVALUE;
}

/* (socket-nonblock fd) */
LVAL socket_nonblock()
{
  static char *cfn_name = "socket-nonblock";
  int s;
  int fileflags;

  LVAL arg_s;

  arg_s = xlgafixnum();
  s = (int)getfixnum(arg_s);

  xllastarg();

  if ((fileflags = fcntl(s, F_GETFL)) < 0)
    xlsockerror(cfn_name, arg_s);

  if (fcntl(s, F_SETFL, fileflags | O_NDELAY) < 0)
    xlsockerror(cfn_name, arg_s);

  return TRUEVALUE;
}

/* (socket-reuse fd) */
LVAL socket_reuse()
{
  static char *cfn_name = "socket-reuse";
  int s;
  int val = 1;

  LVAL arg_s;

  arg_s = xlgafixnum();
  s = (int)getfixnum(arg_s);

  xllastarg();

  setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (char*)&val, sizeof val);

  return TRUEVALUE;
}

/* (socket-noreuse fd) */
LVAL socket_noreuse()
{
  static char *cfn_name = "socket-noreuse";
  int s;
  int val = 0;

  LVAL arg_s;

  arg_s = xlgafixnum();
  s = (int)getfixnum(arg_s);

  xllastarg();

  setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (char*)&val, sizeof val);

  return TRUEVALUE;
}

/* (socket-close fd) */
LVAL socket_close()
{
  static char *cfn_name = "socket-close";
  int s;

  LVAL arg_s;

  arg_s = xlgafixnum();
  s = (int)getfixnum(arg_s);

  xllastarg();

  close(s);

  return TRUEVALUE;
}

/* (socket-shutdown fd how) */
LVAL socket_shutdown()
{
  static char *cfn_name = "socket-shutdown";
  int s; int how;

  LVAL arg_s, arg_how;

  arg_s = xlgafixnum();
  s = (int)getfixnum(arg_s);

  arg_how = xlgafixnum();
  how = (int)getfixnum(arg_how);

  xllastarg();

  if (shutdown(s, how) < 0)
    xlsockerror(cfn_name, arg_s);

  return TRUEVALUE;
}

/* (socket-peeraddr fd) */
LVAL socket_peeraddr()
{
  static char *cfn_name = "socket-peeraddr";
  int s;
  struct sockaddr_in peer_name;
  int peer_namelen = sizeof peer_name;
  struct hostent *hp;

  LVAL arg_s;

  arg_s = xlgafixnum();
  s = (int)getfixnum(arg_s);

  xllastarg();

  if (getpeername(s, (struct sockaddr*) &peer_name, &peer_namelen) < 0)
    xlsockerror(cfn_name, arg_s);

  if ((hp = gethostbyaddr((char*)&peer_name.sin_addr.s_addr,
			  sizeof peer_name.sin_addr.s_addr, AF_INET)) == NULL)
    xlcerror("socket-peeraddr: unknown address",
	     cvfixnum((FIXTYPE)peer_name.sin_addr.s_addr), s_socket_error);

  return (cvstring(hp->h_name));
}

/* (socket-peerport fd) */
LVAL socket_peerport()
{
  static char *cfn_name = "socket-peerport";
  int s;
  struct sockaddr_in peer_name;
  int peer_namelen = sizeof peer_name;

  LVAL arg_s;

  arg_s = xlgafixnum();
  s = (int)getfixnum(arg_s);

  xllastarg();

  if (getpeername(s, (struct sockaddr*) &peer_name, &peer_namelen) < 0)
    xlsockerror(cfn_name, arg_s);

  return (cvfixnum((FIXTYPE)ntohs(peer_name.sin_port)));
}

/* (socket-sockaddr fd) */
LVAL socket_sockaddr()
{
  static char *cfn_name = "socket-sockaddr";
  int s;
  struct sockaddr_in sock_name;
  int sock_namelen = sizeof sock_name;
  struct hostent *hp;

  LVAL arg_s;

  arg_s = xlgafixnum();
  s = (int)getfixnum(arg_s);

  xllastarg();

  if (getsockname(s, (struct sockaddr*) &sock_name, &sock_namelen) < 0)
    xlsockerror(cfn_name, arg_s);

  if ((hp = gethostbyaddr((char*)&sock_name.sin_addr.s_addr,
			  sizeof sock_name.sin_addr.s_addr, AF_INET)) == NULL)
    xlcerror("socket-sockaddr: unknown address",
	     cvfixnum((FIXTYPE)sock_name.sin_addr.s_addr), s_socket_error);

  return (cvstring(hp->h_name));
}

/* (socket-sockport fd) */
LVAL socket_sockport()
{
  static char *cfn_name = "socket-sockport";
  int s;
  struct sockaddr_in sock_name;
  int sock_namelen = sizeof sock_name;

  LVAL arg_s;

  arg_s = xlgafixnum();
  s = (int)getfixnum(arg_s);

  xllastarg();

  if (getsockname(s, (struct sockaddr*) &sock_name, &sock_namelen) < 0)
    xlsockerror(cfn_name, arg_s);

  return (cvfixnum((FIXTYPE)ntohs(sock_name.sin_port)));
}

/* (socket-host-to-ip name) */
LVAL socket_host_to_ip()
{
  static char *cfn_name = "socket-host-to-ip";
  char *name;
  struct hostent *hp;
  struct in_addr in;

  LVAL arg_name;

  arg_name = xlgastring();
  name = (char*)getstring(arg_name);

  xllastarg();

  if ((hp = gethostbyname(name)) == NULL)
    xlcerror("host->ip: unknown host", arg_name, s_socket_error);

  memcpy(&in.s_addr, hp->h_addr, hp->h_length);
  return (cvstring(inet_ntoa(in))); 
}

/* (socket-ip-to-host ip) */
LVAL socket_ip_to_host()
{
  static char *cfn_name = "socket-ip-to-host";
  char *ip;
  unsigned long inet;
  struct hostent *hp;

  LVAL arg_ip;

  arg_ip = xlgastring();
  ip = (char*)getstring(arg_ip);

  xllastarg();

  if ((inet = inet_addr(ip)) < 0)
    xlcerror("ip->host: cannot convert IP address", arg_ip, s_socket_error);

  if ((hp = gethostbyaddr((char*)&inet, sizeof inet, AF_INET)) == NULL)
    xlcerror("ip->host: unknown ip address", arg_ip, s_socket_error);

  return (cvstring(hp->h_name));
}

/* convert to port to get I/O routines for free */

/* (socket-convert-to-port fd) */
LVAL socket_convert_to_port()
{
  static char *cfn_name = "socket-convert-to-port";
  int s;
  FILE *handle;

  LVAL arg_s;
  
  arg_s = xlgafixnum();
  s = (int)getfixnum(arg_s);

  xllastarg();

  if ((handle = fdopen(s, "a+")) == NULL)
    xlsockerror(cfn_name, arg_s);

  /* make socket unbuffered so we don't lose so much data */
  setvbuf(handle, NULL, _IONBF, 0) < 0;

  return cvport(handle, PF_INPUT | PF_OUTPUT);
}

/* (port-fd port) */
LVAL port_fd()
{
  static char *cfn_name = "port-fd";
  FILE *handle;
  
  LVAL arg_handle;

  arg_handle = xlgaport();
  handle = getfile(arg_handle);

  xllastarg();

  return (cvfixnum((FIXTYPE)fileno(handle)));
}

/* buffering stuff loses data on most OSes */
#ifdef __linux
/* (port-unbuffered port) */
LVAL port_unbuffered()
{
  static char *cfn_name = "port-unbuffered";
  FILE *handle;

  LVAL arg_handle;
  
  arg_handle = xlgaport();
  handle = getfile(arg_handle);

  xllastarg();

  fflush(handle);
  if (setvbuf(handle, NULL, _IONBF, 0) < 0)
    xlsockerror(cfn_name, arg_handle);

  return TRUEVALUE;
}

/* (port-block-buffered port . bufsize) */
LVAL port_block_buffered()
{
  static char *cfn_name = "port-block-buffered";
  FILE *handle;
  int size;

  LVAL arg_handle, arg_size;

  arg_handle = xlgaport();
  handle = getfile(arg_handle);

  if (moreargs())
    {
      arg_size = xlgafixnum();
      size = getfixnum(arg_size);
      xllastarg();
    }
  else
    size = BUFSIZ;

  fflush(handle);
  if (setvbuf(handle, NULL, _IOFBF, size) < 0)
    xlsockerror(cfn_name, arg_handle);

  return TRUEVALUE;
}

/* (port-line-buffered port) */
LVAL port_line_buffered()
{
  static char *cfn_name = "port-line-buffered";
  FILE *handle;

  LVAL arg_handle;

  arg_handle = xlgaport();
  handle = getfile(arg_handle);

  xllastarg();
  
  fflush(handle);
  if (setvbuf(handle, NULL, _IOLBF, 0) < 0)
    xlsockerror(cfn_name, arg_handle);

  return TRUEVALUE;
}
#else
/* (port-unbuffered port) */
LVAL port_unbuffered()
{
  static char *cfn_name = "port-unbuffered";
  FILE *handle;

  LVAL arg_handle;
  
  arg_handle = xlgaport();
  handle = getfile(arg_handle);

  xllastarg();

  /* do nothing */

  return TRUEVALUE;
}

/* (port-block-buffered port . bufsize) */
LVAL port_block_buffered()
{
  static char *cfn_name = "port-block-buffered";
  FILE *handle;
  int size;

  LVAL arg_handle, arg_size;

  arg_handle = xlgaport();
  handle = getfile(arg_handle);

  if (moreargs())
    {
      arg_size = xlgafixnum();
      size = getfixnum(arg_size);
      xllastarg();
    }
  else
    size = BUFSIZ;

  /* do nothing */

  return TRUEVALUE;
}

/* (port-line-buffered port) */
LVAL port_line_buffered()
{
  static char *cfn_name = "port-line-buffered";
  FILE *handle;

  LVAL arg_handle;

  arg_handle = xlgaport();
  handle = getfile(arg_handle);

  xllastarg();
  
  /* do nothing */

  return TRUEVALUE;
}
#endif /* no more buffering */

/* select functions */

static fd_set readfds, writefds;

/* (socket-fd-zero-read) */
LVAL socket_fd_zero_read()
{
  static char *cfn_name = "socket-fd-zero-read";

  xllastarg();
  FD_ZERO(&readfds);
  return TRUEVALUE;
}

/* (socket-fd-set-read fd) */
LVAL socket_fd_set_read()
{
  static char *cfn_name = "socket-fd-set-read";
  int fd;

  LVAL arg_fd;

  arg_fd = xlgafixnum();
  fd = (int)getfixnum(arg_fd);

  xllastarg();

  FD_SET(fd, &readfds);
  return TRUEVALUE;
}

/* (socket-fd-isset-read fd) */
LVAL socket_fd_isset_read()
{
  static char *cfn_name = "socket-fd-isset-read";
  int fd;

  LVAL arg_fd;

  arg_fd = xlgafixnum();
  fd = (int)getfixnum(arg_fd);

  xllastarg();

  return FD_ISSET(fd, &readfds) ? TRUEVALUE : FALSEVALUE;
}

/* (socket-select-read timeout) */
LVAL socket_select_read()
{
  static char *cfn_name = "socket-select-read";
  int time;
  struct timeval timeout;

  LVAL arg_time;

  arg_time = xlgafixnum();
  time = (int)getfixnum(arg_time);

  xllastarg();

  if (time >= 0) 
    {
      timeout.tv_sec = time / 1000;
      timeout.tv_usec = (time - (timeout.tv_sec * 1000)) * 1000;
    }

  switch (select(FD_SETSIZE, &readfds, NULL, NULL, (time<0)?NULL:&timeout)) 
    {
    case -1:
      xlsockerror(cfn_name, arg_time);
      break;
    case 0:  return FALSEVALUE;
    default: return TRUEVALUE;
    }

  return NIL;			/* not reached */
}

/* (socket-fd-zero-write) */
LVAL socket_fd_zero_write()
{
  static char *cfn_name = "socket-fd-zero-write";

  xllastarg();
  FD_ZERO(&writefds);
  return TRUEVALUE;
}

/* (socket-fd-set-write fd) */
LVAL socket_fd_set_write()
{
  static char *cfn_name = "socket-fd-set-write";
  int fd;

  LVAL arg_fd;

  arg_fd = xlgafixnum();
  fd = (int)getfixnum(arg_fd);

  xllastarg();

  FD_SET(fd, &writefds);
  return TRUEVALUE;
}

/* (socket-fd-isset-write fd) */
LVAL socket_fd_isset_write()
{
  static char *cfn_name = "socket-fd-isset-write";
  int fd;

  LVAL arg_fd;

  arg_fd = xlgafixnum();
  fd = (int)getfixnum(arg_fd);

  xllastarg();

  return FD_ISSET(fd, &writefds) ? TRUEVALUE : FALSEVALUE;
}

/* (socket-select-write timeout) */
LVAL socket_select_write()
{
  static char *cfn_name = "socket-select-write";
  int time;
  struct timeval timeout;

  LVAL arg_time;

  arg_time = xlgafixnum();
  time = (int)getfixnum(arg_time);

  xllastarg();

  if (time >= 0) 
    {
      timeout.tv_sec = time / 1000;
      timeout.tv_usec = (time - (timeout.tv_sec * 1000)) * 1000;
    }

  switch (select(FD_SETSIZE, NULL, &writefds, NULL, (time<0)?NULL:&timeout)) 
    {
    case -1:
      xlsockerror(cfn_name, arg_time);
      break;
    case 0:  return FALSEVALUE;
    default: return TRUEVALUE;
    }

  return NIL;			/* not reached */
}

/* utility wrappers for XDR I/O */

bool_t xdr_send_int(handle, i)
FILE *handle; int i;
{
 XDR xdrs;
 bool_t retval;

 /* following may return 'bad fseek' but seems to fix interleaving
    reads and writes */
 fseek(handle, 0L, SEEK_CUR);
 errno = 0;
 xdrstdio_create(&xdrs, handle, XDR_ENCODE);
 retval = xdr_int(&xdrs, &i);
 fflush(handle);
 xdr_destroy(&xdrs);
 return retval;
}

bool_t xdr_recv_int(handle, ip)
FILE *handle; int *ip;
{
 XDR xdrs;
 bool_t retval;

 fseek(handle, 0L, SEEK_CUR);
 errno = 0;
 xdrstdio_create(&xdrs, handle, XDR_DECODE);
 HANDLE(retval = xdr_int(&xdrs, ip));
 fflush(handle);
 xdr_destroy(&xdrs);
 return retval;
}

bool_t xdr_send_double(handle, i)
FILE *handle; double i;
{
 XDR xdrs;
 bool_t retval;

 fseek(handle, 0L, SEEK_CUR);
 errno = 0;
 xdrstdio_create(&xdrs, handle, XDR_ENCODE);
 retval = xdr_double(&xdrs, &i);
 fflush(handle);
 xdr_destroy(&xdrs);
 return retval;
}

bool_t xdr_recv_double(handle, ip)
FILE *handle; double *ip;
{
 XDR xdrs;
 bool_t retval;

 fseek(handle, 0L, SEEK_CUR);
 errno = 0;
 xdrstdio_create(&xdrs, handle, XDR_DECODE);
 HANDLE(retval = xdr_double(&xdrs, ip));
 fflush(handle);
 xdr_destroy(&xdrs);
 return retval;
}

bool_t xdr_send_string(handle, i, len)
FILE *handle; char *i; int len;
{
 XDR xdrs;
 bool_t retval;

 fseek(handle, 0L, SEEK_CUR);
 errno = 0;
 xdrstdio_create(&xdrs, handle, XDR_ENCODE);
 retval = xdr_string(&xdrs, &i, len);
 fflush(handle);
 xdr_destroy(&xdrs);
 return retval;
}

bool_t xdr_recv_string(handle, ip, len)
FILE *handle; char **ip; int len;
{
 XDR xdrs;
 bool_t retval;

 fseek(handle, 0L, SEEK_CUR);
 errno = 0;
 xdrstdio_create(&xdrs, handle, XDR_DECODE);
 HANDLE(retval = xdr_string(&xdrs, ip, len));
 fflush(handle);
 xdr_destroy(&xdrs);
 return retval;
}

/* entry routines for XDR I/O to and from ports */

/* (port-xdr-send-int port val) */
LVAL port_xdr_send_int()
{
  static char *cfn_name = "port-xdr-send-int";
  FILE *handle; int i;

  LVAL arg_handle, arg_i;

  arg_handle = xlgaoport();
  handle = getfile(arg_handle);

  arg_i = xlgafixnum();
  i = (int)getfixnum(arg_i);

  xllastarg();

  if (xdr_send_int(handle, i) == FALSE)
    xlsockerror(cfn_name, cons(arg_handle, arg_i));
  
  return TRUEVALUE;
} 
	
/* (port-xdr-recv-int port) */
LVAL port_xdr_recv_int()
{
  static char *cfn_name = "port-xdr-recv-int";
  FILE *handle;
  int i;

  LVAL arg_handle;

  arg_handle = xlgaiport();
  handle = getfile(arg_handle);

  xllastarg();

  if (xdr_recv_int(handle, &i) == FALSE)
    xlsockerror(cfn_name, arg_handle);
	
  return (cvfixnum((FIXTYPE)i));
} 
	
/* (port-xdr-send-float port val) */
LVAL port_xdr_send_float()
{
  static char *cfn_name = "port-xdr-send-float";
  FILE *handle; double i;

  LVAL arg_handle, arg_i;

  arg_handle = xlgaoport();
  handle = getfile(arg_handle);

  arg_i = xlganumber();

  if (fixp(arg_i))
    i = (double)getfixnum(arg_i);
  else
    i = (double)getflonum(arg_i);

  xllastarg();

  if (xdr_send_double(handle, i) == FALSE)
    xlsockerror(cfn_name, cons(arg_handle, arg_i));
 
  return TRUEVALUE;
}
 
/* (port-xdr-recv-float port) */
LVAL port_xdr_recv_float()
{
  static char *cfn_name = "port-xdr-recv-float";
  FILE *handle;
  double i;
 
  LVAL arg_handle;

  arg_handle = xlgaiport();
  handle = getfile(arg_handle);

  xllastarg();

  if (xdr_recv_double(handle, &i) == FALSE)
    xlsockerror(cfn_name, arg_handle);
 
  return (cvflonum((FLOTYPE)i));
}
   
/* (port-xdr-send-string port val) */
LVAL port_xdr_send_string()
{
  static char *cfn_name = "port-xdr-send-string";
  FILE *handle; char *string;
  int i;

  LVAL arg_handle, arg_string;

  arg_handle = xlgaoport();
  handle = getfile(arg_handle);

  arg_string = xlgastring();
  string = (char*)getstring(arg_string);

  xllastarg();

  i = getslength(arg_string);

  if (xdr_send_int(handle, i) == FALSE)
    xlsockerror(cfn_name, cons(arg_handle, arg_string));

  if (xdr_send_string(handle, string, i) == FALSE)
    xlsockerror(cfn_name, cons(arg_handle, arg_string));
	
  return TRUEVALUE;
} 

/* (port-xdr-recv-string port) */
LVAL port_xdr_recv_string()
{
  static char *cfn_name = "port-xdr-recv-string";
  FILE *handle;
  int i;
  char *buffer;
  LVAL temp;

  LVAL arg_handle;
  
  arg_handle = xlgaiport();
  handle = getfile(arg_handle);

  xllastarg();

  if (xdr_recv_int(handle, &i) == FALSE)
    xlsockerror(cfn_name, arg_handle);

  if ((buffer = (char*) malloc(i * sizeof(char))) == NULL)
    xlcerror("port-xdr-recv-string: failed to allocate memory", arg_handle,
	     s_socket_error);

  if (xdr_recv_string(handle, &buffer, i) == FALSE)
    xlsockerror(cfn_name, arg_handle);

  buffer[i-1] = '\0';
	
  temp = cvstring2(buffer, i-1);
  free(buffer);
  return temp;
}

#else
/* some linkers complain of empty files */
static void no_sockets() {}
#endif
