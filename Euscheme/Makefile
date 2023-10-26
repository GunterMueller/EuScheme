# -DSIGNAL when you don't have sigset
# -DGTOD1 when gettimeofday has one arg
# -DRESTORE_SIGNAL when signals are not automatically reset
# use -DSOCK to include socket code, but beware: it is possibly buggy

# tried on sunos, aix, solaris, linux, ultrix, stardent, irix.

it:
	@$(MAKE) `uname`

SunOS:
	@$(MAKE) SunOS-`uname -r | awk -F. -e '{print $$1}'`

# cc is OK
SunOS-4:
	$(MAKE) all "CFLAGS = -DUNIX -DSIGNAL" "LIBS = -lm" "CC = gcc"

# Solaris
#-lsocket -lnsl for sockets
SunOS-5:
	$(MAKE) all "CFLAGS = -DUNIX -g" "LDFLAGS = -g" "LIBS = -lm" "CC = cc"

AIX:
	$(MAKE) all "CFLAGS = -DUNIX -D_ALL_SOURCE -DSOCK" "LIBS = -lm" "CC = c89"

# -DOLD_LINUX for ancient versions of Linux
Linux:
	$(MAKE) all "CFLAGS = -m32 -DUNIX -DSIGNAL -DSTRINGH -Wall" \
                "LIBS = -lm" "CC = cc"

# Stardent
TitanOS:
	$(MAKE) all "CFLAGS = -DUNIX" "LIBS = -lm" "CC = cc"

# IRIX 4.0.5 and 5.3
# 4.0.5 requires -lsun for sockets
IRIX:
	$(MAKE) all "CFLAGS = -DUNIX" "LIBS = -lm" "CC = cc"

# Ultrix 4.3
Ultrix:
	$(MAKE) all "CFLAGS = -DUNIX -DSIGNAL" "LIBS = -lm" "CC = gcc"

# where to look for things
# look first in env variable EU_MODULE_PATH, then builtin
# env variable is a colon separated list of directory names
MODULE_SEARCH_PATH = ".", "Tests", "Modules"

# look first in env variable EU_IMAGE_PATH, then builtin
IMAGE_SEARCH_PATH = "."

# if you want your image to have a different name
IMAGE = image.wks

# where to install
DESTBIN = /usr/local/contrib/bin.sun4
DESTLIB = /usr/local/contrib/share/Euscheme

# generally no need to fiddle with stuff below this line
#---------------------------------------------------------------------------

# ensure which shell we are using
SHELL = /bin/sh

SRCS = msstuff.c xsdmem.c xsfun2.c xsint.c xsobj.c xssym.c \
xscheme.c xsftab.c xsimage.c xsio.c xsprint.c \
xscom.c xsfun1.c xsinit.c xsmath.c xsread.c \
osdefs.h osptrs.h xsbcode.h xscheme.h xsproto.h \
xsmodule.c xsobj.h xstable.c xsocket.c xsbanner.h

LISP = Boot/*.em

OBJS = msstuff.o xsdmem.o xsfun2.o xsint.o xsobj.o xssym.o \
xscheme.o xsftab.o xsimage.o xsio.o xsprint.o \
xscom.o xsfun1.o xsinit.o xsmath.o xsread.o \
xsmodule.o xstable.o xsocket.o

OTHERS = Makefile saveimage README Modules/*.em euscheme.man euscheme.1 \
eunotes.html

DOS = makefile.dos saveimg.dos saveimg.bat readme.dos
RISCOS = makefile-riscos via

MISC = Tests Misc Machines TODO

TARFILES = $(SRCS) $(LISP) $(OTHERS) $(MISC)
DISTFILES = $(SRCS) $(LISP) $(OTHERS) $(DOS) $(RISCOS)

all: $(IMAGE)

$(IMAGE): euscheme $(LISP) saveimage
	EU_MODULE_PATH=Boot ./euscheme -n < saveimage
	mv image.wks image.tmp; mv image.tmp $(IMAGE)

euscheme: $(OBJS)
	$(CC) -o euscheme $(LDFLAGS) $(OBJS) $(LIBS)

xscheme.o: xscheme.c xsbanner.h Makefile
	$(CC) $(CFLAGS) -DIMAGE='"$(IMAGE)"' -c xscheme.c

xscom.o: xscom.c Makefile
	$(CC) $(CFLAGS) -DMODULE_SEARCH_PATH='$(MODULE_SEARCH_PATH)' -c xscom.c

xsimage.o: xsimage.c xsbanner.h Makefile
	$(CC) $(CFLAGS) -DIMAGE_SEARCH_PATH='$(IMAGE_SEARCH_PATH)' -c xsimage.c

euscheme.1: euscheme.man
	nroff -man euscheme.man > euscheme.1

dist: euscheme.tgz

version: xsbanner.h
	awk '/define BANNER/ {print $$6}' < xsbanner.h | sed 's/[."]//g' > version

euscheme.tgz: $(DISTFILES) version
	sed "s/^VERSION=[0-9]*/VERSION=`cat version`/" < makefile.dos > xx.dos
	mv xx.dos makefile.dos
	tar cvf - $(DISTFILES) | gzip > euscheme.tgz

unpack:
	mv Makefile mk
	gunzip < euscheme.tgz | tar xvf -
	mv Makefile Makefile.dist
	mv mk Makefile

update:
	getdist
	$(MAKE) unpack
	$(MAKE)

TAGS: $(SRCS) $(LISP)
	etags $(SRCS) $(LISP)

tags: TAGS

OTHERDIR = ../sunos5
links:
	for i in $(SRCS) saveimage Boot Modules; do ln -s $(OTHERDIR)/$$i . ; done
	cp $(OTHERDIR)/Makefile .

install: install-bin install-img install-mod
install-bin: euscheme
	cp euscheme $(DESTBIN)
install-img: $(IMAGE)
	cp $(IMAGE) $(DESTLIB)/Image
install-mod: Modules/*.em
	cp Modules/*.em $(DESTLIB)/Modules

ftp: euscheme.tgz version
	cp euscheme.tgz ftp/euscheme`cat version`.tgz

clean:
	rm -f *.o *~ euscheme image.wks core TAGS Tests/*~ Modules/*~ \
Misc/*~ Boot/*.em~

clean_sock:
	rm -f msstuff.o xsftab.o xsinit.o xsocket.o
